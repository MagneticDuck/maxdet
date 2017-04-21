{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Det where

import qualified Data.List as L
import Data.List ((\\))
import Data.Maybe
import Control.Monad
import Data.Ord (comparing)
import Data.List.Split

tagWith :: (a -> b) -> a -> (a, b)
tagWith f x = (x, f x)
 
toReal :: Rational -> Float
toReal = realToFrac

-- ** 3x3 matrix utilities. **
type Mat a = [[a]]

det3 :: (Num a) => Mat a -> a
det3 [[a, b, c], [d, e, f], [g, h, i]] =
  a * e * i - g * e * c + b * f * g + d * h * c - b * d * i - f * h * a

cofactorSum :: (Num a) => Mat a -> a
cofactorSum [[a, b, c], [d, e, f], [g, h, i]] =
  -b*d + c*d + a*e - c*e - a*f + b*f + b*g - c*g - e*g + f*g - a*h + c*h + d*h - f*h + a*i - b*i - d*i + e*i

-- ** Enumerations, ultimately of our search space. **
type Enumeration a = [a]

combinations :: Int -> [a] -> Enumeration [a]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k - 1) xs) ++ combinations k xs

permutations :: [a] -> Enumeration [a]
permutations = L.permutations 

matrixIncidences :: Int -> Enumeration (Mat Int)
matrixIncidences n = do
  hook <- combinations (2*n - 2) [2..n^2]
  (col, line) <- tagWith (hook \\) <$> combinations (n - 1) (drop 1 hook)
  body <- chunksOf (n - 1) <$> permutations ([2..n^2] \\ hook)
  return $ (1:line) : zipWith (:) col body

-- ** Utilities for {piecewise} linear functions in 1 dimension. **

data Linear = Linear { linearM :: Rational, linearB :: Rational } deriving (Eq) 
type Piecewise = [(Rational, Rational)] -- Linear interpolation of the points.
data Range = Range Rational Rational deriving (Show, Eq)

inRange :: Range -> Rational -> Bool
inRange (Range low high) rat = rat <= high && rat >= low

interpolate :: Piecewise -> [(Range, Linear)]
interpolate pcs = zipWith makePair pcs (drop 1 pcs)
  where makePair (x1, y1) (x2, y2) = let m = (y2 - y1) / (x2 - x1) in 
          (Range x1 x2, Linear m (y1 - m * x1))

instance Show Linear where show (Linear m b) = "(" ++ show m ++ ") x + (" ++ show b ++ ")"

-- Right-compose with a shift by some value.
shiftLinear :: Rational -> Linear -> Linear
shiftLinear v (Linear m b) = (Linear m (b + v * m))

evalLinear :: Linear -> Rational -> Rational
evalLinear (Linear m b) x = m * x + b

evalPiecewise :: Piecewise -> Rational -> Maybe Rational
evalPiecewise pcs x = flip evalLinear x . snd <$> 
  (L.find (flip inRange x . fst) $ interpolate pcs)

linearToPiecewise :: Rational -> Rational -> Linear -> Piecewise
linearToPiecewise x1 x2 l = [(x1, (evalLinear l x1)), (x2, (evalLinear l x2))]
 
intersectLinear :: Linear -> Linear -> Maybe Rational
intersectLinear (Linear m1 b1) (Linear m2 b2) = 
  if v == 0 then Nothing else Just $ (b2 - b1) / v where v = m1 - m2

intersectPiecewise :: Piecewise -> Linear -> [Rational]
intersectPiecewise xs l = 
  concatMap (maybeToList . snd) . filter (\(range, inter) -> 
    fromMaybe False (inRange range <$> inter)) $ 
    (\(range, seg) -> (range, intersectLinear seg l)) <$> interpolate xs

multLinear :: Rational -> Linear -> Linear
multLinear v (Linear m b) = Linear (v * m) (v * b)

reflectLinear :: Linear -> Linear
reflectLinear (Linear m b) = Linear (-m) b

maxPiecewise :: Rational -> Rational -> [Linear] -> Piecewise
maxPiecewise _ _ [] = []
maxPiecewise start end xs =
  case nextInters of
    [] ->  [(start, evalLinear best start), (end, evalLinear best end)]
    _ -> let next = snd $ L.minimumBy (comparing snd) nextInters in
      if (next > end) then [(start, bestValue), (end, evalLinear best end)]
      else (start, bestValue) : maxPiecewise next end (fst <$> nextInters)
  where (best, bestValue) = L.maximumBy (comparing snd) $ 
          tagWith (flip evalLinear start) <$> (L.sortBy (comparing linearM) xs)
        rest = xs \\ [best]
        nextInters =
          filter ((> start) . snd) $ 
            concatMap (maybeToList . uncurry (fmap . (,)) . 
              tagWith (intersectLinear best)) rest

-- Linear function that relates \lambda with det(M + \lambda J) where M is {-4, -3 .. 4} 
-- distributed according to the given configuration.
recallArithDet :: Int -> Linear
recallArithDet cfg = shiftLinear (-5) $
  Linear (realToFrac $ cofactorSum mat) (realToFrac $ det3 mat)
  where mat = recallConfig cfg

-- Solving matrices, finding optimal configurations for arithmetic sequences.
solve3 :: (Num a, Ord a) => [a] -> (a, [Int])
solve3 vals = 
  (win, map fst $ filter ((== win) . snd) space)
  where obj = abs . det3 . chunksOf 3 . map ((vals !!) . subtract 1) . concat
        win = maximum . map snd $ space
        space = map (\(i, config) -> (i, obj config)) . zip [1..] $ 
                  matrixIncidences 3
    
recallConfig :: Int -> Mat Int
recallConfig = (matrixIncidences 3 !!) . (subtract 1)

recallArithConfig :: Int -> Rational -> Mat Rational
recallArithConfig i x = (map . map) ((+x) . subtract 1 . realToFrac) $ recallConfig i

-- The various linear functions det(M + \lambda J) generated as M takes on all 
-- configurations of the values [-4..+4].
allDets :: [Linear]
allDets = unsigned ++ (multLinear (-1) <$> unsigned)
  where unsigned = recallArithDet <$> [1..5040] -- TODO: fast

maxDets :: Piecewise
maxDets = maxPiecewise 0 200 allDets

data Report = Report (Maybe Rational) Rational [Int] 

instance Show Report where
  show (Report hyp calc mats) = unlines
    [ "Interpolated solution: " ++ show hyp
    , "Real solution:         " ++ show calc
    , "Configurations:        " ++ show mats ]

report :: Rational -> Report
report x = Report (evalPiecewise maxDets x) max mats
  where (max, mats) = solve3 [x-4, x-3 .. x+4]

