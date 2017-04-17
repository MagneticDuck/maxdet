{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Det where

import qualified Data.List as L
import Data.List ((\\))
import Data.Maybe
import Data.Ord (comparing)
import Data.List.Split

tagWith :: (a -> b) -> a -> (a, b)
tagWith f x = (x, f x)

-- An enumeration is implemented as a lazy list.
newtype Enumeration a = Enumeration { fromEnumeration :: [a] }
  deriving (Eq, Functor, Applicative, Monad, Monoid, Foldable)
instance (Show a) => Show (Enumeration a) where
  show (Enumeration xs) = L.intercalate ", " $ map show xs

combinations :: Int -> [a] -> Enumeration [a]
combinations = (Enumeration .) . choose
  where choose 0 _  = [[]]
        choose _ [] = []
        choose k' (x:xs) = map (x:) (choose (k' - 1) xs) ++ choose k' xs

permutations :: [a] -> Enumeration [a]
permutations = Enumeration . L.permutations 

type Mat a = [[a]]

-- Enumerate n \times n matrices up to similarity under row / column permutations 
-- and transposition.
matrixIncidences :: Int -> Enumeration (Mat Int)
matrixIncidences n = do
  hook <- combinations (2*n - 2) [2..n^2]
  (col, line) <- tagWith (hook \\) <$> combinations (n - 1) (drop 1 hook)
  body <- chunksOf (n - 1) <$> permutations ([2..n^2] \\ hook)
  return $ (1:line) : zipWith (:) col body

-- Compute a 3 \times 3 determinant via cofactor expansion.
det3 :: (Num a) => Mat a -> a
det3 [[a, b, c], [d, e, f], [g, h, i]] =
  a * e * i - g * e * c + b * f * g + d * h * c - b * d * i - f * h * a -- maybe LU?

-- Compute an n \times n determinant via LU decomposition.
det :: (Num a) => Mat a -> a
det = undefined

-- Determine all the optimal configurations for a set of 9. Matrix configurations
-- are represented with integer codes. View them with `recallConfig`.
solve3 :: (Num a, Ord a) => [a] -> (a, [Int])
solve3 vals = 
  (win, map fst $ filter ((== win) . snd) space)
  where obj = abs . det3 . chunksOf 3 . map ((vals !!) . subtract 1) . concat
        win = maximum . map snd $ space
        space = map (\(i, config) -> (i, obj config)) . zip [1..] . fromEnumeration $ 
                  matrixIncidences 3
    
-- Determine the 3 \times 3 matrix referred to by an integer code.
recallConfig :: Int -> Mat Int
recallConfig = (fromEnumeration (matrixIncidences 3) !!) . (subtract 1)

-- There is a single maximum configuration for the values {1 .. 9}:
--
-- *Det> solve3 [1..9]
-- (412,[4192])
-- *Det> recallConfig 4192
-- [[1,4,8],[5,9,3],[7,2,6]]
--
-- The story for {1 + i .. 9 + i} in general is not so simple:
--
-- *Det> map snd $ solve3 <$> map (\i -> map (+ i) [1..9]) [1..15]
-- [[4192],[4192],[4192],[4192],[4192],[4192],[4192,4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432]]
--

swapElements :: Int -> Int -> [a] -> [a]
swapElements x y xs = concat
  [ take (x - 1) xs, [xs !! (y - 1)]
  , take (y - x - 1) $ drop x xs, [xs !! (x - 1)]
  , drop y xs ]

-- Swap single pairs of elements of an n \times n matrix in the most beneficial ways.
greedyImprove :: (Ord a, Num a) => Int -> Mat a -> [Mat a]
greedyImprove n mat = undefined
  where
    mats = tagWith det <$> choices
    choices = fromEnumeration $ 
      (\[x, y] -> chunksOf n . swapElements x y $ concat mat) <$> 
        combinations 2 [1..n^2]

