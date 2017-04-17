{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Det where

import qualified Data.List as L
import Data.List ((\\))
import Data.Maybe
import Data.Ord (comparing)
import Data.List.Split

-- We are not interested in finding the nth element of an enumeration; we are always
-- performing an exhaustive linear search.

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

matrixIncidences :: Int -> Enumeration (Mat Int)
matrixIncidences n = do
  hook <- combinations (2*n - 2) [2..n^2]
  (col, line) <- (\x -> (x, hook \\ x)) <$> combinations (n - 1) (drop 1 hook)
  body <- chunksOf (n - 1) <$> permutations ([2..n^2] \\ hook)
  return $ (1:line) : zipWith (:) col body

det3 :: (Num a) => Mat a -> a
det3 [[a, b, c], [d, e, f], [g, h, i]] =
  a * e * i - g * e * c + b * f * g + d * h * c - b * d * i - f * h * a -- maybe LU?

solve3 :: (Num a, Ord a) => [a] -> (a, [Int])
solve3 vals = 
  (win, map fst $ filter ((== win) . snd) space)
  where obj = abs . det3 . chunksOf 3 . map ((vals !!) . subtract 1) . concat
        win = maximum . map snd $ space
        space = map (\(i, config) -> (i, obj config)) . zip [1..] . fromEnumeration $ 
                  matrixIncidences 3
    
recallConfig :: Int -> Mat Int
recallConfig = (fromEnumeration (matrixIncidences 3) !!) . (subtract 1)

-- Example usage:
-- *Det> solve3 [1..9]
-- (412,[4192])
-- *Det> recallConfig 4192
-- [[1,4,8],[5,9,3],[7,2,6]]
-- *Det> map snd $ solve3 <$> map (\i -> map (+ i) [1..9]) [1..15]
-- [[4192],[4192],[4192],[4192],[4192],[4192],[4192,4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432],[4285,4432]]

