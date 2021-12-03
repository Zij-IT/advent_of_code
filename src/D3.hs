module D3
  ( format
  , part1
  , part2
  ) where

import Data.Bits (complement, (.&.), shiftR)
import Data.Char (digitToInt)
import Data.List (transpose)
import Control.Monad (liftM2)

format :: String -> [[Int]]
format = map (map digitToInt) . lines

tupleCount :: [[Int]] -> [(Int, Int)]
tupleCount xs = map (foldr f' (0, 0)) $ transpose xs
  where
    f' :: Int -> (Int, Int) -> (Int, Int)
    f' x (a, b) = (a + (.&.) x 1, b + 1 - x)

mostCommon :: [[Int]] -> [Int]
mostCommon xs = map f' $ tupleCount xs
  where
    f' :: (Int, Int) -> Int
    f' (a, b) = if a >= b then 1 else 0

listToDecimal :: [Int] -> Int
listToDecimal (x:[]) = x
listToDecimal (x:xs) = 2 ^ (length xs) * x + listToDecimal xs

nextPowerOf2 :: Int -> Int
nextPowerOf2 0 = 1
nextPowerOf2 x = 2 * nextPowerOf2 (shiftR x 1)

complement' :: Int -> Int
complement' = liftM2 (.&.) complement (subtract 1 . nextPowerOf2)

getRating :: [[Int]] -> (Int -> Int -> Bool) -> Int -> [Int]
getRating  [xs] _  _  = xs
getRating   xs  f idx = getRating (filter f' xs) f (idx + 1)
  where
    f' :: [Int] -> Bool
    f' elem = f (mostCommon xs !! idx) (elem !! idx)

part1 :: [[Int]] -> Int
part1 xs = gamma * complement' gamma
  where
    gamma :: Int
    gamma = listToDecimal $ mostCommon xs

part2 :: [[Int]] -> Int
part2 xs = oxy * co2
  where
    subRating f = listToDecimal $ getRating xs f 0
    oxy = subRating (==)
    co2 = subRating (/=)

