module D3
  ( format
  , part1
  , part2
  ) where

import Data.Bits (complement, (.&.), shiftL)
import Data.Char (digitToInt)
import Data.List (transpose)
import Control.Monad (liftM2)

type Input = [[Int]]

format :: String -> Input
format = map (map digitToInt) . lines

tupleCount :: Input -> [(Int, Int)]
tupleCount xs = map (foldr f' (0, 0)) $ transpose xs
  where
    f' :: Int -> (Int, Int) -> (Int, Int)
    f' x (a, b) = (a + x .&. 1, b + 1 - x)

mostCommon :: Input -> [Int]
mostCommon xs = map f' $ tupleCount xs
  where
    f' :: (Int, Int) -> Int
    f' (a, b) = if a >= b then 1 else 0

listToDecimal :: [Int] -> Int
listToDecimal = foldl (\sum bit -> 2 * sum + bit) 0

part1 :: Input -> Int
part1 xs = gamma * epsilon
  where
    gamma   = listToDecimal $ mostCommon xs
    epsilon = listToDecimal $ map (1-) $ mostCommon xs

part2 :: Input -> Int
part2 xs = oxy * co2
  where
    subRating f = listToDecimal $ getRating xs f 0
    oxy = subRating (==)
    co2 = subRating (/=)

    -- On good input, this won't get a bad index access
    getRating :: Input -> (Int -> Int -> Bool) -> Int -> [Int]
    getRating  [xs] _  _  = xs
    getRating   xs  f idx = getRating (filter f' xs) f (idx + 1)
      where
        f' :: [Int] -> Bool
        f' elem = f (mostCommon xs !! idx) (elem !! idx)

