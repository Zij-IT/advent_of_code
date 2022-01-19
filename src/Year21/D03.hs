module Year21.D03
  ( format
  , part1
  , part2
  ) where

import Data.Bits ((.&.))
import Data.Char (digitToInt)
import Data.List (transpose)

type Input = [[Int]]

format :: String -> Input
format = map (map digitToInt) . lines

columnCount :: Input -> [(Int, Int)]
columnCount = map (foldr f' (0, 0)) . transpose
  where
    f' :: Int -> (Int, Int) -> (Int, Int)
    f' x (a, b) = (a + x .&. 1, b + 1 - x)

mostCommon :: Input -> [Int]
mostCommon = map f' . columnCount
  where
    f' :: (Int, Int) -> Int
    f' (a, b) = if a >= b then 1 else 0

listToDecimal :: [Int] -> Int
listToDecimal = foldl (\sum bit -> 2 * sum + bit) 0

-- Explanation:
-- Part 1: This one just involves finding the most bit for each column, and multiplying by the
--         inverse. This was super easy thanks to transpose, and then just folding it.
--         The fold function can be understood as:
--         f' x (a, b) = (a + if x == 1 then 1 else 0, b + if x == 0 then 1 else 0).
--         I wanted to do it branchless, so I used the bitwise &, and (1 - 0)
-- Part 2: The base function used to get the rating is almost entirely the same.
--         The first function wants to check if the bits are equal to the most common rating
--         and the second wants to check if they are equal to the inverse. As we are dealing
--         only with 1 and 0, the second can be said to be checking if the bits are NOT equal
--         to the most common at position X.

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

