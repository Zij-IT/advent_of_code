module D7 ( format
          , part1
          , part2
          ) where

import Data.List (sort)
import Control.Monad (ap)

type Input = [Int]

format :: String -> Input
format s = read $ concat ["[", s, "]"]

median :: [Int] -> Int
median = ap (!!) (flip quot 2 . length)


part1 :: Input -> Int
part1 xs = sum $ map (abs . (`subtract` medium)) xs
  where
    medium :: Int
    medium = median $ sort xs

part2 :: Input -> Int
part2 xs = sum $ map cost xs
  where
    mean :: Int
    mean = quot (sum xs) (length xs)

    cost :: Int -> Int
    cost x = sum [1 .. (abs $ x - mean)]
