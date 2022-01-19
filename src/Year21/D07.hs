module Year21.D07 ( format
          , part1
          , part2
          ) where

import Data.List (sort)
import Control.Monad (ap)

type Input = [Int]

format :: String -> Input
format s = read $ concat ["[", s, "]"]

median :: [Int] -> Int
median xs = xs !! quot (length xs) 2

part1 :: Input -> Int
part1 xs = foldr ((+) . abs . (`subtract` medium)) 0 xs
  where
    medium :: Int
    medium = median $ sort xs

part2 :: Input -> Int
part2 xs = foldr ((+) . cost) 0 xs
  where
    mean :: Int
    mean = quot (sum xs) (length xs)

    cost :: Int -> Int
    cost x = quot (diff * succ diff) 2
      where
        diff :: Int
        diff = abs $ x - mean
