module Year21.D6 ( format
          , part1
          , part2
          ) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Age = Int
type Count = Int
type Input = Map.Map Age Count

format :: String -> Input
format s = Map.fromListWith (+) $ zip (read $ concat ["[", s, "]"]) (repeat 1)

step :: Input -> Input
step = Map.fromListWith (+) . concatMap age . Map.toList

age :: (Age, Count) -> [(Age, Count)]
age (fishAge, count)
 | fishAge == 0 = [(6, count), (8, count)]
 | otherwise = [(fishAge - 1, count)]

part1 :: Input -> Int
part1 = sum . Map.elems . (!! 80) . iterate step

part2 :: Input -> Int
part2 = sum . Map.elems . (!! 256) . iterate step

