module D1
  ( format
  , part1
  , part2
  ) where

type Input = [Int]

format :: String -> Input
format = map read . lines

pairs :: Int -> [Int] -> [(Int, Int)]
pairs n xs = zip xs (drop n xs)

solveN :: Int -> Input -> Int
solveN n = length . filter (uncurry (<)) . pairs n

part1 :: Input -> Int
part1 = solveN 1

part2 :: Input -> Int
part2 = solveN 3
