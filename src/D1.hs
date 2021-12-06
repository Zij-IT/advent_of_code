module D1
  ( format
  , part1
  , part2
  ) where

format :: String -> [Int]
format = map read . lines

pairs :: Int -> [Int] -> [(Int, Int)]
pairs n xs = zip xs (drop n xs)

solveN :: Int -> [Int] -> Int
solveN n = length . filter (uncurry (<)) . pairs n

part1 :: [Int] -> Int
part1 = solveN 1

part2 :: [Int] -> Int
part2 = solveN 3
