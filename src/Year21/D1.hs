module Year21.D1
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

-- Explanation:
-- Part1: Solved simply by comparing one element with the next one in the list.
--
-- Part2: The question asks to find all instances in a list where
--        SUM [a, b, c] < SUM [b, c, d] for any consecutive a, b, c, d
--        in the list. This can be solved very similarly to Part 1.
--        For any [a, b, c, d | a, b, c, d ELEM Z] it is true that:
--        a + b + c < b + c + d <=> a < d
--        Therefore we can solve this by comparing a and d, skipping the addition!

part1 :: Input -> Int
part1 = solveN 1

part2 :: Input -> Int
part2 = solveN 3
