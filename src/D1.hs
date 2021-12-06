module D1
  ( format
  , part1
  , part2
  ) where

format :: String -> [Int]
format = map read . lines

windows :: [a] -> [[a]]
windows xs = zipWith (\x y -> [x, y]) xs (drop 1 xs)

tripletSum :: [Int] -> [Int]
tripletSum xs = zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)

part1 :: [Int] -> Int
part1 = length . filter (\[x, y] -> y > x) . windows

part2 :: [Int] -> Int
part2 = part1 . tripletSum
