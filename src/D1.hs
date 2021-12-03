module D1
  ( format
  , part1
  , part2
  ) where

format :: String -> [Int]
format = map read . lines

windows :: [a] -> [[a]]
windows (x:y:[]) = [[x, y]]
windows (x:y:xs) = [[x,y]] ++ windows (y:xs)

tripletSum :: [Int] -> [Int]
tripletSum (x:y:z:[]) = [x + y + z]
tripletSum (x:y:z:xs) = [x + y + z] ++ tripletSum (y:z:xs)

part1 :: [Int] -> Int
part1 = length . filter (\[x, y] -> y > x) . windows

part2 :: [Int] -> Int
part2 = part1 . tripletSum
