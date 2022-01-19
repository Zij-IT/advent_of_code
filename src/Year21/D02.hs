module Year21.D02
  ( format
  , part1
  , part2
  ) where

data Command = Forward Int | Down Int | Up Int
    deriving (Show)

type Input = [Command]

format :: String -> Input
format = map stringToCommand . lines

stringToCommand :: String -> Command
stringToCommand xs = case break (== ' ') xs of
    ('f':_, num) -> Forward (read num :: Int)
    ('d':_, num) -> Down (read num :: Int)
    ('u':_, num) -> Up (read num :: Int)
    _ -> error "Bad input"

applyCommandPt1 :: Command -> (Int, Int) -> (Int, Int)
applyCommandPt1 cmd (x, y) = case cmd of
    Up num -> (x, y - num)
    Down num -> (x, y + num)
    Forward num -> (x + num, y)

applyCommandPt2 :: Command -> (Int, Int, Int) -> (Int, Int, Int)
applyCommandPt2 cmd (x, y, aim) = case cmd of
    Up num -> (x, y, aim - num)
    Down num -> (x, y, aim + num)
    Forward num -> (x + num, y + aim * num, aim)

part1 :: Input -> Int
part1 = uncurry (*) . foldr applyCommandPt1 (0, 0)

part2 :: Input -> Int
part2 = (\(x, y, z) -> x * y) . foldl (flip applyCommandPt2) (0, 0, 0)
