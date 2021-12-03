module D2
  ( format
  , part1
  , part2
  ) where

data Command = Forward Int | Down Int | Up Int
    deriving (Show)

format :: String -> [Command]
format = map stringToCommand . lines

stringToCommand :: String -> Command
stringToCommand xs = case break (\x -> x == ' ') xs of
    ("forward", num) -> Forward (read num :: Int)
    ("down", num) -> Down (read num :: Int)
    ("up", num) -> Up (read num :: Int)

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

part1 :: [Command] -> Int
part1 = uncurry (*) . foldr applyCommandPt1 (0, 0)

part2 :: [Command] -> Int
part2 = uncurry (*) . (\(x, y, z) -> (x, y)) . foldl (flip applyCommandPt2) (0, 0, 0)
