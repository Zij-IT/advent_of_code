module D10
  ( format
  , part1
  , part2
  ) where

import Data.Maybe (mapMaybe)
import Data.List (sort)

type Input = [String]
type Output = Int

format :: String -> Input
format = lines

evaluate :: String -> String -> Either (Maybe Char) [Char]
evaluate [] [] = Left Nothing
evaluate [] ys = Right ys
evaluate (x:line) [] = evaluate line [x]
evaluate (x:line) (y:stack) = case (x, y) of
  ('(', _) -> evaluate line (x:y:stack)
  ('{', _) -> evaluate line (x:y:stack)
  ('<', _) -> evaluate line (x:y:stack)
  ('[', _) -> evaluate line (x:y:stack)
  (')', '(') -> evaluate line stack
  ('}', '{') -> evaluate line stack
  (']', '[') -> evaluate line stack
  ('>', '<') -> evaluate line stack
  _ -> Left $ Just x

scoreCorruption :: Char -> Int
scoreCorruption x
  | x == ')' = 3
  | x == ']' = 57
  | x == '}' = 1197
  | otherwise = 25137

scoreCompletion :: Char -> Int
scoreCompletion x
  | x == '(' = 1
  | x == '[' = 2
  | x == '{' = 3
  | otherwise = 4

onlyLeft :: Either (Maybe Char) [Char] -> Maybe Char
onlyLeft (Right _) = Nothing
onlyLeft (Left  x) = x

onlyRight :: Either (Maybe Char) [Char] -> [Char]
onlyRight (Left  _) = []
onlyRight (Right x) = x

part1 :: Input -> Output
part1 = sum . mapMaybe (fmap scoreCorruption . onlyLeft . (`evaluate` []))

part2 :: Input -> Int
part2 = median . dropWhile (==0) . sort . map (foldl (\acc x -> 5 * acc + x) 0 . map scoreCompletion . onlyRight . (`evaluate` []))
  where median xs = xs !! quot (length xs) 2
