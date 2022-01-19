module Year21.D5 ( format
          , part1
          , part2
          ) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Point = (Int, Int)

format :: String -> [(Point, Point)]
format = map (arrayToPoints . concatMap splitCommas . splitOn " -> ") . lines
  where
    splitCommas :: String -> [Int]
    splitCommas = map read . splitOn ","

    arrayToPoints :: [Int] -> (Point, Point)
    arrayToPoints = \[x1, y1, x2, y2] -> ((x1, y1), (x2, y2))

range :: Int -> Int -> [Int]
range a b
  | a < b = [a..b]
  | otherwise = [a, (a-1) .. b]

toLine :: (Point, Point) -> (Int -> Int -> Int -> Int -> [Point]) -> [Point]
toLine ((x1, y1), (x2, y2)) f
  | x1 == x2 = [(x1, y) | y <- range y1 y2]
  | y1 == y2 = [(x, y1) | x <- range x1 x2]
  | otherwise = f x1 y1 x2 y2

sharedSolve :: [Point] -> Int
sharedSolve = Map.size . Map.filter (>= 2) . Map.fromListWith (+) . flip zip (repeat 1)

part1 :: [(Point, Point)] -> Int
part1 = sharedSolve . concatMap (`toLine` (\_ _ _ _ -> []))

part2 :: [(Point, Point)] -> Int
part2 = sharedSolve . concatMap (`toLine` (\x1 y1 x2 y2 -> zip (range x1 x2) (range y1 y2)))
