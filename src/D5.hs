module D5 ( format
          , part1
          , part2
          , diagonal
          ) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Point = (Int, Int)

format :: String -> [(Point, Point)]
format = map (arrayToPoints . concat . ((map splitCommas) . splitOn " -> ")) . lines
  where
    splitCommas :: String -> [Int]
    splitCommas = (map read) . splitOn ","

    arrayToPoints :: [Int] -> (Point, Point)
    arrayToPoints = (\[x1, y1, x2, y2] -> ((x1, y1), (x2, y2)))

toLine :: (Point, Point) -> (Int -> Int -> Int -> Int -> [Point]) -> [Point]
toLine ((x1, y1), (x2, y2)) f = case (compare x1 x2, compare y1 y2) of
    (EQ,  _)  -> [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    (_ , EQ)  -> [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    otherwise -> f x1 y1 x2 y2

sharedSolve :: [Point] -> Int
sharedSolve = Map.size . Map.filter (>= 2) . Map.fromListWith (+) . flip zip (repeat 1)

part1 :: [(Point, Point)] -> Int
part1 = sharedSolve . concat . map (flip toLine (\_ _ _ _ -> []))

part2 :: [(Point, Point)] -> Int
part2 = sharedSolve . concat . map (flip toLine diagonal)
  where
    diagonal :: Int -> Int -> Int -> Int -> [Point]
    diagonal x1 y1 x2 y2 = map (\z -> (x1 + z * fst slope, y1 + z * snd slope)) [0 .. abs x]
      where
      x :: Int
      x = x2 - x1

      y :: Int
      y = y2 - y1

      slope :: (Int, Int)
      slope = case (x /= 0, y /= 0) of
        (True, _) -> (quot x (abs x), quot y (abs x))
        (_, True) -> (quot x (abs x), quot y (abs x))
        _ -> (x, y)
