module Year21.D17
  ( format
  , part1
  , part2
  ) where

import Data.Maybe (fromJust)
import Data.List (find)
import Data.List.Split (splitOn)

type Pair = (Int, Int)
type Input = (Pair, Pair)
type Output = Int

format :: String -> Input
format str =
  let [[x1, x2], [y1, y2]] = map (map read . splitOn "..") . splitOn ", y=" $ drop 15 str
  in ((x1, y1), (x2, y2))

launch :: Int -> Pair -> [Pair]
launch y1 = takeWhile ((>= y1) . snd) . fly (0, 0)
  where
    fly (x, y) (dx, dy) = (x, y) : fly (x + dx, y + dy) (dx - signum dx, dy - 1)

withinRange :: Input -> ([Pair] -> Bool)
withinRange ((x1, y1), (x2, y2)) = any (\(x, y) -> x1 <= x && x <= x2 && y1 <= y && y <= y2)

allArcs :: (Pair, Pair) -> [[Pair]]
allArcs input@((x1, y1), (x2, y2)) = [ arc | dx <- [1 .. x2] , dy <- [y1 .. negate y1], let arc = launch y1 (dx, dy), withinRange input arc]

part1 :: Input -> Output
part1 = maximum . map snd . concat . allArcs

part2 :: Input -> Output
part2 = length . allArcs
