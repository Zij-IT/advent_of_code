module D13
  ( format
  , part1
  , part2
  ) where

import qualified Data.Set as S
import Data.List (maximumBy, minimumBy)
import Data.List.Split (splitOn)
import Data.Function (on)

data Fold = X Int | Y Int

type Pair = (Int, Int)
type Input = ([Pair], [Fold])

format :: String -> Input
format = (\(x, y) -> (map toTuple x, map toFold $ drop 1 y)) . break null . lines
  where
    toTuple = (\[x, y] -> (x, y)) . map read . splitOn ","
    toFold  = foldFromPair . splitOn "=" . drop 11
    foldFromPair [x, y] = case x of
      "x" -> X (read y)
      "y" -> Y (read y)
      _ -> error "BAD INPUT"
    foldFromPair _ = error "BAD INPUT"

foldSet :: S.Set Pair -> Fold -> S.Set Pair
foldSet coords fold = S.map (func fold) coords
  where
    func :: Fold -> Pair -> Pair
    func (X fx) (cx, cy) = if cx > fx then (cx - 2 * (cx - fx), cy) else (cx, cy)
    func (Y fy) (cx, cy) = if cy > fy then (cx, cy - 2 * (cy - fy)) else (cx, cy)

part1 :: Input -> Int
part1 (coords, folds) = S.size $ foldSet (S.fromList coords) (head folds)

part2 :: Input -> String
part2 (coords, folds) =
  let set = foldl foldSet (S.fromList coords) folds
      isX (X _) = True
      isX (Y _) = False
      maxX = minimum $ map (\(X x) -> x) $ filter isX folds
      maxY = minimum $ map (\(Y y) -> y) $ filter (not . isX) folds
  in '\n' : concat [(if S.member (x', y') set then "#" else " ") ++ (if x' == maxX then "\n" else "") | y' <- [0..maxY], x' <- [0..maxX]]
