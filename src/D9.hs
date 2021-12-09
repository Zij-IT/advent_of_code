module D9 ( format
          , part1
          , part2
          ) where

import Data.Maybe (catMaybes, mapMaybe, fromJust, isJust)
import Data.Char (digitToInt)
import Data.List (sort)

type Input = [[Int]]
type Pair = (Int, Int)

format :: String -> Input
format = map (map digitToInt) . lines

(!?) :: Int -> [a] -> Maybe a
(!?) n xs = if n >= 0 && (length xs > n) then Just (xs !! n) else Nothing

allCoords :: Input -> [Pair]
allCoords xs = [(x, y) | y <- [0 .. length xs - 1], x <- [0 .. length (head xs) - 1]]

index :: Input -> Pair -> Maybe Int
index xs (x, y) = (y !? xs) >>= (x !?)

getNeighbors :: Pair -> [Pair]
getNeighbors (x, y) = [(x + x', y + y') | (x', y') <- [(1, 0), (0, 1), (-1, 0), (0, -1)]]

neighborValues :: Input -> Pair -> [Maybe Int]
neighborValues xs = map (index xs) . getNeighbors

isSmallest :: Input -> Pair -> Bool
isSmallest xs (x, y) = all (> (fromJust $ index xs (x, y))) $ catMaybes $ neighborValues xs (x, y)

part1 :: Input -> Int
part1 xs = sum . mapMaybe (fmap (+1) . index xs) $ filter (isSmallest xs) $ allCoords xs

part2 :: Input -> Int
part2 xs = product . take 3 . reverse . sort . map (length . basins xs []) $ filter (isSmallest xs) $ allCoords xs
  where
    basins :: Input -> [Pair] -> Pair -> [Pair]
    basins xs checked p = if isJust(index xs p) && index xs p < Just 9 && p `notElem` checked then
      let [a, b, c, d] = getNeighbors p
          b' = basins xs (p:checked) a
          c' = basins xs b' b
          d' = basins xs c' c
      in basins xs d' d
    else checked
