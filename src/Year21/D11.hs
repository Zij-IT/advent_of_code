module Year21.D11
  ( format
  , part1
  , part2
  ) where

import Data.Char (digitToInt)
import qualified Data.Map.Strict as M
import Data.List (sort, group)
import Data.Maybe (isNothing)

type Output = Int
type Pair = (Int, Int)
type OctoMap = M.Map Pair (Maybe Int)

format :: String -> OctoMap
format = toOctoMap . map (map digitToInt) . lines
  where
    toOctoMap :: [[Int]] -> OctoMap
    toOctoMap xs = M.fromAscList $ zip (allCoords xs) (map Just $ concat xs)

allCoords :: [[a]] -> [Pair]
allCoords xs = [(x, y) | y <- [0 .. length xs - 1], x <- [0 .. length (head xs) - 1]]

getNeighbors :: Pair -> [Pair]
getNeighbors (x, y) = [(x + x', y + y') | x' <- [-1 .. 1], y' <- [-1 .. 1]]

increaseEnergy :: OctoMap -> OctoMap
increaseEnergy = M.map ((+1) <$>)

flash :: OctoMap -> Int -> (OctoMap, Int)
flash om flashCount = if null flashPoints
  then (om, flashCount)
  else flash postFlash $ flashCount + length flashPoints
  where
    flashPoints :: [Pair]
    flashPoints = M.keys $ M.filter (> Just 9) om

    postFlash :: OctoMap
    postFlash = M.mapWithKey incByCount preFlash
      where
        preFlash :: OctoMap
        preFlash = M.map (\x -> if x > Just 9 then Nothing else x) om

        incByCount :: Pair -> Maybe Int -> Maybe Int
        incByCount key val = case flashedCount M.!? key of
          Just original -> fmap (+original) val
          Nothing -> val
          where
            flashedCount :: M.Map Pair Int
            flashedCount =
              M.fromList
              . map (\g -> (head g, length g))
              . group
              . sort
              . filter (\(x, y) -> x >= 0 && y >= 0 && x < 10 && y < 10)
              $ concatMap getNeighbors flashPoints

step :: (OctoMap, Int) -> (OctoMap, Int)
step (om, count) = (resetFlashed newMap, newCount)
  where
    (newMap, newCount) = flash (increaseEnergy om) count
    resetFlashed = M.map (\x -> if isNothing x then Just 0 else x)

part1 :: OctoMap -> Int
part1 om = (!! 100) . map snd . iterate step $ (om, 0)

part2 :: OctoMap -> Int
part2 om = fst . head . dropWhile (not . allZero . snd) . zip [0..] . map fst . iterate step $ (om, 0)
  where
    allZero :: OctoMap -> Bool
    allZero om = M.size (M.filter (/= Just 0) om) == 0
