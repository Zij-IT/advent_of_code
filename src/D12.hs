module D12
  ( format
  , part1
  , part2
  ) where

import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Data.List (sort, group)
import Data.Char (isUpper, isLower)

type Input = M.Map PathNode [PathNode]
type Output = Int

type PathPredicate = Path -> PathNode -> Bool
type PathNode = String
type Path = [PathNode]

format :: String -> Input
format = M.fromListWith (++) . concatMap ((\[from, to] -> [(from, [to]), (to, [from])]) . splitOn "-") . lines

exploreCaves :: Input -> PathPredicate -> PathNode -> Path -> [Path] -> [Path]
exploreCaves caveMap pred currCave explored pathsToEnd = if currCave == "end"
  then [currCave:explored]
  else pathsToEnd ++ goodPaths
  where
    allowedExplore :: [PathNode]
    allowedExplore = filter (pred (currCave:explored)) $ caveMap M.! currCave

    goodPaths :: [Path]
    goodPaths = concatMap (\cave -> exploreCaves caveMap pred cave (currCave:explored) pathsToEnd) allowedExplore

sharedSolve :: Input -> PathPredicate -> Output
sharedSolve caveMap pred = length $ exploreCaves caveMap pred "start" [] []

part1 :: Input -> Output
part1 = flip sharedSolve pred
  where pred path cave = cave `notElem` path || all isUpper cave

part2 :: Input -> Output
part2 = flip sharedSolve pred
  where
    hasDouble = any (\xs -> length xs > 1) . group . sort . filter (isLower . head)

    pred :: Path -> PathNode -> Bool
    pred path cave = case cave of
      "end" -> cave `notElem` path
      "start" -> cave `notElem` path
      _ -> not (hasDouble path) || cave `notElem` path || all isUpper cave
