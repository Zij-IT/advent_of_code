module Year21.D12
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

type PathNode = String
type Path = [PathNode]

format :: String -> Input
format = M.fromListWith (++) . concatMap ((\[from, to] -> [(from, [to]), (to, [from])]) . splitOn "-") . lines

exploreCaves :: Input -> PathNode -> Path -> Bool -> Int
exploreCaves caveMap currCave explored twoSmall = if currCave == "end" then 1 else goodPaths
  where
    newPath :: Path
    newPath = currCave:explored

    hasTwoSmall :: Bool
    hasTwoSmall = twoSmall || (any ((>1) . length) . group . sort $ filter (isLower . head) newPath)

    canInsert :: Path -> PathNode -> Bool
    canInsert path cave = case cave of
      "end" -> cave `notElem` path
      "start" -> cave `notElem` path
      _ -> not hasTwoSmall || cave `notElem` path || all isUpper cave

    allowedExplore :: [PathNode]
    allowedExplore = filter (canInsert newPath) $ caveMap M.! currCave

    goodPaths :: Int
    goodPaths = sum $ map (\cave -> exploreCaves caveMap cave newPath hasTwoSmall) allowedExplore

sharedSolve :: Input -> Bool -> Output
sharedSolve caveMap = exploreCaves caveMap "start" []

part1 :: Input -> Output
part1 = flip sharedSolve True

part2 :: Input -> Output
part2 = flip sharedSolve False
