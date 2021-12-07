module D4 ( format
          , part1
          , part2
          ) where

import Data.Function (on)
import Data.List (transpose, elemIndex, minimumBy, maximumBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Control.Monad (ap)

type Board = [[Int]]

extractBoards :: [String] -> [Board]
extractBoards [] = []
extractBoards (_:a:b:c:d:e:xs) = map (map read . words) [a,b,c,d,e] : extractBoards xs
_ = error "Bad input"

format :: String -> ([Int], [Board])
format xs = (header, boards)
  where
    inputLines = lines xs
    header = map read $ splitOn "," $ head inputLines
    boards = extractBoards (tail inputLines)

getLastIdx :: [Int] -> [[Int]] -> [[Int]]
getLastIdx header = map (map (fromJust . flip elemIndex header))

sharedPart ::
  (((Int, Int) -> (Int, Int) -> Ordering) -> [(Int, Int)] -> (Int, Int))
 -> ([Int], [Board])
 -> Int
sharedPart compBy (header, boards) = pulled * nonMarkedSum
  where
    minIdx :: [[Int]] -> Int
    minIdx xs = minimum . map maximum $ getLastIdx header xs

    minIndices :: [Int]
    minIndices = map (minIdx . ap (++) transpose) boards

    indexTuple :: (Int, Int)
    indexTuple = compBy (compare `on` snd) $ zip [0..] minIndices

    boardId :: Int
    boardId = fst indexTuple

    nonMarkedSum :: Int
    nonMarkedSum = sum $ filter (`elem` drop (1 + idx) header) (concat $ boards !! boardId)

    idx :: Int
    idx = snd indexTuple

    pulled :: Int
    pulled = header !! idx

part1 :: ([Int], [Board]) -> Int
part1 = sharedPart minimumBy

part2 :: ([Int], [Board]) -> Int
part2 = sharedPart maximumBy
