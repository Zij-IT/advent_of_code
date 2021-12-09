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

pullTime :: [Int] -> [[Int]] -> [[Int]]
pullTime header = map (map (fromJust . (`elemIndex` header)))

sharedPart ::
  (((Int, Int) -> (Int, Int) -> Ordering) -> [(Int, Int)] -> (Int, Int))
 -> ([Int], [Board])
 -> Int
sharedPart compBy (header, boards) = pulled * nonMarkedSum
  where
    minTime :: [[Int]] -> Int
    minTime xs = minimum . map maximum $ pullTime header xs

    minTimes :: [Int]
    minTimes = map (minTime . ap (++) transpose) boards

    boardId :: Int
    time :: Int
    (boardId, time) = compBy (compare `on` snd) $ zip [0..] minTimes

    nonMarkedSum :: Int
    nonMarkedSum = sum $ filter (`elem` drop (1 + time) header) (concat $ boards !! boardId)

    pulled :: Int
    pulled = header !! time

part1 :: ([Int], [Board]) -> Int
part1 = sharedPart minimumBy

part2 :: ([Int], [Board]) -> Int
part2 = sharedPart maximumBy
