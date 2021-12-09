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
type Input = ([Int], [Board])
type OrderingFunction = (((Int, Int) -> (Int, Int) -> Ordering) -> [(Int, Int)] -> (Int, Int))

format :: String -> Input
format xs = (header, boards)
  where
    header :: [Int]
    header = map read . splitOn "," . head $ lines xs

    boards :: [Board]
    boards = extractBoards . tail $ lines xs

    extractBoards :: [String] -> [Board]
    extractBoards [] = []
    extractBoards (_:a:b:c:d:e:xs) = map (map read . words) [a,b,c,d,e] : extractBoards xs
    extractBoards _ = error "Bad input"


pullTime :: [Int] -> [[Int]] -> [[Int]]
pullTime header = map $ map $ fromJust . (`elemIndex` header)

sharedPart :: OrderingFunction -> Input -> Int
sharedPart compBy (header, boards) = pulled * nonMarkedSum
  where
    minTime :: [[Int]] -> Int
    minTime = minimum . map maximum . pullTime header

    minTimes :: [Int]
    minTimes = map (minTime . ap (++) transpose) boards

    boardId :: Int
    time :: Int
    (boardId, time) = compBy (compare `on` snd) $ zip [0..] minTimes

    nonMarkedSum :: Int
    nonMarkedSum = sum . filter (`elem` drop (1 + time) header) . concat $ boards !! boardId

    pulled :: Int
    pulled = header !! time

part1 :: Input -> Int
part1 = sharedPart minimumBy

part2 :: Input -> Int
part2 = sharedPart maximumBy
