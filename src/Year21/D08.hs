module Year21.D08 ( format
          , part1
          , part2
          ) where

import Data.List       ( (\\), partition, intersect, sort )
import Data.List.Split ( splitOn )
import Data.Map        ( Map )
import qualified Data.Map as M ( (!), fromList )

type Input = [[String]]

format :: String -> Input
format = map words .  lines

toSevenSegMap :: [String] -> Map Char Char
toSevenSegMap xs = M.fromList base
  where
    base = [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]
    [one] = filter ((== 2) . length) xs
    [four] = filter ((== 4) . length) xs
    [seven] = filter ((== 3) . length) xs
    [eight] = filter ((== 7) . length) xs
    zeroSixNine = filter ((== 6) . length) xs
    twoThreeFive = filter ((== 5) . length) xs
    (zeroNine, [six]) = partition ((== 2) . length . intersect one) zeroSixNine
    ([nine], [zero]) = partition (elem d) zeroNine
    ([five], twoThree) = partition (null . (\\ six)) twoThreeFive
    ([two], [three]) = partition ((== 3) . length . intersect five) twoThree

    [a] = seven \\ one
    [b] = five \\ three
    [c] = nine \\ six
    [d] = foldl intersect four twoThreeFive
    [e] = zero \\ nine
    [f] = seven \\ two
    [g] = (nine \\ four) \\ seven

decodeSevenSeg :: Map Char Char -> String -> Int
decodeSevenSeg ssm = signalToNumber . sort . map (ssm M.!)
  where
    signalToNumber :: String -> Int
    signalToNumber s
      | s == "abcefg"  = 0
      | s == "cf"      = 1
      | s == "acdeg"   = 2
      | s == "acdfg"   = 3
      | s == "bcdf"    = 4
      | s == "abdfg"   = 5
      | s == "abdefg"  = 6
      | s == "acf"     = 7
      | s == "abcdefg" = 8
      | s == "abcdfg"  = 9
      | otherwise = error "BAD INPUT"

part1 :: Input -> Int
part1 = sum . map (length . filter validLength . map length . tail . dropWhile(/= "|"))
  where
    validLength :: Int -> Bool
    validLength n  = n == 2 || n == 3 || n == 4 || n == 7

part2 :: Input -> Int
part2 = sum . map getOutput
  where
    getOutput :: [String] -> Int
    getOutput xs = read . concatMap (show . decodeSevenSeg ssm) . tail $ dropWhile (/= "|") xs
      where
        ssm = toSevenSegMap $ takeWhile (/= "|") xs
