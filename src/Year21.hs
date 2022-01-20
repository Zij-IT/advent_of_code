module Year21
  ( runDay
  ) where

import qualified Year21.D01
import qualified Year21.D02
import qualified Year21.D03
import qualified Year21.D04
import qualified Year21.D05
import qualified Year21.D06
import qualified Year21.D07
import qualified Year21.D08
import qualified Year21.D09
import qualified Year21.D10
import qualified Year21.D11
import qualified Year21.D12
import qualified Year21.D13
import qualified Year21.D14
import qualified Year21.D15
import qualified Year21.D16
import qualified Year21.D17
import qualified Year21.D18
import qualified Year21.D19
import qualified Year21.D20
import qualified Year21.D21
import qualified Year21.D22
-- import qualified Year21.D23
-- import qualified Year21.D24
-- import qualified Year21.D25

-- PROJECT STRUCTURE:
-- app/Main.js
-- src/YearXY/DZ.hs
-- data/YearXY/DZ.txt

runDay' :: (Show b, Show c) => Int -> (String -> a) -> (a -> b) -> (a -> c) -> IO ()
runDay' day format pt1 pt2 = do
  putStrLn $ "Running '" ++ show day ++ "'"
  input <- readFile $ "./data/Year21/d" ++ show day ++ ".txt"
  putStrLn . (++) "------ Part 1: " . show . pt1 $ format input
  putStrLn . (++) "------ Part 2: " . show . pt2 $ format input

runDay :: Int -> IO ()
runDay day = case day of
  1  -> runDay' day Year21.D01.format Year21.D01.part1 Year21.D01.part2
  2  -> runDay' day Year21.D02.format Year21.D02.part1 Year21.D02.part2
  3  -> runDay' day Year21.D03.format Year21.D03.part1 Year21.D03.part2
  4  -> runDay' day Year21.D04.format Year21.D04.part1 Year21.D04.part2
  5  -> runDay' day Year21.D05.format Year21.D05.part1 Year21.D05.part2
  6  -> runDay' day Year21.D06.format Year21.D06.part1 Year21.D06.part2
  7  -> runDay' day Year21.D07.format Year21.D07.part1 Year21.D07.part2
  8  -> runDay' day Year21.D08.format Year21.D08.part1 Year21.D08.part2
  9  -> runDay' day Year21.D09.format Year21.D09.part1 Year21.D09.part2
  10 -> runDay' day Year21.D10.format Year21.D10.part1 Year21.D10.part2
  11 -> runDay' day Year21.D11.format Year21.D11.part1 Year21.D11.part2
  12 -> runDay' day Year21.D12.format Year21.D12.part1 Year21.D12.part2
  13 -> runDay' day Year21.D13.format Year21.D13.part1 Year21.D13.part2
  14 -> runDay' day Year21.D14.format Year21.D14.part1 Year21.D14.part2
  15 -> runDay' day Year21.D15.format Year21.D15.part1 Year21.D15.part2
  16 -> runDay' day Year21.D16.format Year21.D16.part1 Year21.D16.part2
  17 -> runDay' day Year21.D17.format Year21.D17.part1 Year21.D17.part2
  18 -> runDay' day Year21.D18.format Year21.D18.part1 Year21.D18.part2
  19 -> runDay' day Year21.D19.format Year21.D19.part1 Year21.D19.part2
  20 -> runDay' day Year21.D20.format Year21.D20.part1 Year21.D20.part2
  21 -> runDay' day Year21.D21.format Year21.D21.part1 Year21.D21.part2
  22 -> runDay' day Year21.D22.format Year21.D22.part1 Year21.D22.part2
  -- 23 -> runDay' day Year21.D23.format Year21.D23.part1 Year21.D23.part2
  -- 24 -> runDay' day Year21.D24.format Year21.D24.part1 Year21.D24.part2
  -- 25 -> runDay' day Year21.D25.format Year21.D25.part1 Year21.D25.part2
  _  -> error "Invalid day."
