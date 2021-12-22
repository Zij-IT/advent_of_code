module Main where

import qualified D1
import qualified D2
import qualified D3
import qualified D4
import qualified D5
import qualified D6
import qualified D7
import qualified D8
import qualified D9
import qualified D10
import qualified D11
import qualified D12
import qualified D13
import qualified D14
-- import qualified D15
import qualified D16
import qualified D17
import qualified D18
-- import qualified D19
-- import qualified D20
import qualified D21
import qualified D22

runDay :: (Show b, Show c) => String -> (a -> b) -> (a -> c) -> (String -> a) -> IO ()
runDay file pt1 pt2 format = do
  putStrLn $ "Running '" ++ file ++ "'"
  input <- readFile file
  putStrLn $ (++) "------ Part 1: " $ show $ pt1 $ format input
  putStrLn $ (++) "------ Part 2: " $ show $ pt2 $ format input

run13 :: String -> IO ()
run13 file = do
  putStrLn $ "Running '" ++ file ++ "'"
  input <- readFile file
  putStrLn $ (++) "------ Part 1: " $ show $ D13.part1 $ D13.format input
  putStrLn $ (++) "------ Part 2: " $        D13.part2 $ D13.format input

main :: IO ()
main = do
    putStrLn "Advent of Code in Haskell!"
    runDay "./data/d1.txt" D1.part1 D1.part2 D1.format
    runDay "./data/d2.txt" D2.part1 D2.part2 D2.format
    runDay "./data/d3.txt" D3.part1 D3.part2 D3.format
    runDay "./data/d4.txt" D4.part1 D4.part2 D4.format
    runDay "./data/d5.txt" D5.part1 D5.part2 D5.format
    runDay "./data/d6.txt" D6.part1 D6.part2 D6.format
    runDay "./data/d7.txt" D7.part1 D7.part2 D7.format
    runDay "./data/d8.txt" D8.part1 D8.part2 D8.format
    runDay "./data/d9.txt" D9.part1 D9.part2 D9.format
    runDay "./data/d10.txt" D10.part1 D10.part2 D10.format
    runDay "./data/d11.txt" D11.part1 D11.part2 D11.format
    runDay "./data/d12.txt" D12.part1 D12.part2 D12.format
    run13"./data/d13.txt"
    runDay "./data/d14.txt" D14.part1 D14.part2 D14.format
    -- runDay "./data/d15.txt" D15.part1 D15.part2 D15.format
    runDay "./data/d16.txt" D16.part1 D16.part2 D16.format
    runDay "./data/d17.txt" D17.part1 D17.part2 D17.format
    runDay "./data/d18.txt" D18.part1 D18.part2 D18.format
    -- runDay "./data/d19.txt" D19.part1 D19.part2 D19.format
    -- runDay "./data/d20.txt" D20.part1 D20.part2 D20.format
    runDay "./data/d21.txt" D21.part1 D21.part2 D21.format
    runDay "./data/d22.txt" D22.part1 D22.part2 D22.format
