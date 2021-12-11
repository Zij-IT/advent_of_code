module Main where

import qualified D1 ( format
                    , part1
                    , part2
                    )

import qualified D2 ( format
                    , part1
                    , part2
                    )

import qualified D3 ( format
                    , part1
                    , part2
                    )

import qualified D4 ( format
                    , part1
                    , part2
                    )

import qualified D5 ( format
                    , part1
                    , part2
                    )

import qualified D6 ( format
                    , part1
                    , part2
                    )

import qualified D7 ( format
                    , part1
                    , part2
                    )

import qualified D8 ( format
                    , part1
                    , part2
                    )

import qualified D9 ( format
                    , part1
                    , part2
                    )

import qualified D10 ( format
                    , part1
                    , part2
                    )

import qualified D11 ( format
                    , part1
                    , part2
                    )

runDay :: (Show b, Show c) => String -> (a -> b) -> (a -> c) -> (String -> a) -> IO ()
runDay file pt1 pt2 format = do
  putStrLn $ "Running '" ++ file ++ "'"
  input <- readFile file
  print $ (++) "------ Part 1: " $ show $ pt1 $ format input
  print $ (++) "------ Part 2: " $ show $ pt2 $ format input

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
