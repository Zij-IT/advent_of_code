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
