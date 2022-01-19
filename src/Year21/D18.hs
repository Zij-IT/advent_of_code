module Year21.D18
  ( format
  , part1
  , part2
  ) where

data Snail = Pair Snail Snail | Singleton Int

instance Show Snail where
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"
  show (Singleton a) = show a

type Input = Int
type Output = Int

format :: String -> Input
format _ = 0 -- read

part1 :: Input -> Output
part1 _ = 0

part2 :: Input -> String
part2 _  = show (Pair (Singleton 2) (Pair (Singleton 1) (Singleton 2)))
