module Year21.D21
  ( format
  , part1
  , part2
  ) where

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Tuple (swap)

data Player = PlayerOne | PlayerTwo
  deriving (Show, Ord, Eq)

type Pair = (Int, Int)
type Game = (Pair, Pair)
type Input = Pair
type Output = Int

format :: String -> Input
format _ = (9, 10)

move :: Pair -> Int -> Pair
move (p, s) dieResult =
  let newPos = ((p + dieResult - 1) `rem` 10) + 1
  in (newPos, newPos + s)

roll :: (Player, Pair, Pair, Int) -> (Player, Pair, Pair, Int)
roll (PlayerOne, p1, p2, die) = (PlayerTwo, move p1 (3 * die + 3), p2, die + 3)
roll (PlayerTwo, p1, p2, die) = (PlayerOne, p1, move p2 (3 * die + 3), die + 3)

part1 :: Input -> Output
part1 (one, two) = ans . fromJust . find win . zip [0..] $ iterate roll (PlayerOne, (one, 0), (two, 0), 1)
  where
    win :: (Int, (Player, Pair, Pair, Int)) -> Bool
    win (_, (_, (_, s1), (_, s2), _)) = s1 >= 1000 || s2 >= 1000

    ans :: (Int, (Player, Pair, Pair, Int)) -> Int
    ans (idx, (_, (_, s1), (_, s2), _)) =  3 * idx * min s1 s2

allCombinations :: [[Int]]
allCombinations = [[1,1,1],[1,1,2],[1,1,3],[1,2,1],[1,2,2],[1,2,3],[1,3,1],[1,3,2],[1,3,3],[2,1,1],[2,1,2],[2,1,3],[2,2,1],[2,2,2],[2,2,3],[2,3,1],[2,3,2],[2,3,3],[3,1,1],[3,1,2],[3,1,3],[3,2,1],[3,2,2],[3,2,3],[3,3,1],[3,3,2],[3,3,3]]

solve :: Game -> State (M.Map Game Pair) Pair
solve game@(one@(_, s1), two@(_, s2))
    | s1 >= 21 = return (1, 0)
    | s2 >= 21 = return (0, 1)
    | otherwise = do
      result <- get
      case result M.!? game of
        Just res -> return res
        Nothing  -> do
          result <- swap
                     .  foldl (\(x,y) (a,b) -> (x + a, y + b)) (0, 0)
                    <$> traverse solve [(two, move one step) | step <- sum <$> allCombinations]
          modify (M.insert game result)
          return result

part2 :: Input -> Output
part2 (one, two) = uncurry max $ evalState (solve ((one, 0), (two, 0))) M.empty
