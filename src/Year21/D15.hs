module Year21.D15
  ( format
  , part1
  , part2
  ) where

import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Vector as V

type Input = (V.Vector Int, Int, Int)
type Pair = (Int, Int)

format :: String -> Input
format xs =
  let list = concatMap (map digitToInt) $ lines xs
      height = length $ lines xs
      width = length $ head $ lines xs
  in (V.fromList list, width, height)

spf :: ((Int , Pair) -> [(Int , Pair)]) -> Pair -> (Int , Pair) -> Maybe (Int , Pair)
spf next target start = find mempty (S.singleton start)
    where find visited toBeVisited = S.minView toBeVisited >>=
            \((cost , vertex) , withoutVertex) -> case () of
              _ | vertex == target            -> Just (cost , vertex)
                | vertex `S.member` visited   -> find visited withoutVertex
                | otherwise                   -> find (S.insert vertex visited) (foldr S.insert withoutVertex $ next (cost , vertex))

getNeighbors :: Pair -> [Pair]
getNeighbors (x, y) = [(x + x', y + y') | (x', y') <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

withinBounds :: Int -> Int -> Int -> Int -> Bool
withinBounds w h x y = x >= 0 && y >= 0 && x < w && y < h

index :: V.Vector Int -> Int -> Int -> Int -> Int -> Maybe Int
index xs width height x y = if withinBounds width height x y
  then Just $ V.unsafeIndex xs (width * y + x)
  else Nothing

sharedSolve :: V.Vector Int -> Int -> Int -> (Int -> Pair -> (Int, Pair)) -> Int
sharedSolve xs width height costFunc = fst . fromJust $ spf next goal (0, (0, 0))
  where
    goal :: Pair
    goal = (width - 1, height - 1)

    next :: (Int, Pair) -> [(Int, Pair)]
    next (cost, curr) = map (costFunc cost)
      $ filter (uncurry (withinBounds width height))
      $ getNeighbors curr

part1 :: Input -> Int
part1 (xs, width, height) = sharedSolve xs width height cost'
  where
    cost' :: Int -> Pair -> (Int, Pair)
    cost' cost p@(x,y) = (cost + fromJust (index xs width height x y), p)

part2 :: Input -> Int
part2 (xs, width, height) = sharedSolve xs (width * 5) (height * 5) cost'
  where
    cost' :: Int -> Pair -> (Int, Pair)
    cost' cost p@(x,y) =
      let y' = y `mod` height
          x' = x `mod` width
          baseCost = fromJust (index xs width height x' y') + quot x width + quot y height
          fixedCost = if baseCost > 9 then baseCost - 9 else baseCost
      in (cost + fixedCost, p)
