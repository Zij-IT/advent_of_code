module D15
  ( format
  , part1
  , part2
  ) where

import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import qualified Data.Set as S

type Input = ([Int], Int, Int)
type Pair = (Int, Int)

format :: String -> Input
format xs =
  let list = concatMap (map digitToInt) $ lines xs
      height = length $ lines xs
      width = length $ head $ lines xs
  in (list, width, height)

spf :: ((Int , Pair) -> [(Int , Pair)]) -> Pair -> (Int , Pair) -> Maybe (Int , Pair)
spf next target start = find mempty (S.singleton start)
    where find visited toBeVisited = S.minView toBeVisited >>=
            \((cost , vertex) , withoutVertex) -> case () of
              _ | vertex == target            -> Just (cost , vertex)
                | vertex `S.member` visited   -> find visited withoutVertex
                | otherwise                   -> find (S.insert vertex visited) (foldr S.insert withoutVertex $ next (cost , vertex))

getNeighbors :: Pair -> [Pair]
getNeighbors (x, y) = [(x + x', y + y') | (x', y') <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

index :: [Int] -> Int -> Int -> Int -> Int -> Maybe Int
index xs width height x y = if withinBounds width height x y
  then Just $ xs !! (width * y + x)
  else Nothing

withinBounds :: Int -> Int -> Int -> Int -> Bool
withinBounds w h x y = x >= 0 && y >= 0 && x < w && y < h

sharedSolve :: [Int] -> Int -> Int -> ((Int , Pair) -> [(Int , Pair)]) -> Int
sharedSolve xs width height next =
  let goal = (width - 1, height - 1)
  in fst . fromJust $ spf next goal (0, (0, 0))

part1 :: Input -> Int
part1 (xs, width, height) = sharedSolve xs width height next
  where
    next :: (Int, Pair) -> [(Int, Pair)]
    next (cost, curr) = map cost' . filter (uncurry (withinBounds width height)) $ getNeighbors curr
      where
        cost' :: Pair -> (Int, Pair)
        cost' p@(x,y) = (cost + fromJust (index xs width height x y), p)

part2 :: Input -> Int
part2 (xs, width, height) = sharedSolve xs fiveWidth fiveHeight next
  where
    fiveWidth = width * 5
    fiveHeight = height * 5

    next :: (Int, Pair) -> [(Int, Pair)]
    next (cost, curr) = map getCost . filter (uncurry (withinBounds fiveWidth fiveHeight)) $ getNeighbors curr
      where
        getCost :: Pair -> (Int, Pair)
        getCost p@(x,y) =
          let y' = y `mod` height
              x' = x `mod` width
              baseCost = fromJust (index xs width height x' y') + quot x width + quot y height
              fixedCost = if baseCost > 9 then baseCost - 9 else baseCost
          in (cost + fixedCost, p)
