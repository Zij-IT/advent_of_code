module Year21.D20
  ( format
  , part1
  , part2
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.List (sort)
import Data.Maybe (fromMaybe)

data Pixel = Light | Dark
  deriving (Ord, Eq)

instance Show Pixel where
  show Light = "#"
  show Dark  = "."

charToPixel '#' = Light
charToPixel  _  = Dark

flip' :: Pixel -> Pixel
flip' Light = Dark
flip' Dark = Light

type Pair = (Int, Int)
type Grid = M.Map Pair Pixel
type Input = (V.Vector Pixel, Grid)
type Output = Int

-- Parsing
format :: String -> Input
format str =
  let (header, karte) = splitAt 1 $ lines str
  in (V.fromList . map charToPixel $ head header, arrToGrid $ map (map charToPixel) $ tail karte)

arrToGrid :: [[Pixel]] -> Grid
arrToGrid = M.fromList . concatMap pointLines . zip [0..]
  where
    pointLines :: (Int, [Pixel]) -> [(Pair, Pixel)]
    pointLines (y, xs) = zipWith (\x v -> ((x, y), v)) [0..] xs

-- Solving
pixelIndex :: Grid -> Pair -> Pixel
pixelIndex grid pair = fromMaybe Dark (grid M.!? pair)

getThreeGrid :: Grid -> Pixel -> Pair -> [Pixel]
getThreeGrid grid def (x, y) = [M.findWithDefault def (x + x', y + y') grid
                           | y' <- [-1..1]
                           , x' <- [-1..1]
                           ]

toDecimal :: [Pixel] -> Int
toDecimal = foldl (\acc x -> 2 * acc + if x == Light then 1 else 0) 0

step :: V.Vector Pixel -> (Grid, Pixel) -> (Grid, Pixel)
step header (grid, color) = (M.mapWithKey keyToNew paddedGrid, flip' color)
  where
    keyToNew :: Pair -> Pixel -> Pixel
    keyToNew pair _ = (header V.!) . toDecimal . getThreeGrid paddedGrid color $ pair

    (minX, minY) = fst $ M.findMin grid
    (maxX, maxY) = fst $ M.findMax grid

    paddedGrid = M.fromList [((x, y), a)
                            | x <- [minX-2..maxX+2]
                            , y <- [minY-2..maxY+2]
                            , let a = M.findWithDefault color (x,y) grid
                            ]

solve :: Int -> Input -> Output
solve n (header, grid) = length
                        . filter (==Light)
                        . M.elems
                        . fst
                        . (!! n)
                        . iterate (step header)
                        $ (grid, Dark)

part1 :: Input -> Output
part1 = solve 2
part2 :: Input -> Output
part2 = solve 50
