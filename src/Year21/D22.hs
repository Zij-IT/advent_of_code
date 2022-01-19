{-# LANGUAGE TupleSections #-}
module Year21.D22
  ( format
  , part1
  , part2
  ) where

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Bifunctor (first, second)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Pair = (Int, Int)
type TriPair = (Pair, Pair, Pair)
type Input = [(Cube, TriPair)]
type Output = Int

data Cube = On | Off deriving (Ord, Show, Eq)

-- Helper Functions for TriPair
px :: TriPair -> Pair
px (x, _, _) = x

py :: TriPair -> Pair
py (_, y, _) = y

pz :: TriPair -> Pair
pz (_, _, z) = z

intersect :: TriPair -> TriPair -> Maybe TriPair
intersect one two =
  let lx = max' px one two
      rx = min' px one two
      ly = max' py one two
      ry = min' py one two
      lz = max' pz one two
      rz = min' pz one two
  in if lx > rx || ly > ry || lz > rz
  then Nothing
  else Just ((lx, rx), (ly, ry), (lz, rz))
    where max' c tp1 tp2 = max (fst $ c tp1) (fst $ c tp2)
          min' c tp1 tp2 = min (snd $ c tp1) (snd $ c tp2)

format :: String -> Input
format = map toInstruction . lines
  where
    toInstruction :: String -> (Cube, TriPair)
    toInstruction str =
      let [cube, coords] = splitOn " " str
          [[xn, xx], [yn, yx], [zn, zx]] = map (map read . splitOn ".." . drop 2) $ splitOn "," coords
          cube' = if cube == "on" then On else Off
      in (cube', ((xn, xx), (yn, yx), (zn, zx)))

solve :: ((TriPair, Int) -> Bool) -> Input -> Output
solve f = sum . map prod . filter f . M.toList . foldl put M.empty
  where
    put :: M.Map TriPair Int -> (Cube, TriPair) -> M.Map TriPair Int
    put hmap (cube, curr) =
      let inters = M.fromListWith (+) $ mapMaybe (\(tp, w) -> (, -w) <$> intersect tp curr) $ M.toList hmap
          newMap = if cube == On then M.insertWith (+) curr 1 hmap else hmap
      in M.unionWith (+) newMap inters

    prod :: (TriPair, Int) -> Int
    prod (((xn, xx), (yn, yx), (zn,zx)), amt) =
      let dx = xx - xn + 1
          dy = yx - yn + 1
          dz = zx - zn + 1
      in dx * dy * dz * amt

part1 :: Input -> Output
part1 = solve withinRange
  where
    withinRange :: (TriPair, Int) -> Bool
    withinRange (((xn, xx), (yn, yx), (zn, zx)), _) = xn >= (-50) && xx <= 50
                                                   && yn >= (-50) && yx <= 50
                                                   && zn >= (-50) && zx <= 50

part2 :: Input -> Output
part2 = solve (const True)
