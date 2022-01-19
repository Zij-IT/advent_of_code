module Year21.D19
  ( format
  , part1
  , part2
  ) where

import qualified Data.Set as S
import Data.Either (partitionEithers)
import Data.List (foldl')
import Data.List.Split (splitWhen)
import Data.Maybe (mapMaybe, fromJust)
import Data.Foldable (find)

type Beacons  = S.Set XYZ
type Scanners = [XYZ]
type Input    = [Beacons]
type Output   = Int

-- COORD STUFF -- {
data XYZ = XYZ Int Int Int
  deriving (Show, Eq, Ord)

instance Num XYZ where
  (+) (XYZ a b c) (XYZ a' b' c') = XYZ (a + a') (b + b') (c + c')
  (*) (XYZ a b c) (XYZ a' b' c') = XYZ (a * a') (b * b') (c * c')
  abs (XYZ a b c) = XYZ (abs a) (abs b) (abs c)
  negate (XYZ a b c) = XYZ (-a) (-b) (-c)
  signum (XYZ a b c) = XYZ (signum a) (signum b) (signum c)
  fromInteger a = XYZ 0 0 0

norm :: XYZ -> Int
norm (XYZ x y z) = abs x + abs y + abs z

fromTuple :: (Int, Int, Int) -> XYZ
fromTuple (x, y, z) = XYZ x y z

allRotations :: S.Set XYZ -> [S.Set XYZ]
allRotations ps = [ S.map (flip1 . flip2 . flip3 . swap) ps
                  | flip1 <- [id, flipX]
                  , flip2 <- [id, flipY]
                  , flip3 <- [id, flipZ]
                  , swap  <- [swapXYZ, swapXZY, swapYXZ, swapYZX, swapZXY, swapZYX]
                  ]
    where flipX   (XYZ x y z) = XYZ (-x)  y   z
          flipY   (XYZ x y z) = XYZ   x (-y)  z
          flipZ   (XYZ x y z) = XYZ   x   y (-z)
          swapXYZ (XYZ x y z) = XYZ   x   y   z
          swapXZY (XYZ x y z) = XYZ   x   z   y
          swapYXZ (XYZ x y z) = XYZ   y   x   z
          swapYZX (XYZ x y z) = XYZ   y   z   x
          swapZXY (XYZ x y z) = XYZ   z   x   y
          swapZYX (XYZ x y z) = XYZ   z   y   x

-- COORD STUFF -- }

format :: String -> Input
format = map (S.fromList . map toCoord . tail) . splitWhen (=="") . lines
  where
    toCoord :: String -> XYZ
    toCoord x = fromTuple $ read ("(" ++ x ++ ")")

alignOn :: Beacons -> Beacons -> Either Beacons (Beacons, XYZ)
alignOn fixed ps =
  let shifts = [ (shifted, shift)
               | fixedPoint <- S.toList fixed
               , rotatedS   <- allRotations ps
               , rotatedXYZ <- S.toList rotatedS
               , let shift = fixedPoint - rotatedXYZ
               , let shifted = S.map (+ shift) rotatedS
               , length (fixed `S.intersection` shifted) >= 12
               ]
  in if null shifts
  then Left ps
  else Right $ head shifts

align :: (Beacons, Scanners, [Beacons]) -> (Beacons, Scanners, [Beacons])
align (aligned, shifts, others) =
  let (unaligned, shifted)  = partitionEithers $ map (alignOn aligned) others
      aligned'              = S.unions $ aligned : map fst shifted
      shifts'               = shifts ++ map snd shifted
  in  (aligned', shifts', unaligned)

alignBeacons :: [Beacons] -> (Beacons, Scanners)
alignBeacons beacons =
  let (aligned, scanners, _) = fromJust
                               . find (\(_, _, unaligned) -> null unaligned)
                               $ iterate align (head beacons, [], tail beacons)
  in  (aligned, scanners)

part1 :: Input -> Output
part1 = length . fst . alignBeacons

part2 :: Input -> Output
part2 input =
  let sc = snd . alignBeacons $ input
      distances = [norm $ p1 - p2 | p1 <- sc, p2 <- sc, p1 /= p2]
  in maximum distances
