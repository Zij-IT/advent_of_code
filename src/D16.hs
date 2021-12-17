{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module D16
  ( format
  , part1
  , part2
  ) where

import Data.Maybe (fromJust)
import Data.Bifunctor (first)
import Control.Arrow ((***))

type Input = [Int]
type Output = Int

data Packet = Literal  { id :: Int, value :: Int }
            | Operator { id :: Int, subpackets :: [Packet], f :: [Int] -> Int }

format :: String -> Input
format = concatMap toBinRep
  where toBinRep c = case c of
           '0' -> [0, 0, 0, 0]; '4' -> [0, 1, 0, 0]; '8' -> [1, 0, 0, 0]; 'C' -> [1, 1, 0, 0]
           '1' -> [0, 0, 0, 1]; '5' -> [0, 1, 0, 1]; '9' -> [1, 0, 0, 1]; 'D' -> [1, 1, 0, 1]
           '2' -> [0, 0, 1, 0]; '6' -> [0, 1, 1, 0]; 'A' -> [1, 0, 1, 0]; 'E' -> [1, 1, 1, 0]
           '3' -> [0, 0, 1, 1]; '7' -> [0, 1, 1, 1]; 'B' -> [1, 0, 1, 1]; 'F' -> [1, 1, 1, 1]
           _   -> error "BAD INPUT"

funcFromType :: [Int] -> ([Int] -> Int)
funcFromType t = case t of
  [0, 0, 0] -> sum    ; [1, 0, 1] -> \[x, y] -> if x >  y then 1 else 0
  [0, 0, 1] -> product; [1, 1, 0] -> \[x, y] -> if x <  y then 1 else 0
  [0, 1, 0] -> minimum; [1, 1, 1] -> \[x, y] -> if x == y then 1 else 0
  [0, 1, 1] -> maximum;

binListToDecimal :: [Int] -> Int
binListToDecimal = foldl (\acc x -> 2 * acc + x) 0

getLiteral :: [Int] -> Maybe ([Int], [Int])
getLiteral lit = case lit of
  (0:xs) -> Just $ splitAt 4 xs
  (1:xs) ->
    let (bits, rest) = splitAt 4 xs
    in first (bits ++) <$> getLiteral rest
  _ -> error "Bad literal parse"

parseLiteral :: [Int] -> [Int] -> Maybe (Packet, [Int])
parseLiteral v xs = first (Literal (binListToDecimal v) . binListToDecimal) <$> getLiteral xs

parsePackets :: [Int] -> [Packet]
parsePackets xs = case parseHierarchy xs of
  Just (packet, rest) -> packet : parsePackets rest
  Nothing -> []

parseNPackages :: [Int] -> Int -> ([Packet], [Int])
parseNPackages xs n = first reverse . fromJust . (!! n) $ iterate conv $ Just ([], xs)
  where conv = \(Just (packets, xs)) -> first (:packets) <$> parseHierarchy xs

parseOperator :: [Int] -> [Int] -> [Int] -> Maybe (Packet, [Int])
parseOperator v t (lengthId : xs)
  | null xs = Nothing
  | lengthId == 0 =
    let (lenBits, rest) = splitAt 15 xs
        len = binListToDecimal lenBits
        ver = binListToDecimal v
        func = funcFromType t
        (packets, rest') = splitAt len rest
        packages = parsePackets packets
    in Just (Operator ver packages func, rest')
  | otherwise =
    let (countBits, rest) = splitAt 11 xs
        count = binListToDecimal countBits
        ver = binListToDecimal v
        func = funcFromType t
        (packages, rest') = parseNPackages rest count
    in Just (Operator ver packages func, rest')

parsePacket :: [Int] -> [Int] -> [Int] -> Maybe (Packet, [Int])
parsePacket _ _ [] = Nothing
parsePacket v t xs = case t of
    [1, 0, 0] -> parseLiteral v xs
    _         -> parseOperator v t xs

parseHierarchy :: Input -> Maybe (Packet, [Int])
parseHierarchy xs =
  let (version,  postV) = splitAt 3 xs
      (pType,    postP) = splitAt 3 postV
  in parsePacket version pType postP

part1 :: Input -> Int
part1 = sum . onlyVersions . fst . fromJust . parseHierarchy
    where onlyVersions (Literal id _) = [id]
          onlyVersions (Operator id packets _) = id : concatMap onlyVersions packets

part2 :: Input -> Int
part2 = calcValue . fst . fromJust . parseHierarchy
    where calcValue (Literal _ value) = value
          calcValue (Operator _ packets f) = f $ map calcValue packets
