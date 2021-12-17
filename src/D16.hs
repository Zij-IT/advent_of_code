{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module D16
  ( format
  , part1
  , part2
  ) where

import Data.Maybe (fromJust)
import Data.Bifunctor (first)

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

funcFromType :: Int -> ([Int] -> Int)
funcFromType t = case t of
  0 -> sum    ; 5 -> \[x, y] -> if x >  y then 1 else 0
  1 -> product; 6 -> \[x, y] -> if x <  y then 1 else 0
  2 -> minimum; 7 -> \[x, y] -> if x == y then 1 else 0
  3 -> maximum;

binToDec :: [Int] -> Int
binToDec = foldl (\acc x -> 2 * acc + x) 0

getLiteral :: [Int] -> Maybe ([Int], [Int])
getLiteral lit = case lit of
  (0:xs) -> Just $ splitAt 4 xs
  (1:xs) -> first (take 4 xs ++) <$> getLiteral (drop 4 xs)

parseLiteral :: Int -> [Int] -> Maybe (Packet, [Int])
parseLiteral v xs = first (Literal v . binToDec) <$> getLiteral xs

parseSubpackets :: [Int] -> [Packet]
parseSubpackets xs = case parsePacket xs of
  Just (packet, rest) -> packet : parseSubpackets rest
  Nothing -> []

parseNPackets :: Int -> [Int] -> ([Packet], [Int])
parseNPackets n xs = first reverse . fromJust . (!! n) $ iterate conv $ Just ([], xs)
  where conv = \(Just (packets, xs)) -> first (:packets) <$> parsePacket xs

getLengAndParseMethod :: Int -> (Int, Int -> [Int] -> ([Packet], [Int]))
getLengAndParseMethod 0 = (15, \l xs -> first parseSubpackets $ splitAt l xs)
getLengAndParseMethod 1 = (11, parseNPackets)

parseOperator :: Int -> Int -> [Int] -> Maybe (Packet, [Int])
parseOperator v t [_] = Nothing
parseOperator v t (lengthId : xs) =
    let (len,      parse) = getLengAndParseMethod lengthId
        (count,    rest ) = first binToDec $ splitAt len xs
        (packages, rest') = parse count rest
        op = funcFromType t
    in  Just (Operator v packages op, rest')

parsePacket' :: Int -> Int -> [Int] -> Maybe (Packet, [Int])
parsePacket' _ _ [] = Nothing
parsePacket' v t xs = case t of
    4 -> parseLiteral v xs
    _ -> parseOperator v t xs

parsePacket :: Input -> Maybe (Packet, [Int])
parsePacket xs =
  let (version,  postV) = first binToDec $ splitAt 3 xs
      (pType,    postP) = first binToDec $ splitAt 3 postV
  in parsePacket' version pType postP

part1 :: Input -> Int
part1 = sum . onlyVersions . fst . fromJust . parsePacket
    where onlyVersions (Literal id _) = [id]
          onlyVersions (Operator id packets _) = id : concatMap onlyVersions packets

part2 :: Input -> Int
part2 = calcValue . fst . fromJust . parsePacket
    where calcValue (Literal _ value) = value
          calcValue (Operator _ packets f) = f $ map calcValue packets
