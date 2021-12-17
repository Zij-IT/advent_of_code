module D16
  ( format
  , part1
  , part2
  ) where

import Data.Maybe (fromJust)
import Data.Bifunctor (first)

type Input = [Bool]
type Output = Int

data Packet = Literal  { id :: Int, value :: Int }
            | Operator { id :: Int, subpackets :: [Packet], f :: [Int] -> Int }

format :: String -> Input
format = concatMap toBinRep
  where toBinRep c = case c of
           '0' -> [False, False, False, False]; '4' -> [False, True, False, False];
           '1' -> [False, False, False, True];  '5' -> [False, True, False, True];
           '2' -> [False, False, True, False];  '6' -> [False, True, True, False];
           '3' -> [False, False, True, True];   '7' -> [False, True, True, True];
           '8' -> [True, False, False, False];  'C' -> [True, True, False, False]
           '9' -> [True, False, False, True];   'D' -> [True, True, False, True]
           'A' -> [True, False, True, False];   'E' -> [True, True, True, False]
           'B' -> [True, False, True, True];    'F' -> [True, True, True, True]
           _   -> error "BAD INPUT"

funcFromType :: Int -> ([Int] -> Int)
funcFromType t = case t of
  0 -> sum    ; 5 -> \[x, y] -> if x >  y then 1 else 0
  1 -> product; 6 -> \[x, y] -> if x <  y then 1 else 0
  2 -> minimum; 7 -> \[x, y] -> if x == y then 1 else 0
  3 -> maximum; _ -> error "BAD TYPE"

binToDec :: [Bool] -> Int
binToDec = foldl (\acc x -> 2 * acc + if x then 1 else 0) 0

getLiteral :: [Bool] -> Maybe ([Bool], [Bool])
getLiteral lit = case lit of
  (True :xs) -> first (take 4 xs ++) <$> getLiteral (drop 4 xs)
  (False:xs) -> Just $ splitAt 4 xs
  []         -> Nothing

parseLiteral :: Int -> [Bool] -> Maybe (Packet, [Bool])
parseLiteral v xs = first (Literal v . binToDec) <$> getLiteral xs

parseSubpackets :: [Bool] -> [Packet]
parseSubpackets xs = case parsePacket xs of
  Just (packet, rest) -> packet : parseSubpackets rest
  Nothing             -> []

parseNPackets :: Int -> [Bool] -> ([Packet], [Bool])
parseNPackets n xs = first reverse . fromJust . (!! n) $ iterate conv $ Just ([], xs)
  where conv = \(Just (packets, xs)) -> first (:packets) <$> parsePacket xs

getLengAndParseMethod :: Bool -> (Int, Int -> [Bool] -> ([Packet], [Bool]))
getLengAndParseMethod True  = (11, parseNPackets)
getLengAndParseMethod False = (15, \l xs -> first parseSubpackets $ splitAt l xs)

parseOperator :: Int -> Int -> [Bool] -> Maybe (Packet, [Bool])
parseOperator v t (lengthId : xs) =
    let (len,      parse) = getLengAndParseMethod lengthId
        (count,    rest ) = first binToDec $ splitAt len xs
        (packages, rest') = parse count rest
        op = funcFromType t
    in  Just (Operator v packages op, rest')
parseOperator v t _ = Nothing

parsePacket' :: Int -> Int -> [Bool] -> Maybe (Packet, [Bool])
parsePacket' v t xs = case t of
    4 -> parseLiteral v xs
    _ -> parseOperator v t xs

parsePacket :: Input -> Maybe (Packet, [Bool])
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
