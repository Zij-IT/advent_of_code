module D14
  ( format
  , part1
  , part2
  ) where

import qualified Data.Map.Strict as M
import Data.List (sort)

type Polymer = M.Map (Char, Char) Int
type Rules = M.Map (Char, Char) Char

type Input = (Polymer, Rules)
type Output = Int

-- Parsing Logic
toPolymer :: String -> Polymer
toPolymer (x:y:rest) = M.insertWith (+) (x, y) 1 $ toPolymer (y:rest)
toPolymer _ = M.empty

format :: String -> Input
format str =
  let polymer = toPolymer $ head $ lines str
      toTuple = \[x, y] -> (x, y)
      rules = M.fromList $ map (\line -> (toTuple $ take 2 line, last line)) $ drop 2 $ lines str
  in (polymer, rules)

-- Solving Logic
polymerFrequency :: Polymer -> M.Map Char Int
polymerFrequency = M.map ((`quot` 2) . (+1)) . M.fromListWith (+) . concatMap splitCount . M.toList
  where splitCount ((x, y), count) = [(x, count), (y, count)]

mutate :: Rules -> Polymer -> Polymer
mutate rules = M.fromListWith (+) . concatMap f . M.toList
  where
    f (pair@(x, y), count) = case (M.!?) rules pair of
      Just k -> [((x, k), count), ((k, y), count)]
      Nothing -> [(pair, count)]

solve :: Input -> Int -> Output
solve (polymer, rules) n =
  let poly = (!! n) $ iterate (mutate rules) polymer
      freq = sort . M.elems $ polymerFrequency poly
  in last freq - head freq

part1 :: Input -> Output
part1 input = solve input 10

part2 :: Input -> Output
part2 input = solve input 40
