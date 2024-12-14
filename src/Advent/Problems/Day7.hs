module Advent.Problems.Day7 (parse, solve) where

import Relude

import Advent.Math (digits)
import Advent.Parse (Parser, char, intP, linesP, sepBy1, string)

parse :: Parser [(Int, NonEmpty Int)]
parse = linesP $ (,) <$> intP <* string ": " <*> sepBy1 intP (char ' ')

solve :: [(Int, NonEmpty Int)] -> (Int, Int)
solve equations =
  ( calibrateWith satisfiable
  , calibrateWith satisfiable'
  )
 where
  calibrateWith f = sum $ fst <$> filter (uncurry f) equations

satisfiable :: Int -> NonEmpty Int -> Bool
satisfiable = satisfiableWith [(+), (*)]

satisfiable' :: Int -> NonEmpty Int -> Bool
satisfiable' = satisfiableWith [(+), (*), digitConcat]
 where
  digitConcat :: Int -> Int -> Int
  digitConcat x y = x * 10 ^ digits @Int y + y

satisfiableWith :: [Int -> Int -> Int] -> Int -> NonEmpty Int -> Bool
satisfiableWith fs target operands = elem target $ go (head operands) $ tail operands
 where
  go :: Int -> [Int] -> [Int]
  go acc [] = [acc]
  go acc _ | acc > target = []
  go acc (op : ops) = concatMap (\f -> go (f acc op) ops) fs
