module Advent.Problems.Day11 (parse, solve) where

import Relude
import Relude.Extra (elems, toPairs)

import Data.Map.Strict qualified as Map

import Advent.Math (digits)
import Advent.Parse (Parser, char, eof, intP, newline, sepBy)

parse :: Parser [Int]
parse = sepBy intP (char ' ') <* newline <* eof

solve :: [Int] -> (Int, Int)
solve initial = (stoneCountAfter 25 initial, stoneCountAfter 75 initial)
 where
  stoneCountAfter i stones =
    sum
      . elems
      . head
      . fromMaybe (error "stone iteration is finite")
      . nonEmpty
      . drop i
      . iterate blink'
      $ Map.fromListWith (+)
      $ (,1) <$> stones

  blink' counts =
    Map.fromListWith (+)
      . concatMap (\(stone, count) -> [(stone', count) | stone' <- blink stone])
      $ toPairs counts

  blink stone
    | stone == 0 = [1]
    | even $ digits @Int stone = let split = 10 ^ (digits @Int stone `div` 2) in [stone `div` split, stone `mod` split]
    | otherwise = [stone * 2024]
