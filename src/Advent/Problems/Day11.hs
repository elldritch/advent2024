module Advent.Problems.Day11 (parse, solve) where

import Relude

import Advent.Math (digits)
import Advent.Parse (Parser, char, eof, intP, newline, sepBy)

parse :: Parser [Int]
parse = sepBy intP (char ' ') <* newline <* eof

solve :: [Int] -> (Int, Int)
solve stones =
  ( length $ head $ fromMaybe (error "impossible: stone iteration is finite") $ nonEmpty $ drop 25 $ iterate blink stones
  , undefined
  )
 where
  blink = concatMap $ \stone ->
    if
      | stone == 0 -> [1]
      | even $ digits @Int stone -> let split = 10 ^ (digits @Int stone `div` 2) in [stone `div` split, stone `mod` split]
      | otherwise -> [stone * 2024]
