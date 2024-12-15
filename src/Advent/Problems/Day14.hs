module Advent.Problems.Day14 (parse, solve, solve') where

import Relude
import Relude.Extra (bimapBoth)

import Data.List (partition)

import Advent.Parse (Parser, char, intP, linesP, string)

type Robot = ((Int, Int), (Int, Int))

parse :: Parser [Robot]
parse = linesP $ (,) <$> ((,) <$> (string "p=" *> intP <* char ',') <*> intP) <* char ' ' <*> ((,) <$> (string "v=" *> intP <* char ',') <*> intP)

solve :: [Robot] -> (Int, Int)
solve = solve' (101, 103)

solve' :: (Int, Int) -> [Robot] -> (Int, Int)
solve' (width, height) start = (product (length <$> [q1, q2, q3, q4]), undefined)
 where
  iterations = 100
  endingPositions = fmap (\((x, y), (vx, vy)) -> ((x + vx * iterations) `mod` width, (y + vy * iterations) `mod` height)) start
  ((q1, q2), (q3, q4)) =
    bimapBoth (partition ((< height `div` 2) . snd))
      . partition ((< width `div` 2) . fst)
      $ filter (\(x, y) -> x /= (width `div` 2) && y /= (height `div` 2)) endingPositions
