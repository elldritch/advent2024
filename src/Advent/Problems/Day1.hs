module Advent.Problems.Day1 (parse, solve) where

import Relude

import Data.Map.Strict qualified as Map

import Advent.Parse (Parser, hspace1, intP, linesP)

parse :: Parser ([Int], [Int])
parse = unzip <$> linesP ((,) <$> intP <* hspace1 <*> intP)

solve :: ([Int], [Int]) -> (Int, Int)
solve (xs, ys) =
  ( sum ((\(x, y) -> abs (x - y)) <$> zip (sort xs) (sort ys))
  , sum similarities
  )
 where
  counts = foldl' (\m y -> Map.insertWith (+) y 1 m) mempty ys
  similarities = (\x -> x * fromMaybe 0 (Map.lookup x counts)) <$> xs
