module Advent.Problems.Day2 (parse, solve) where

import Relude

import Advent.Parse (Parser, char, intP, linesP, sepBy)

parse :: Parser [[Int]]
parse = linesP $ sepBy intP (char ' ')

solve :: [[Int]] -> (Int, Int)
solve reports =
  ( length $ filter reportIsSafe reports
  , length $ filter reportIsSafeWithDampening reports
  )

reportIsSafe :: [Int] -> Bool
reportIsSafe report = (allIncreasing || allDecreasing) && allGradual
 where
  deltas = zipWith (-) report $ drop 1 report

  allIncreasing = all (< 0) deltas
  allDecreasing = all (> 0) deltas
  allGradual = all (\x -> let x' = abs x in x' >= 1 && x' <= 3) deltas

reportIsSafeWithDampening :: [Int] -> Bool
reportIsSafeWithDampening report = any reportIsSafe $ report : dampenedReports
 where
  dampenedReports = fmap (report `without`) [0 .. (length report)]
  without xs i = take i xs ++ drop (i + 1) xs
