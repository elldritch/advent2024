module Main (main) where

import Relude

import Advent.Parse (char, eof, intP, newline, parsePuzzleInput, sepBy1, someTill)

main :: IO ()
main = do
  reports <- parsePuzzleInput "data/2" $ someTill (sepBy1 intP (char ' ') <* newline) eof
  putStrLn $ "Part 1: " <> show (length $ filter reportIsSafe reports)

reportIsSafe :: [Int] -> Bool
reportIsSafe report = (allIncreasing || allDecreasing) && allGradual
 where
  deltas = zipWith (-) report $ drop 1 report

  allIncreasing = all (< 0) deltas
  allDecreasing = all (> 0) deltas
  allGradual = all (\x -> let x' = abs x in x' >= 1 && x' <= 3) deltas
