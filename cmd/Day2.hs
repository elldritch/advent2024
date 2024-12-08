module Main (main) where

import Relude

import Advent.Parse (char, intP, parsePuzzleInputLines, sepBy)

main :: IO ()
main = do
  reports <- parsePuzzleInputLines "data/2" $ sepBy intP (char ' ')
  putStrLn $ "Part 1: " <> show (length $ filter reportIsSafe reports)
  putStrLn $ "Part 2: " <> show (length $ filter reportIsSafeWithDampening reports)

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
