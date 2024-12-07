module Main (main) where

import Relude

import Advent.Parse (char, intP, parsePuzzleInputLines, sepBy1', string)

main :: IO ()
main = do
  equations <- parsePuzzleInputLines "data/7" $ (,) <$> intP <* string ": " <*> sepBy1' intP (char ' ')
  putStrLn $ "Part 1: " <> show (sum $ fst <$> filter (uncurry satisfiable) equations)

satisfiable :: Int -> NonEmpty Int -> Bool
satisfiable target operands = elem target $ go (head operands) $ tail operands
 where
  go :: Int -> [Int] -> [Int]
  go acc [] = [acc]
  go acc (op : ops) = go (acc + op) ops ++ go (acc * op) ops
