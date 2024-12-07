module Main (main) where

import Relude

import Advent.Parse (char, intP, parsePuzzleInputLines, sepBy1', string)

main :: IO ()
main = do
  equations <- parsePuzzleInputLines "data/7" $ (,) <$> intP <* string ": " <*> sepBy1' intP (char ' ')
  let calibrateWith f = sum $ fst <$> filter (uncurry f) equations
  putStrLn $ "Part 1: " <> show (calibrateWith satisfiable)
  putStrLn $ "Part 1: " <> show (calibrateWith satisfiable')

satisfiable :: Int -> NonEmpty Int -> Bool
satisfiable = satisfiableWith [(+), (*)]

satisfiable' :: Int -> NonEmpty Int -> Bool
satisfiable' = satisfiableWith [(+), (*), digitConcat]
 where
  digitConcat :: Int -> Int -> Int
  digitConcat x y = x * (10 ^ (floor @Double @Int (logBase 10 (fromIntegral y)) + 1)) + y

satisfiableWith :: [Int -> Int -> Int] -> Int -> NonEmpty Int -> Bool
satisfiableWith fs target operands = elem target $ go (head operands) $ tail operands
 where
  go :: Int -> [Int] -> [Int]
  go acc [] = [acc]
  go acc (op : ops) = concatMap (\f -> go (f acc op) ops) fs
