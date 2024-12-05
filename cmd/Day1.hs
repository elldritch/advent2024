module Main (main) where

import Relude

import Data.Map.Strict qualified as Map

import Advent.Parse (eof, hspace1, intP, newline, parsePuzzleInput, someTill)

main :: IO ()
main = do
  (xs, ys) <- parsePuzzleInput "data/1" $ unzip <$> someTill ((,) <$> intP <* hspace1 <*> intP <* newline) eof
  putStrLn $ "Part 1: " <> show (sum ((\(x, y) -> abs (x - y)) <$> zip (sort xs) (sort ys)))
  let counts = foldl' (\m y -> Map.insertWith (+) y 1 m) mempty ys
      similarities = (\x -> x * fromMaybe 0 (Map.lookup x counts)) <$> xs
  putStrLn $ "Part 2: " <> show (sum similarities)
