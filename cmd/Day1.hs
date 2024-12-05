module Main (main) where

import Relude

import Advent.Parse (eof, hspace1, intP, newline, parsePuzzleInput, someTill)

main :: IO ()
main = do
  (xs, ys) <- parsePuzzleInput "data/1" $ unzip <$> someTill ((,) <$> intP <* hspace1 <*> intP <* newline) eof
  print $ sum ((\(x, y) -> abs (x - y)) <$> zip (sort xs) (sort ys))
