module Main (main) where

import Relude

import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

import Advent.Parse (Parser, parsePuzzleInput)
import Advent.Problems.Day1 qualified as Day1
import Advent.Problems.Day2 qualified as Day2
import Advent.Problems.Day3 qualified as Day3
import Advent.Problems.Day4 qualified as Day4
import Advent.Problems.Day5 qualified as Day5
import Advent.Problems.Day6 qualified as Day6
import Advent.Problems.Day7 qualified as Day7

main :: IO ()
main = hspec $ do
  describe "Problems" $ do
    day 1 (11, 31) Day1.parse Day1.solve
    day 2 (2, 4) Day2.parse Day2.solve
    day 3 (161, 48) Day3.parse Day3.solve
    day 4 (18, 9) Day4.parse Day4.solve
    day 5 (143, 123) Day5.parse Day5.solve
    day 6 (41, 6) Day6.parse Day6.solve
    day 7 (3749, 11387) Day7.parse Day7.solve
 where
  day :: (Show part1, Show part2, Eq part1, Eq part2) => Int -> (part1, part2) -> Parser input -> (input -> (part1, part2)) -> SpecWith ()
  day n (expected1, expected2) parse solve = it ("Day " <> show n) $ do
    input <- liftIO $ parsePuzzleInput ("test/examples/" <> show n) parse
    solve input `shouldBe` (expected1, expected2)
