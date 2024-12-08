module Main (main) where

import Relude

import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, short)

import Advent.Parse (parsePuzzleInput)
import Advent.Problems.Day1 qualified as Day1
import Advent.Problems.Day2 qualified as Day2
import Advent.Problems.Day3 qualified as Day3
import Advent.Problems.Day4 qualified as Day4
import Advent.Problems.Day5 qualified as Day5
import Advent.Problems.Day6 qualified as Day6
import Advent.Problems.Day7 qualified as Day7
import Advent.Problems.Day8 qualified as Day8

newtype Argv = Argv {day :: Int}

argvP :: Parser Argv
argvP = Argv <$> option auto (long "day" <> short 'd' <> metavar "DAY" <> help "Day to run")

argparser :: ParserInfo Argv
argparser = info (argvP <**> helper) (fullDesc <> progDesc "Advent of Code 2024")

main :: IO ()
main = do
  opts <- execParser argparser
  case opts.day of
    1 -> parsePuzzleInput "data/1" Day1.parse >>= output . Day1.solve
    2 -> parsePuzzleInput "data/2" Day2.parse >>= output . Day2.solve
    3 -> parsePuzzleInput "data/3" Day3.parse >>= output . Day3.solve
    4 -> parsePuzzleInput "data/4" Day4.parse >>= output . Day4.solve
    5 -> parsePuzzleInput "data/5" Day5.parse >>= output . Day5.solve
    6 -> parsePuzzleInput "data/6" Day6.parse >>= output . Day6.solve
    7 -> parsePuzzleInput "data/7" Day7.parse >>= output . Day7.solve
    8 -> parsePuzzleInput "data/8" Day8.parse >>= output . Day8.solve
    _ -> putStrLn "Day not implemented" >> exitFailure
 where
  output (p1, p2) = do
    putStrLn $ "Part 1: " <> show p1
    putStrLn $ "Part 2: " <> show p2
