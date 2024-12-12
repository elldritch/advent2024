module Main (main) where

import Relude
import Relude.Extra (lookup, toPairs)

import Data.Time (diffUTCTime, getCurrentTime)
import Options.Applicative (auto, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, short)
import Options.Applicative qualified as Arg (Parser, ParserInfo)

import Advent.Parse (readPuzzleInput, runPuzzle)
import Advent.Problems.Day1 qualified as Day1
import Advent.Problems.Day10 qualified as Day10
import Advent.Problems.Day11 qualified as Day11
import Advent.Problems.Day12 qualified as Day12
import Advent.Problems.Day2 qualified as Day2
import Advent.Problems.Day3 qualified as Day3
import Advent.Problems.Day4 qualified as Day4
import Advent.Problems.Day5 qualified as Day5
import Advent.Problems.Day6 qualified as Day6
import Advent.Problems.Day7 qualified as Day7
import Advent.Problems.Day8 qualified as Day8
import Advent.Problems.Day9 qualified as Day9

newtype Argv = Argv {day :: Maybe Int}

argvP :: Arg.Parser Argv
argvP = Argv <$> optional (option auto (long "day" <> short 'd' <> metavar "DAY" <> help "Day to run"))

argparser :: Arg.ParserInfo Argv
argparser = info (argvP <**> helper) (fullDesc <> progDesc "Advent of Code 2024")

main :: IO ()
main = do
  opts <- execParser argparser
  hSetBuffering stdout LineBuffering
  case opts.day of
    Just n -> maybe (putStrLn "Day not implemented") (run n) $ lookup n solutions
    Nothing -> traverse_ (uncurry run) $ toPairs solutions
 where
  run :: Int -> (Text -> Either String (Int, Int)) -> IO ()
  run n f = do
    input <- readPuzzleInput ("data/" <> show n)
    let (p1, p2) = either (error . toText) id $ f input
    putStrLn $ "Day " <> show n
    timed $ "  Part 1: " <> show p1
    timed $ "  Part 2: " <> show p2
    putStrLn ""
   where
    timed s = do
      start <- getCurrentTime
      putStr s
      end <- getCurrentTime
      putStrLn $ " (" <> show (diffUTCTime end start) <> ")"

  solutions :: Map Int (Text -> Either String (Int, Int))
  solutions = fromList $ zip [1 ..] days
   where
    days =
      [ runPuzzle Day1.parse Day1.solve
      , runPuzzle Day2.parse Day2.solve
      , runPuzzle Day3.parse Day3.solve
      , runPuzzle Day4.parse Day4.solve
      , runPuzzle Day5.parse Day5.solve
      , runPuzzle Day6.parse Day6.solve
      , runPuzzle Day7.parse Day7.solve
      , runPuzzle Day8.parse Day8.solve
      , runPuzzle Day9.parse Day9.solve
      , runPuzzle Day10.parse Day10.solve
      , runPuzzle Day11.parse Day11.solve
      , runPuzzle Day12.parse Day12.solve
      ]
