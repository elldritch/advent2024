module Advent.Problems.Day13 (parse, solve) where

import Relude

import GHC.Real ((%))

import Advent.Parse (Parser, char, eof, intP, newline, sepBy, string)

data Button = Button {dx :: Rational, dy :: Rational}
  deriving stock (Show)

data ClawMachine = ClawMachine {a :: Button, b :: Button, prize :: (Rational, Rational)}
  deriving stock (Show)

parse :: Parser [ClawMachine]
parse = sepBy clawMachineP newline <* eof
 where
  ratP :: Parser Rational
  ratP = (% 1) . toInteger <$> intP

  clawMachineP :: Parser ClawMachine
  clawMachineP = ClawMachine <$> buttonP (char 'A') <*> buttonP (char 'B') <*> prizeP

  buttonP :: Parser Char -> Parser Button
  buttonP charP = Button <$> (string "Button " *> charP *> string ": X+" *> ratP) <* string ", Y+" <*> ratP <* newline

  prizeP :: Parser (Rational, Rational)
  prizeP = (,) <$> (string "Prize: X=" *> ratP) <* string ", Y=" <*> ratP <* newline

solve :: [ClawMachine] -> (Int, Int)
solve machines =
  ( sum $ cost . solveMachine <$> machines
  , undefined
  )
 where
  solveMachine ClawMachine{a, b, prize} =
    ( (fst prize - (snd prize - fst prize * a.dy / a.dx) / (b.dy - b.dx * a.dy / a.dx) * b.dx) / a.dx
    , (snd prize - fst prize * a.dy / a.dx) / (b.dy - b.dx * a.dy / a.dx)
    )

  cost :: (Rational, Rational) -> Int
  cost (a, b) = if denominator a /= 1 || denominator b /= 1 then 0 else 3 * truncate a + truncate b
