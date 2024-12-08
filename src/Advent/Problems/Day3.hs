module Advent.Problems.Day3 (parse, solve) where

import Relude

import Advent.Parse (Parser, anySingle, eof, intP, manyTill, string, try)

parse :: Parser [Instruction]
parse = catMaybes <$> manyTill (try (Just <$> instructionP) <|> Nothing <$ anySingle) eof

data Instruction = Enable | Disable | Mul (Int, Int)

instructionP :: Parser Instruction
instructionP = Enable <$ string "do()" <|> Disable <$ string "don't()" <|> Mul <$> mulP
 where
  mulP = (,) <$> (string "mul(" *> intP) <* string "," <*> intP <* string ")"

solve :: [Instruction] -> (Int, Int)
solve instructions =
  ( sum $ mapMaybe (\case Mul (x, y) -> Just (x * y); _ -> Nothing) instructions
  , fst $ foldl' run (0, True) instructions
  )
 where
  run :: (Int, Bool) -> Instruction -> (Int, Bool)
  run (n, enabled) = \case
    Enable -> (n, True)
    Disable -> (n, False)
    Mul (x, y) -> if enabled then (n + x * y, enabled) else (n, enabled)
