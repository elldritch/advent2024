module Main (main) where

import Relude

import Advent.Parse (Parser, anySingle, eof, intP, manyTill, parsePuzzleInput, string, try)

main :: IO ()
main = do
  instructions <- parsePuzzleInput "data/3" $ catMaybes <$> manyTill (try (Just <$> instructionP) <|> (Nothing <$ anySingle)) eof
  putStrLn $ "Part 1: " <> show (sum $ mapMaybe (\case Mul (x, y) -> Just (x * y); _ -> Nothing) instructions)
  putStrLn $ "Part 2: " <> show (fst $ foldl' run (0, True) instructions)
 where
  run :: (Int, Bool) -> Instruction -> (Int, Bool)
  run (n, enabled) = \case
    Enable -> (n, True)
    Disable -> (n, False)
    Mul (x, y) -> if enabled then (n + x * y, enabled) else (n, enabled)

data Instruction = Enable | Disable | Mul (Int, Int)

instructionP :: Parser Instruction
instructionP = Enable <$ string "do()" <|> Disable <$ string "don't()" <|> Mul <$> mulP
 where
  mulP = (,) <$> (string "mul(" *> intP) <* string "," <*> intP <* string ")"
