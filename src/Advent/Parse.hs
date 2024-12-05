module Advent.Parse (
  parsePuzzleInput,
  intP,
  module Text.Megaparsec.Char,
  module Control.Applicative.Combinators,
  eof,
) where

import Relude

import Control.Applicative.Combinators (someTill)
import Data.Char (isDigit)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser, takeWhile1P)
import Text.Megaparsec.Char (hspace1, newline)

type Parser = Parsec Void Text

parsePuzzleInput :: FilePath -> Parser a -> IO a
parsePuzzleInput fp p = do
  contents <- decodeUtf8 <$> readFileBS fp
  case runParser p fp contents of
    Left err -> error $ toText $ errorBundlePretty err
    Right a -> pure a

intP :: Parser Int
intP = takeWhile1P (Just "digit") isDigit >>= mustRead

mustRead :: (MonadFail m, Read a, ToString i) => i -> m a
mustRead input = case readMaybe $ toString input of
  Just value -> pure value
  Nothing -> fail "intP"
