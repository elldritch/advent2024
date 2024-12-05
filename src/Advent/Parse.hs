module Advent.Parse (
  parsePuzzleInput,
  intP,
  module Text.Megaparsec.Char,
  module Control.Applicative.Combinators,
  eof,
) where

import Relude

import Control.Applicative.Combinators
import Data.Char (isDigit)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser, takeWhile1P)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parsePuzzleInput :: FilePath -> Parser a -> IO a
parsePuzzleInput fp p = readFileText' fp >>= either (error . toText . errorBundlePretty) pure . runParser p fp
 where
  readFileText' = fmap decodeUtf8 . readFileBS

intP :: Parser Int
intP = takeWhile1P (Just "digit") isDigit >>= mustRead

mustRead :: (MonadFail m, Read a, ToString i) => i -> m a
mustRead input = maybe (fail "intP") pure $ readMaybe $ toString input
