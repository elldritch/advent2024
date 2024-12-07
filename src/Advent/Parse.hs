module Advent.Parse (
  Parser,
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  module Text.Megaparsec.Debug,
  sepBy1',
  parsePuzzleInput,
  parsePuzzleInputLines,
  intP,
) where

import Relude

import Control.Monad.Combinators.NonEmpty qualified as NE (sepBy1)
import Data.Char (isDigit)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

parsePuzzleInput :: FilePath -> Parser a -> IO a
parsePuzzleInput fp p = readFileText' fp >>= either (error . toText . errorBundlePretty) pure . runParser p fp
 where
  readFileText' = fmap decodeUtf8 . readFileBS

parsePuzzleInputLines :: FilePath -> Parser a -> IO [a]
parsePuzzleInputLines fp lineP = parsePuzzleInput fp $ someTill (lineP <* newline) eof

intP :: Parser Int
intP = takeWhile1P (Just "digit") isDigit >>= mustRead

mustRead :: (MonadFail m, Read a, ToString i) => i -> m a
mustRead input = maybe (fail "intP") pure $ readMaybe $ toString input

sepBy1' :: (MonadPlus m) => m a -> m sep -> m (NonEmpty a)
sepBy1' = NE.sepBy1
