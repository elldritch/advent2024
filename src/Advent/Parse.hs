module Advent.Parse (
  Parser,
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  module Text.Megaparsec.Debug,
  module Control.Monad.Combinators.NonEmpty,
  parsePuzzleInput,
  linesP,
  gridP,
  intP,
  digitP,
) where

import Relude
import Relude.Extra (maximum1)

import Control.Monad.Combinators.NonEmpty
import Data.Char (isDigit)
import Text.Megaparsec hiding (endBy1, sepBy1, sepEndBy1, some, someTill)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

import Advent.NonEmpty qualified as NE

type Parser = Parsec Void Text

parsePuzzleInput :: FilePath -> Parser a -> IO a
parsePuzzleInput fp p = readFileText' fp >>= either (error . toText . errorBundlePretty) pure . runParser p fp
 where
  readFileText' = fmap decodeUtf8 . readFileBS

linesP :: Parser a -> Parser [a]
linesP lineP = manyTill (lineP <* newline) eof

gridP :: (Int -> Int -> grid) -> Parser tile -> Parser (Map (Int, Int) tile, grid)
gridP makeGrid tileP = do
  tiles <- NE.zip NE.ints <$> someTill (NE.zip NE.ints <$> someTill tileP newline) eof
  let tiles' = NE.concatMap (\(y, row) -> fmap (\(x, c) -> ((x, y), c)) row) tiles
      coords = fst <$> tiles'
      maxX = maximum1 $ fst <$> coords
      maxY = maximum1 $ snd <$> coords
      tiles'' = first (second (maxY -)) <$> toList tiles'
      grid = makeGrid (maxX + 1) (maxY + 1)
  pure (fromList $ toList tiles'', grid)

intP :: Parser Int
intP = takeWhile1P (Just "digit") isDigit >>= mustRead

digitP :: Parser Int
digitP = digitChar >>= mustRead . one @String

mustRead :: (MonadFail m, Read a, ToString i) => i -> m a
mustRead input = maybe (fail "intP") pure $ readMaybe $ toString input
