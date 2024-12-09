module Advent.Problems.Day9 (parse, solve) where

import Relude

import Data.Vector (Vector, (!))
import Data.Vector qualified as V

import Advent.Parse (Parser, digitP, eof, manyTill, newline)

parse :: Parser (Vector Block)
parse = go . zip [0 ..] <$> manyTill digitP (newline >> eof)
 where
  go :: [(Int, Int)] -> Vector Block
  go [] = mempty
  go ((i, n) : xs) = V.replicate n (if even i then FileID (i `div` 2) else Empty) <> go xs

data Block = Empty | FileID Int
  deriving (Show)

solve :: Vector Block -> (Int, Int)
solve blocks =
  ( sum $ zipWith (*) [0 ..] $ toList compacted
  , undefined
  )
 where
  compacted = V.unfoldr unfoldCompacted (0, length blocks - 1)

  unfoldCompacted :: (Int, Int) -> Maybe (Int, (Int, Int))
  unfoldCompacted (i, i') =
    if i > i'
      then Nothing
      else case blocks ! i of
        Empty -> case blocks ! i' of
          Empty -> unfoldCompacted (i, i' - 1)
          FileID n -> Just (n, (i + 1, i' - 1))
        FileID n -> Just (n, (i + 1, i'))
