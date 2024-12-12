module Advent.Problems.Day9 (parse, solve) where

import Relude
import Relude.Extra (bimapBoth)

import Data.Sequence (findIndexL, index)
import Data.Sequence qualified as Seq

import Advent.Parse (Parser, digitP, eof, manyTill, newline)

parse :: forall f. (Applicative f, Monoid (f Segment)) => Parser (f Segment)
parse = go . zip [0 ..] <$> manyTill digitP (newline >> eof)
 where
  go :: [(Int, Int)] -> f Segment
  go [] = mempty
  go ((i, n) : xs) = pure (Segment (if even i then FileID (i `div` 2) else Empty) n) <> go xs

data Block = Empty | FileID Int
  deriving stock (Show, Eq)

data Segment = Segment {block :: Block, length :: Int}
  deriving stock (Show, Eq)

toBlocks :: (Foldable t, Applicative f, Monoid (f Block)) => t Segment -> f Block
toBlocks = foldl' (\acc x -> acc <> if x.length > 0 then stimes x.length (pure x.block) else mempty) mempty

solve :: Seq Segment -> (Int, Int)
solve segments = bimapBoth checksum (blockCompacted, fileCompacted)
 where
  blocks = toBlocks segments

  blockCompacted = unfoldr compactBlocks (0, length blocks - 1)
   where
    compactBlocks (i, i') | i > i' = Nothing
    compactBlocks (i, i') = case blocks `index` i of
      Empty -> case blocks `index` i' of
        Empty -> compactBlocks (i, i' - 1)
        FileID n -> Just (FileID n, (i + 1, i' - 1))
      FileID n -> Just (FileID n, (i + 1, i'))

  fileCompacted = toBlocks $ foldl' compactFile segments $ Seq.reverse segments
   where
    withIndex xs = Seq.zip (Seq.fromList $ take (length xs) [0 ..]) xs

    compactFile :: Seq Segment -> Segment -> Seq Segment
    compactFile segments' segment = case segment of
      Segment Empty _ -> segments'
      Segment (FileID n) l -> case leftmostSpace of
        Nothing -> segments'
        Just (i', s) ->
          if i' < i
            then
              segments'
                & Seq.update i (Segment Empty l)
                & Seq.update i' (Segment (FileID n) l)
                & (let remaining = s.length - l in if remaining > 0 then Seq.insertAt (i' + 1) (Segment Empty remaining) else id)
            else segments'
       where
        -- TODO: To improve performance, avoid re-finding indexes for the
        -- segment every time. Instead, there should be a way to store this and
        -- update whenever a new empty segment is partially consumed. Or perhaps
        -- we should be storing block addresses, or mapping which block
        -- addresses are occupied by which segments?
        i = fromMaybe (error "segment is missing") $ findIndexL (== segment) segments'
        -- TODO: To improve performance, avoid recalculating open spaces for
        -- every segment. Instead, calculate all open spaces the first pass,
        -- likely producing a map of open space size to the list of indexes
        -- where those spaces start. Since new open spaces will never be created
        -- (because the rightmost file is always moved first, and therefore new
        -- open spaces will always be to the right of the next file being
        -- moved), this map will never need to be added to (some spaces will
        -- just need to be removed as they are used).
        leftmostSpace = find (\(_, s) -> case s of Segment Empty l' -> l' >= l; _ -> False) $ withIndex segments'

checksum :: (Foldable t) => t Block -> Int
checksum blocks = snd $ foldl' f (0, 0) blocks
 where
  f (i, acc) (FileID n) = (i + 1, acc + i * n)
  f (i, acc) Empty = (i + 1, acc)
