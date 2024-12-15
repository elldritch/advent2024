module Advent.Problems.Day9 (parse, solve) where

import Relude
import Relude.Extra (bimapBoth, insert, insertWith, toPairs, (!?))

import Data.Map.Strict qualified as Map
import Data.Sequence (index)
import Data.Sequence qualified as Seq

import Advent.Parse (Parser, digitP, eof, manyTill, newline)

parse :: forall f. (Applicative f, Monoid (f Segment)) => Parser (f Segment)
parse = go . zip [0 ..] <$> manyTill digitP (newline >> eof)
 where
  go :: [(Int, Int)] -> f Segment
  go [] = mempty
  go ((i, n) : xs) = pure (Segment (if even i then FileID (i `div` 2) else Empty) n) <> go xs

type FileID = Int

data Block = Empty | FileID FileID
  deriving stock (Show, Eq)

type Address = Int

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

  fileCompacted = toList $ fst $ foldl' compactFile (blocks, emptySpaces) $ Seq.reverse segments
   where
    emptySpaces :: Map Int [Address]
    emptySpaces = fst $ foldl' f (mempty, 0) segments
     where
      f (m, i) (Segment block len) = case block of
        FileID _ -> (m, i + len)
        Empty -> (insertWith (flip (<>)) len [i] m, i + len)

    segmentToBlocks :: Map FileID Address
    segmentToBlocks = fst $ foldl' f (mempty, 0) segments
     where
      f (m, i) (Segment block len) = case block of
        FileID n -> (insert n i m, i + len)
        Empty -> (m, i + len)

    compactFile :: (Seq Block, Map Int [Address]) -> Segment -> (Seq Block, Map Int [Address])
    compactFile (bs, empties) (Segment block len) = case block of
      Empty -> (bs, empties)
      FileID n -> case closestEmpty of
        Nothing -> (bs, empties)
        Just (_, []) -> (bs, empties)
        Just (l, addr : addrs) -> if addr > oldBlockAddr then (bs, empties) else (bs'', empties')
         where
          oldBlockAddr = fromMaybe (error "unknown segment") $ segmentToBlocks !? n
          bs' = foldl' (\b i -> Seq.update (oldBlockAddr + i) Empty b) bs [0 .. len - 1]
          bs'' = foldl' (\b i -> Seq.update (addr + i) (FileID n) b) bs' [0 .. len - 1]
          empties' = insert l addrs empties & (if l > len then insertWith (\new old -> sort $ new <> old) (l - len) [addr + len] else id)
       where
        closestEmpty :: Maybe (Int, [Address])
        closestEmpty = viaNonEmpty head $ sortBy sortSpace $ toPairs $ Map.filterWithKey (\k _ -> k >= len) empties
         where
          sortSpace (_, as) (_, bs') = case (viaNonEmpty head as, viaNonEmpty head bs') of
            (Nothing, Nothing) -> EQ
            (Nothing, Just _) -> GT
            (Just _, Nothing) -> LT
            (Just a', Just b') -> compare a' b'

checksum :: (Foldable t) => t Block -> Int
checksum blocks = snd $ foldl' f (0, 0) blocks
 where
  f (i, acc) (FileID n) = (i + 1, acc + i * n)
  f (i, acc) Empty = (i + 1, acc)
