module Advent.NonEmpty (
  module Data.List.NonEmpty,
  concat,
  concatMap,
  ints,
) where

import Relude hiding (concat, concatMap)

import Data.List.NonEmpty

concat :: NonEmpty (NonEmpty a) -> NonEmpty a
concat (xs :| xss) = foldl' (<>) xs xss

concatMap :: (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
concatMap f = concat . fmap f

ints :: (Integral i) => NonEmpty i
ints = 0 :| [1 ..]
