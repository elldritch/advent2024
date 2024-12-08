module Advent.Problems.Day8 (parse, solve) where

import Relude
import Relude.Extra (lookup, toPairs)

import Data.List ((\\))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Math.Geometry.Grid (Index, contains)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

import Advent.Parse (Parser, char, gridP, noneOf)

parse :: Parser (Map (Index RectSquareGrid) Tile, RectSquareGrid)
parse = gridP rectSquareGrid tileP

data Tile = Empty | Antenna Char
  deriving stock (Show, Eq)

tileP :: Parser Tile
tileP = Empty <$ char '.' <|> Antenna <$> noneOf ['.', '\n']

solve :: (Map (Index RectSquareGrid) Tile, RectSquareGrid) -> (Int, Int)
solve (tileMap, grid) =
  ( length $ Set.filter (grid `contains`) antinodes
  , undefined
  )
 where
  antennae = mapMaybe (\case ((x, y), Antenna c) -> Just ((x, y), c); _ -> Nothing) $ toPairs tileMap
  antennaeByFrequency = Map.fromListWith (<>) $ second (one @[(Int, Int)]) . swap <$> antennae
  antinodesOf ((x, y), freq) = concatMap antinodeWithPartner partners
   where
    partners = fromMaybe mempty $ lookup freq antennaeByFrequency
    antinodeWithPartner (x', y') = [(x + dx, y + dy), (x - dx, y - dy)] \\ [(x, y), (x', y')]
     where
      dx = x - x'
      dy = y - y'
  antinodes = Set.fromList (concatMap antinodesOf antennae)
