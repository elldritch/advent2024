module Advent.Problems.Day8 (parse, solve) where

import Relude
import Relude.Extra (bimapBoth, toPairs, (!?))

import Data.List ((\\))
import Data.Map.Strict qualified as Map
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
solve (tileMap, grid) = bimapBoth (length . antinodesWith) (fundamentalAntinodes, harmonicAntinodes)
 where
  antennae = mapMaybe (\case ((x, y), Antenna c) -> Just ((x, y), c); _ -> Nothing) $ toPairs tileMap
  antennaeByFrequency = Map.fromListWith (<>) $ second one . swap <$> antennae

  antennaAntinodesWith generator (pos, freq) =
    concatMap
      (takeWhile (grid `contains`) . generator pos)
      $ fromMaybe mempty (antennaeByFrequency !? freq) \\ [pos]
  antinodesWith generator = sortNub $ concatMap (antennaAntinodesWith generator) antennae

  fundamentalAntinodes = allAntinodes [2]
  harmonicAntinodes = allAntinodes (0 : [2 ..])
  allAntinodes harmonics (x, y) (x', y') = [(x + i * dx, y + i * dy) | i <- harmonics]
   where
    dx = x' - x
    dy = y' - y
