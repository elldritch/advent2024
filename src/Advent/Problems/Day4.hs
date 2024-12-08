module Advent.Problems.Day4 (parse, solve) where

import Relude
import Relude.Extra (lookup)

import Math.Geometry.Grid (Direction, Grid, Index, indices, neighbour)
import Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)
import Math.Geometry.Grid.OctagonalInternal (OctDirection (..))

import Advent.Parse (Parser, gridP, oneOf)

parse :: Parser (Map (Index RectOctGrid) Char, RectOctGrid)
parse = gridP rectOctGrid (oneOf ['X', 'M', 'A', 'S'])

solve :: (Map (Index RectOctGrid) Char, RectOctGrid) -> (Int, Int)
solve (wordMap, grid) =
  ( length $ filter id finds
  , length $ filter id $ xmasInSquare wordMap <$> indices grid
  )
 where
  directions = [West, Northwest, North, Northeast, East, Southeast, South, Southwest]
  finds = concatMap (\loc -> fmap (xmasInDirection wordMap grid loc) directions) $ indices grid

xmasInDirection :: forall g. (Grid g, Ord (Index g), Eq (Direction g)) => Map (Index g) Char -> g -> Index g -> Direction g -> Bool
xmasInDirection wordMap grid position direction = Just True == go position "XMAS"
 where
  go :: Index g -> String -> Maybe Bool
  go _ "" = error "unreachable: word search terminates with last character remaining"
  go loc [c] = (== c) <$> lookup loc wordMap
  go loc (c : cs) = do
    c' <- lookup loc wordMap
    guard $ c == c'
    loc' <- neighbour grid loc direction
    go loc' cs

xmasInSquare :: Map (Int, Int) Char -> (Int, Int) -> Bool
xmasInSquare wordMap (x, y) =
  Just True
    == ( do
          topLeft <- lookup (x, y) wordMap
          topRight <- lookup (x + 2, y) wordMap
          center <- lookup (x + 1, y + 1) wordMap
          bottomLeft <- lookup (x, y + 2) wordMap
          bottomRight <- lookup (x + 2, y + 2) wordMap
          pure $
            center == 'A'
              && ((topLeft == 'M' && bottomRight == 'S') || (topLeft == 'S' && bottomRight == 'M'))
              && ((topRight == 'M' && bottomLeft == 'S') || (topRight == 'S' && bottomLeft == 'M'))
       )
