module Advent.Problems.Day10 (parse, solve) where

import Relude
import Relude.Extra (lookup)

import Algebra.Graph (edges)
import Algebra.Graph.ToGraph (reachable)
import Math.Geometry.Grid (indices, neighbours)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

import Advent.Parse (Parser, digitP, gridP)

parse :: Parser (Map (Int, Int) Int, RectSquareGrid)
parse = gridP rectSquareGrid digitP

solve :: (Map (Int, Int) Int, RectSquareGrid) -> (Int, Int)
solve (heights, grid) =
  ( sum $ score <$> trailheads
  , undefined
  )
 where
  positions = indices grid
  height p = fromMaybe (error "impossible: position has no height") $ lookup p heights
  trailGraph = edges $ concatMap (concatMap gradualInclines . neighbours grid) positions
   where
    gradualInclines pos = mapMaybe (\pos' -> if height pos' == height pos + 1 then Just (pos, pos') else Nothing) $ neighbours grid pos
  trailheads = filter ((== 0) . height) positions
  score = length . filter ((== 9) . height) . reachable trailGraph
