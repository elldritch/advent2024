module Advent.Problems.Day10 (parse, solve) where

import Relude
import Relude.Extra (bimapBoth, lookup)

import Algebra.Graph (edges)
import Algebra.Graph.ToGraph (postSet, reachable)
import Math.Geometry.Grid (indices, neighbours)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

import Advent.Parse (Parser, digitP, gridP)

parse :: Parser (Map (Int, Int) Int, RectSquareGrid)
parse = gridP rectSquareGrid digitP

solve :: (Map (Int, Int) Int, RectSquareGrid) -> (Int, Int)
solve (heights, grid) = bimapBoth sum (score <$> trailheads, rating <$> trailheads)
 where
  positions = indices grid
  height p = fromMaybe (error "impossible: position has no height") $ lookup p heights
  slopeGraph = edges $ concatMap (concatMap gradualInclines . neighbours grid) positions
   where
    gradualInclines pos = mapMaybe (\pos' -> if height pos' == height pos + 1 then Just (pos, pos') else Nothing) $ neighbours grid pos
  trailheads = filter ((== 0) . height) positions

  score = length . filter ((== 9) . height) . reachable slopeGraph

  -- TODO: This basically brute-forces the paths. To improve performance, do a
  -- topsort and track the number of paths through each node.
  rating = pathsToPeak
   where
    pathsToPeak pos = if height pos == 9 then 1 else sum $ fmap pathsToPeak $ toList $ postSet pos slopeGraph
