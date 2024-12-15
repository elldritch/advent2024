module Advent.Problems.Day10 (parse, solve) where

import Relude
import Relude.Extra (bimapBoth, insert, (!?))

import Algebra.Graph (edges)
import Algebra.Graph.ToGraph (postSet, reachable, toAdjacencyMap, topSort)
import Math.Geometry.Grid (indices, neighbours)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

import Advent.Parse (Parser, digitP, gridP)

parse :: Parser (Map (Int, Int) Int, RectSquareGrid)
parse = gridP rectSquareGrid digitP

solve :: (Map (Int, Int) Int, RectSquareGrid) -> (Int, Int)
solve (heights, grid) = bimapBoth sum (score <$> trailheads, rating <$> trailheads)
 where
  positions = indices grid
  height p = fromMaybe (error "position has no height") $ heights !? p
  slopeGraph = edges $ concatMap (concatMap gradualInclines . neighbours grid) positions
   where
    gradualInclines pos = mapMaybe (\pos' -> if height pos' == height pos + 1 then Just (pos, pos') else Nothing) $ neighbours grid pos
  trailheads = filter ((== 0) . height) positions

  score = length . filter ((== 9) . height) . reachable slopeGraph

  rating :: (Int, Int) -> Int
  rating pos = fromMaybe 0 $ ratings !? pos
   where
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/25573#note_599887
    adj = toAdjacencyMap slopeGraph

    topSorted :: [(Int, Int)]
    topSorted = fromRight (error "graph is cyclic") $ topSort adj

    ratings :: Map (Int, Int) Int
    ratings = foldr pathsTo mempty topSorted
     where
      pathsTo :: (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
      pathsTo pos' m
        | height pos' == 9 = insert pos' 1 m
        | otherwise = insert pos' (sum $ fromMaybe (error "position is not on a trail") . (m !?) <$> toList (postSet pos' adj)) m
