module Advent.Problems.Day10 (parse, solve) where

import Relude
import Relude.Extra (bimapBoth, lookup)

import Algebra.Graph (edges)
import Algebra.Graph.ToGraph (postSet, reachable, topSort)
import Data.Map.Strict qualified as Map
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

  -- FIXME: For some reason, this takes about a minute normally (under O1 and
  -- O2) and takes forever on O0, but takes only 0.1 seconds if you turn on
  -- profiling!
  --
  -- After further testing, the full set of combinations is:
  --
  -- - O0, profiling off: doesn't finish
  -- - O0, profiling on: doesn't finish
  -- - O1, profiling off: ~1 minute
  -- - O1, profiling on: ~0.1 seconds
  -- - O2, profiling off: ~1 minute
  -- - O2, profiling on: ~0.1 seconds
  --
  -- This is extremely strange, especially since all the other solutions take
  -- about 2x to 3x as long when profiling is on. It seems impossible, but @cnr
  -- and I have both reproduced it.
  rating :: (Int, Int) -> Int
  rating pos = fromMaybe 0 $ Map.lookup pos ratings
   where
    topSorted :: [(Int, Int)]
    topSorted = fromRight (error "impossible: graph is cyclic") $ topSort slopeGraph

    ratings :: Map (Int, Int) Int
    ratings = foldr pathsTo mempty topSorted
     where
      pathsTo :: (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
      pathsTo pos' m
        | height pos' == 9 = Map.insert pos' 1 m
        | otherwise = Map.insert pos' (sum $ fromMaybe (error "impossible: position is not on a trail") . flip lookup m <$> toList (postSet pos' slopeGraph)) m
