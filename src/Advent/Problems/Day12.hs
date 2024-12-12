module Advent.Problems.Day12 (parse, solve) where

import Relude
import Relude.Extra (lookup)

import Algebra.Graph (edges)
import Algebra.Graph.ToGraph (reachable)
import Data.Map.Strict qualified as Map
import Math.Geometry.Grid (Index, indices, neighbour)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import Math.Geometry.Grid.SquareInternal (SquareDirection (..))

import Advent.Parse (Parser, anySingleBut, gridP)

parse :: Parser (Map (Index RectSquareGrid) Char, RectSquareGrid)
parse = gridP rectSquareGrid (anySingleBut '\n')

type RegionID = Int

solve :: (Map (Index RectSquareGrid) Char, RectSquareGrid) -> (Int, Int)
solve (plantMap, grid) = (sum $ uncurry (*) <$> fenceMap, undefined)
 where
  directions = [North, South, West, East]
  plant = fromMaybe (error "position has no plant") . flip lookup plantMap

  regionGraph = edges $ concatMap (catMaybes . (<$> directions) . tileEdge) (indices grid)
   where
    tileEdge pos direction = case neighbour grid pos direction of
      Just pos' -> if plant pos == plant pos' then Just (pos, pos') else Nothing
      Nothing -> Nothing

  regionMap :: Map (Index RectSquareGrid) RegionID
  regionMap = fst $ foldl' addRegion (mempty, 0) $ indices grid
   where
    addRegion (m, i) pos = case lookup pos m of
      Just _ -> (m, i)
      Nothing -> (foldl' (\m' x -> Map.insert x i m') (Map.insert pos i m) $ reachable regionGraph pos, i + 1)

  fenceMap :: Map RegionID (Int, Int)
  fenceMap =
    Map.fromListWith (\(p, a) (p', a') -> (p + p', a + a')) $
      ( \pos ->
          ( fromMaybe (error "position has no region") $ pos `lookup` regionMap
          , (sum $ fencing pos <$> directions, 1)
          )
      )
        <$> indices grid
   where
    fencing pos direction = case neighbour grid pos direction of
      Just pos' -> if plant pos /= plant pos' then 1 else 0
      Nothing -> 1
