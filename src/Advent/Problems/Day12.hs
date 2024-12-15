module Advent.Problems.Day12 (parse, solve) where

import Relude
import Relude.Extra (bimapBoth, insert, (!?))

import Algebra.Graph (edges)
import Algebra.Graph.ToGraph (reachable)
import Data.Map.Strict qualified as Map
import Math.Geometry.Grid (Index, indices, neighbour)
import Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)
import Math.Geometry.Grid.OctagonalInternal (OctDirection (..))

import Advent.Parse (Parser, anySingleBut, gridP)

parse :: Parser (Map (Index RectOctGrid) Char, RectOctGrid)
parse = gridP rectOctGrid (anySingleBut '\n')

type RegionID = Int

solve :: (Map (Index RectOctGrid) Char, RectOctGrid) -> (Int, Int)
solve (plantMap, grid) = bimapBoth (sum . (uncurry (*) <$>) . fenceMap) (perimeter, corners)
 where
  squareDirections = [North, South, West, East]
  plant = fromMaybe (error "position has no plant") . (plantMap !?)

  regionGraph = edges $ concatMap (catMaybes . (<$> squareDirections) . tileEdge) (indices grid)
   where
    tileEdge pos direction = case neighbour grid pos direction of
      Just pos' -> if plant pos == plant pos' then Just (pos, pos') else Nothing
      Nothing -> Nothing

  regionMap :: Map (Index RectOctGrid) RegionID
  regionMap = fst $ foldl' addRegion (mempty, 0) $ indices grid
   where
    addRegion (m, i) pos = case m !? pos of
      Just _ -> (m, i)
      Nothing -> (foldl' (\m' x -> insert x i m') (insert pos i m) $ reachable regionGraph pos, i + 1)

  fenceMap :: (Index RectOctGrid -> Int) -> Map RegionID (Int, Int)
  fenceMap cost =
    Map.fromListWith (\(a, p) (a', p') -> (a + a', p + p')) $
      (\pos -> (fromMaybe (error "position has no region") $ regionMap !? pos, (1, cost pos)))
        <$> indices grid

  perimeter :: Index RectOctGrid -> Int
  perimeter pos = sum $ sideFenced <$> squareDirections
   where
    sideFenced direction = case neighbour grid pos direction of
      Just pos' -> if plant pos /= plant pos' then 1 else 0
      Nothing -> 1

  corners :: Index RectOctGrid -> Int
  corners pos = outerCorners + innerCorners
   where
    outerCorners = sum $ outerCorner <$> outerCornerDirections
     where
      outerCornerDirections = [(North, East), (East, South), (South, West), (West, North)]
      outerCorner (a, b) = case bimapBoth (neighbour grid pos) (a, b) of
        (Just a', Just b') | p /= plant a' && p /= plant b' -> 1
        (Nothing, Just p') | p /= plant p' -> 1
        (Just p', Nothing) | p /= plant p' -> 1
        (Nothing, Nothing) -> 1
        _ -> 0
       where
        p = plant pos

    innerCorners = sum $ innerCorner <$> innerCornerDirections
     where
      innerCornerDirections = [(Northeast, (North, East)), (Southeast, (East, South)), (Southwest, (South, West)), (Northwest, (West, North))]
      innerCorner (a, (b, c)) = fromMaybe 0 $ do
        a' <- neighbour grid pos a
        b' <- neighbour grid pos b
        c' <- neighbour grid pos c
        let p = plant pos
        guard (p /= plant a' && p == plant b' && p == plant c')
        pure 1
