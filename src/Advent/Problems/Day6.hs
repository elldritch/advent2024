{-# OPTIONS_GHC -Wno-orphans #-}

module Advent.Problems.Day6 (parse, solve) where

import Relude
import Relude.Extra (lookup, member, toPairs, (!?))

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Math.Geometry.Grid (Index, contains)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import Math.Geometry.Grid.SquareInternal (SquareDirection (..))

import Advent.Parse (Parser, char, gridP)

parse :: Parser (Map (Index RectSquareGrid) Tile, RectSquareGrid)
parse = gridP rectSquareGrid tileP

data Tile = Empty | Obstacle | Guard
  deriving stock (Show, Eq)

tileP :: Parser Tile
tileP = Empty <$ char '.' <|> Obstacle <$ char '#' <|> Guard <$ char '^'

data GuardOutcome = Exited | Looping
  deriving stock (Show, Eq)

solve :: (Map (Index RectSquareGrid) Tile, RectSquareGrid) -> (Int, Int)
solve (tileMap, grid) =
  ( length $ Set.map fst route
  , length $ filter ((== Looping) . fst) obstructedRoutes
  )
 where
  tiles = toPairs tileMap
  obstacles = fromList $ map fst $ filter ((== Obstacle) . snd) tiles
  start = fst $ fromMaybe (error "room has no guard") $ find ((== Guard) . snd) tiles
  route = snd $ visitedByGuard grid obstacles start North
  obstructedRoutes =
    fmap (\pos -> visitedByGuard grid (Set.insert pos obstacles) start North)
      . Set.elems
      . Set.map fst
      $ Set.filter (obstacleMightLoop obstacles) route

visitedByGuard :: RectSquareGrid -> Set (Index RectSquareGrid) -> Index RectSquareGrid -> SquareDirection -> (GuardOutcome, Set (Index RectSquareGrid, SquareDirection))
visitedByGuard grid obstacles = go mempty
 where
  obstaclesByX = Map.fromListWith (<>) $ second one <$> Set.elems obstacles
  obstaclesByY = Map.fromListWith (<>) $ second one . swap <$> Set.elems obstacles

  go :: Set (Index RectSquareGrid, SquareDirection) -> Index RectSquareGrid -> SquareDirection -> (GuardOutcome, Set (Index RectSquareGrid, SquareDirection))
  go seen (x, y) facing = case nextStop of
    Nothing -> (Exited, seen <> see (pathToExit facing))
    Just ((x', y'), pathToObstacle) ->
      if member ((x', y'), facing) seen
        then (Looping, seen)
        else let facing' = turnRight facing in go (Set.insert ((x', y'), facing) seen <> see pathToObstacle) (x', y') facing'
   where
    see = fromList . fmap (,facing)

    pathToExit dir = takeWhile (grid `contains`) $ case dir of
      North -> [(x, y + i) | i <- [1 ..]]
      South -> [(x, y - i) | i <- [1 ..]]
      West -> [(x - i, y) | i <- [1 ..]]
      East -> [(x + i, y) | i <- [1 ..]]

    nextStop = case facing of
      North -> (\y' -> ((x, y' - 1), [(x, yi) | yi <- [y .. y' - 1]])) <$> Set.lookupGT y col
      South -> (\y' -> ((x, y' + 1), [(x, yi) | yi <- [y' + 1 .. y]])) <$> Set.lookupLT y col
      West -> (\x' -> ((x' + 1, y), [(xi, y) | xi <- [x' + 1 .. x]])) <$> Set.lookupLT x row
      East -> (\x' -> ((x' - 1, y), [(xi, y) | xi <- [x .. x' - 1]])) <$> Set.lookupGT x row
     where
      col = fromMaybe mempty $ obstaclesByX !? x
      row = fromMaybe mempty $ obstaclesByY !? y

turnRight :: SquareDirection -> SquareDirection
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

data SquareDirection' = North' | East' | South' | West'
  deriving stock (Show, Eq, Ord)

instance Ord SquareDirection where
  compare a b = compare (toOrd a) (toOrd b)
   where
    toOrd :: SquareDirection -> SquareDirection'
    toOrd North = North'
    toOrd East = East'
    toOrd South = South'
    toOrd West = West'

-- TODO: Can we make this _even faster_? Some thoughts:
--
-- - Is there a condition that allows us to do a simple check for looping? For
--   example, is "the next turned corner is on the old route" sufficient for us
--   to determine a loop? Is it necessary for a loop?
-- - Instead of trying each position on the old route, can we efficiently
--   compute potential looping obstacles from the obstacle layout, and intersect
--   those with the route?
obstacleMightLoop :: Set (Index RectSquareGrid) -> (Index RectSquareGrid, SquareDirection) -> Bool
obstacleMightLoop obstacles ((x, y), facing) = fromMaybe False $ case facing of
  North -> lookup (y - 1) obstaclesByY >>= (fmap (const True) . Set.lookupGT x)
  East -> lookup (x - 1) obstaclesByX >>= (fmap (const True) . Set.lookupLT y)
  South -> lookup (y + 1) obstaclesByY >>= (fmap (const True) . Set.lookupLT x)
  West -> lookup (x + 1) obstaclesByX >>= (fmap (const True) . Set.lookupGT y)
 where
  obstaclesByX = Map.fromListWith (<>) $ second one <$> Set.elems obstacles
  obstaclesByY = Map.fromListWith (<>) $ second one . swap <$> Set.elems obstacles
