module Advent.Problems.Day6 (parse, solve) where

import Relude
import Relude.Extra (member, toPairs)

import Data.Set qualified as Set
import Math.Geometry.Grid (Index, neighbour)
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
  ( length route
  , length $ filter ((== Looping) . fst) obstructedRoutes
  )
 where
  tiles = toPairs tileMap
  obstacles = fromList $ map fst $ filter ((== Obstacle) . snd) tiles
  start = fst $ fromMaybe (error "room has no guard") $ find ((== Guard) . snd) tiles
  route = snd $ visitedByGuard grid obstacles start North
  obstructedRoutes = (\pos -> visitedByGuard grid (Set.insert pos obstacles) start North) <$> toList route

visitedByGuard :: RectSquareGrid -> Set (Index RectSquareGrid) -> Index RectSquareGrid -> SquareDirection -> (GuardOutcome, Set (Index RectSquareGrid))
visitedByGuard grid obstacles start direction = go (one (start, toOrd direction)) start direction
 where
  go :: Set (Index RectSquareGrid, SquareDirection') -> Index RectSquareGrid -> SquareDirection -> (GuardOutcome, Set (Index RectSquareGrid))
  go seen pos facing = case neighbour grid pos facing of
    Nothing -> (Exited, Set.map fst seen)
    Just pos' ->
      if member pos' obstacles
        then let facing' = turnRight facing in go (Set.insert (pos, toOrd facing') seen) pos facing'
        else if member (pos', toOrd facing) seen then (Looping, Set.map fst seen) else go (Set.insert (pos', toOrd facing) seen) pos' facing

  turnRight :: SquareDirection -> SquareDirection
  turnRight North = East
  turnRight East = South
  turnRight South = West
  turnRight West = North

  toOrd :: SquareDirection -> SquareDirection'
  toOrd North = North'
  toOrd East = East'
  toOrd South = South'
  toOrd West = West'

data SquareDirection' = North' | East' | South' | West'
  deriving stock (Show, Eq, Ord)
