module Main (main) where

import Relude
import Relude.Extra (maximum1, member)

import Data.Set qualified as Set
import Math.Geometry.Grid (neighbour)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)
import Math.Geometry.Grid.SquareInternal (SquareDirection (..))

import Advent.Parse (Parser, char, eof, manyTill, newline, parsePuzzleInput)

main :: IO ()
main = do
  (grid, obstacles, start) <- parsePuzzleInput "data/6" $ do
    tiles <- zip [0 ..] <$> manyTill (zip [0 ..] <$> manyTill tileP newline) eof
    let tiles' = fromMaybe (error "room map is empty") $ nonEmpty $ concatMap (\(y, row) -> fmap (\(x, c) -> ((x, y), c)) row) tiles
        maxX = maximum1 $ fst . fst <$> tiles'
        maxY = maximum1 $ snd . fst <$> tiles'
        tiles'' = first (second (maxY -)) <$> toList tiles'
        obstacles = Set.fromList $ map fst $ filter ((== Obstacle) . snd) tiles''
        start = fst $ fromMaybe (error "room has no guard") $ find ((== Guard) . snd) tiles''
    pure (rectSquareGrid (maxX + 1) (maxY + 1), obstacles, start)
  let route = snd $ visitedByGuard grid obstacles start North
  putStrLn $ "Part 1: " <> show (length route)
  let obstructedRoutes = (\pos -> visitedByGuard grid (Set.insert pos obstacles) start North) <$> toList route
  putStrLn $ "Part 2: " <> show (length $ filter ((== Looping) . fst) obstructedRoutes)

data Tile = Empty | Obstacle | Guard
  deriving (Show, Eq)

tileP :: Parser Tile
tileP = Empty <$ char '.' <|> Obstacle <$ char '#' <|> Guard <$ char '^'

data GuardOutcome = Exited | Looping
  deriving (Show, Eq)

visitedByGuard :: RectSquareGrid -> Set (Int, Int) -> (Int, Int) -> SquareDirection -> (GuardOutcome, Set (Int, Int))
visitedByGuard grid obstacles start direction = go (Set.singleton (start, toOrd direction)) start direction
 where
  go :: Set ((Int, Int), SquareDirection') -> (Int, Int) -> SquareDirection -> (GuardOutcome, Set (Int, Int))
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
  deriving (Show, Eq, Ord)
