module Main (main) where

import Relude
import Relude.Extra (lookup, maximum1)

import Data.Map.Strict qualified as Map
import Math.Geometry.Grid (Direction, Grid, Index, indices, neighbour)
import Math.Geometry.Grid.Octagonal (rectOctGrid)
import Math.Geometry.Grid.OctagonalInternal (OctDirection (..))

import Advent.Parse (anySingleBut, eof, manyTill, newline, parsePuzzleInput)

main :: IO ()
main = do
  wordMap <- parsePuzzleInput "data/4" $ do
    tiles <- zip [0 ..] <$> manyTill (zip [0 ..] <$> manyTill (anySingleBut '\n') newline) eof
    pure $ Map.fromList $ concatMap (\(y, row) -> fmap (\(x, c) -> ((x, y), c)) row) tiles
  let keys = fromMaybe (error "no tiles") $ nonEmpty $ Map.keys wordMap
      maxX = maximum1 $ fst <$> keys
      maxY = maximum1 $ snd <$> keys
      grid = rectOctGrid (maxX + 1) (maxY + 1)
      finds = concatMap (\loc -> fmap (xmasInDirection wordMap grid loc) directions) $ indices grid
  putStrLn $ "Part 1: " <> show (length $ filter id finds)
  putStrLn $ "Part 2: " <> show (length $ filter id $ xmasInSquare wordMap <$> indices grid)
 where
  directions = [West, Northwest, North, Northeast, East, Southeast, South, Southwest]

xmasInDirection :: forall g. (Grid g, Ord (Index g), Eq (Direction g)) => Map (Index g) Char -> g -> Index g -> Direction g -> Bool
xmasInDirection wordMap grid position direction = fromMaybe False $ go position "XMAS"
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
xmasInSquare wordMap (x, y) = fromMaybe False $ do
  topLeft <- lookup (x, y) wordMap
  topRight <- lookup (x + 2, y) wordMap
  center <- lookup (x + 1, y + 1) wordMap
  bottomLeft <- lookup (x, y + 2) wordMap
  bottomRight <- lookup (x + 2, y + 2) wordMap
  pure $
    center == 'A'
      && ((topLeft == 'M' && bottomRight == 'S') || (topLeft == 'S' && bottomRight == 'M'))
      && ((topRight == 'M' && bottomLeft == 'S') || (topRight == 'S' && bottomLeft == 'M'))
