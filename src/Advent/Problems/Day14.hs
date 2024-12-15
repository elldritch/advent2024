module Advent.Problems.Day14 (parse, solve, solve') where

import Relude
import Relude.Extra (bimapBoth, insertWith, (!?))

import Data.Char (intToDigit)
import Data.List (minimumBy, partition)

import Advent.Parse (Parser, char, intP, linesP, string)

type Robot = ((Int, Int), (Int, Int))

parse :: Parser [Robot]
parse = linesP $ (,) <$> ((,) <$> (string "p=" *> intP <* char ',') <*> intP) <* char ' ' <*> ((,) <$> (string "v=" *> intP <* char ',') <*> intP)

solve :: [Robot] -> (Int, String)
solve = solve' (101, 103)

solve' :: (Int, Int) -> [Robot] -> (Int, String)
solve' (width, height) start =
  ( safetyFactor $ fmap fst $ head' $ drop 100 positions
  , let (i, rs) = minimumBy (comparing $ safetyFactor . fmap fst . snd) $ take 10000 (zip [(0 :: Int) ..] positions) in '\n' : show i <> ":\n" <> display rs
  )
 where
  head' = fromMaybe (error "no positions in list") . viaNonEmpty head
  safetyFactor position = product (length <$> [q1, q2, q3, q4])
   where
    ((q1, q2), (q3, q4)) =
      bimapBoth (partition ((< height `div` 2) . snd))
        . partition ((< width `div` 2) . fst)
        $ filter (\(x, y) -> x /= (width `div` 2) && y /= (height `div` 2)) position

  positions = iterate (fmap (\((x, y), (vx, vy)) -> (((x + vx) `mod` width, (y + vy) `mod` height), (vx, vy)))) start

  display :: [Robot] -> String
  display robots =
    concatMap
      ( \(x, y) ->
          maybe "." (one . intToDigit) (botsInPosition !? (x, y))
            <> (if x == width - 1 then "\n" else "")
      )
      [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
   where
    botsInPosition = foldl' (\m (pos, _) -> insertWith (+) pos 1 m) (mempty @(Map (Int, Int) Int)) robots
