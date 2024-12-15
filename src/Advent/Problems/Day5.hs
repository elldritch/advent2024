module Advent.Problems.Day5 (parse, solve) where

import Relude
import Relude.Extra (member, (!?))
import Relude.Unsafe ((!!))

import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Advent.Parse (Parser, char, eof, intP, manyTill, newline, sepBy)

parse :: Parser ([(Int, Int)], [[Int]])
parse =
  (,)
    <$> manyTill ((,) <$> intP <* char '|' <*> intP <* newline) newline
    <*> manyTill (sepBy intP (char ',') <* newline) eof

solve :: ([(Int, Int)], [[Int]]) -> (Int, Int)
solve (rules, updates) = (sum $ mid <$> ordered, sum $ mid . fixUpdate rules <$> unordered)
 where
  (ordered, unordered) = partition (updateIsOrdered rules) updates
  mid xs = xs !! (length xs `div` 2)

updateIsOrdered :: [(Int, Int)] -> [Int] -> Bool
updateIsOrdered rules pages = snd $ foldl' checkPage (mempty, True) pages
 where
  rulesMap = makeRulesMap rules pages

  checkPage :: (Set Int, Bool) -> Int -> (Set Int, Bool)
  checkPage (seen, ok) page = (Set.insert page seen, Set.intersection seen afters == mempty && ok)
   where
    afters = fromMaybe mempty $ rulesMap !? page

fixUpdate :: [(Int, Int)] -> [Int] -> [Int]
fixUpdate rules pages = sortBy compareByRules pages
 where
  rulesMap = makeRulesMap rules pages

  compareByRules :: Int -> Int -> Ordering
  compareByRules x y =
    fromMaybe EQ $
      (rulesMap !? x >>= (\afterX -> if y `member` afterX then Just LT else Nothing))
        <|> (rulesMap !? y >>= (\afterY -> if x `member` afterY then Just GT else Nothing))

makeRulesMap :: [(Int, Int)] -> [Int] -> Map Int (Set Int)
makeRulesMap rules pages = Map.fromListWith (<>) $ second one <$> rules'
 where
  pagesSet = Set.fromList pages
  rules' = filter (\(before, after) -> before `member` pagesSet && after `member` pagesSet) rules
