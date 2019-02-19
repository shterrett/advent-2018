module Day14 where

import qualified Data.Text as T
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Control.Arrow ((&&&))
import Debug.Trace

type Elves = (Int, Int)
type Recipe = Int
type Recipes = (Recipe, Recipe)
type Scoreboard = Seq Recipe

initialScores :: Scoreboard
initialScores = Seq.fromList [3, 7]

currentRecipes :: Scoreboard -> Elves -> Recipes
currentRecipes b (e1, e2) = (Seq.index b e1, Seq.index b e2)

nextRecipes :: Recipes -> [Recipe]
nextRecipes (r1, r2) | r1 + r2 < 10 = [r1 + r2]
                     | otherwise =
    let
      (t, o) = (tens &&& ones) $ r1 + r2
    in
      [t, o]
  where tens = (flip div) 10
        ones = (flip mod) 10

addRecipes :: Scoreboard -> [Recipe] -> Scoreboard
addRecipes b rs = foldl' (|>) b rs

updateElves :: Scoreboard -> Elves -> Elves
updateElves b (e1, e2) =
    let
      l = Seq.length b
      e1' = nextIdx l b e1
      e2' = nextIdx l b e2
    in
      (e1', e2')
  where nextIdx l b idx = mod ((Seq.index b idx) + idx + 1) l

step :: Scoreboard -> Elves -> (Scoreboard, Elves)
step b es =
    let
      b' = addRecipes b (nextRecipes $ currentRecipes b es)
      es' = updateElves b' es
    in
      (b', es')

day14 :: Int -> T.Text
day14 n =
    let
      last = head $
             drop (n + 10) $
             iterate (uncurry step) (initialScores, (0, 1))
    in
      T.pack $ showSeq $ Seq.take 10 $ Seq.drop n $ fst last

showSeq :: Show a => Seq a -> String
showSeq = foldl' mappend "" . fmap show

day14p2 :: T.Text -> T.Text
day14p2 n = T.pack $ show $ runStep n (initialScores, (0, 1))

runStep :: T.Text -> (Scoreboard, Elves) -> Int
runStep n x =
    let
      patrn = T.unpack n
      patrnLen = length patrn
      (b, es) = (uncurry step) x
      totalLen = Seq.length b
      end = Seq.drop (totalLen - patrnLen - 1) b
      suffix = showSeq $ Seq.drop 1 end
      suffix2 = showSeq $ Seq.take patrnLen end
    in
      case (suffix == patrn, suffix2 == patrn) of
        (True, False) -> Seq.length b - patrnLen
        (False, True) -> Seq.length b - patrnLen - 1
        _ -> runStep n (b, es)
