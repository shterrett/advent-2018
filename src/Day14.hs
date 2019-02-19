module Day14 where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Scoreboard = Seq Int
type Elves = (Int, Int)
type Recipes = (Int, Int)

initialScores :: Scoreboard
initialScores = Seq.fromList [3, 7]

currentRecipes :: Scoreboard -> Elves -> Recipes
currentRecipes b (e1, e2) = (Seq.index b e1, Seq.index b e2)
