module Day14Spec where

import Test.Hspec
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Day14

spec :: Spec
spec = do
    describe "currentRecipes" $ do
      it "retrieves the elves' current recipes" $ do
        let elves = (3, 6)
        let scoreboard = Seq.fromList [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
        currentRecipes scoreboard elves `shouldBe` (13, 16)
