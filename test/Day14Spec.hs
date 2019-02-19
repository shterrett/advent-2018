{-# LANGUAGE OverloadedStrings #-}

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
    describe "nextRecipes" $ do
      it "returns a list of digits in the sum of the given recipes" $ do
        nextRecipes (3, 5) `shouldBe` [8]
        nextRecipes (9, 5) `shouldBe` [1, 4]
        nextRecipes (8, 2) `shouldBe` [1, 0]
    describe "addRecipes" $ do
      it "appends the new recipes onto the scoreboard" $ do
        let scoreboard = Seq.fromList [1, 2, 3, 4]
        let recipes = [1, 3]
        addRecipes scoreboard recipes `shouldBe` (Seq.fromList [1, 2, 3, 4, 1, 3])
    describe "updateElves" $ do
      it "updates the elves' recipe indexes to (1 + current recipe score) with a wraparound" $ do
        let elves = (0, 1)
        let scoreboard = Seq.fromList [1, 3, 5, 7, 9, 2, 4, 6, 8]
        updateElves scoreboard elves `shouldBe` (2, 5)
        let scoreboard' = Seq.fromList [9, 7, 5]
        updateElves scoreboard' elves `shouldBe` (1, 0)
    describe "step" $ do
      it "adds the new recipes and updates the elf positions" $ do
        let elves = (0, 1)
        let scoreboard = Seq.fromList [3, 7]
        step scoreboard elves `shouldBe` (Seq.fromList [3, 7, 1, 0], (0, 1))
        let elves' = (3, 6)
        let scoreboard' = Seq.fromList [3, 7, 1, 0, 1, 0, 1, 2, 4, 5]
        step scoreboard' elves'
          `shouldBe` ( Seq.fromList [3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1]
                    , (4, 8))
    describe "day14" $ do
      it "runs until n + 10 recipes have been made, and reports the last 10" $ do
        day14 9 `shouldBe` "5158916779"
        day14 5 `shouldBe` "0124515891"
        day14 18 `shouldBe` "9251071085"
        day14 2018 `shouldBe` "5941429882"
    describe "day14p2" $ do
      it "runs until the latest recipes match the digits of the input and take the previous 5 recipes" $ do
        day14p2 "51589" `shouldBe` "9"
        day14p2 "01245" `shouldBe` "5"
        day14p2 "92510" `shouldBe` "18"
        day14p2 "59414" `shouldBe` "2018"
