{-# LANGUAGE OverloadedStrings #-}

module Day9Spec where

import Test.Hspec
import qualified Data.Sequence as Seq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Day9

spec :: Spec
spec = do
    describe "Circle" $ do
      it "constructs a circle from a list" $ do
        fromList [1, 2, 3, 4] `shouldBe` Circle (Seq.fromList [1, 2, 3, 4]) 4
    describe "insertNew" $ do
      it "inserts a new marble between the first and second marble, sets that marble active, and rotates the list" $ do
        let circle = fromList [1, 2, 3, 4]
        insertNew 5 circle `shouldBe`
          fromList [5, 3, 4, 1, 2]
    describe "scorePos finds the position of the marble 7 steps counterclockwise" $ do
        it "returns the clockwise position for a circle with more than 7 marbles" $ do
          scorePos 10 `shouldBe` 3
        it "returns the clockwise position accounting for looping around the circle for circles with less than 7 marbles" $ do
          scorePos 4 `shouldBe` 1
        it "returns 0 for circles with exactly 7 marbles" $ do
          scorePos 7 `shouldBe` 0
    describe "score" $ do
      it "removes the marble 7 counterclockwise of the current, and sets the marble clockwise of that as the current" $ do
        let circle = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        score circle `shouldBe` (4, fromList [5, 6, 7, 8, 9, 10, 1, 2, 3])
      it "removes the marble 7 counterclockwise of the current, allowing for multiple revolutions, and sets the next marble to current" $ do
        let circle = fromList [1, 2, 3, 4]
        score circle `shouldBe` (2, fromList [3, 4, 1])
      it "removes the current marble and sets the next marble to current for 7 marbles" $ do
        let circle = fromList [1, 2, 3, 4, 5, 6, 7]
        score circle `shouldBe` (1, fromList [2, 3, 4, 5, 6, 7])
      it "removes the last marble and sets the current marble to current for 6 marbles" $ do
        let circle = fromList [1, 2, 3, 4, 5, 6]
        score circle `shouldBe` (6, fromList [1, 2, 3, 4, 5])
    describe "parseInput" $ do
      it "returns a tuple of players and marbles" $ do
        parseInput ["10 players; last marble is worth 1618 points"] `shouldBe`
          (Right (10, 1618))
        parseInput ["13 players; last marble is worth 7999 points"] `shouldBe`
          (Right (13, 7999))
        parseInput ["17 players; last marble is worth 1104 points"] `shouldBe`
          (Right (17, 1104))
        parseInput ["21 players; last marble is worth 6111 points"] `shouldBe`
          (Right (21, 6111))
        parseInput ["30 players; last marble is worth 5807 points"] `shouldBe`
          (Right (30, 5807))
    describe "turns" $ do
      it "returns a list of turns for each elf, in order" $ do
        turns (3, 9) `shouldBe` [ (1, 0)
                                , (2, 1)
                                , (3, 2)
                                , (1, 3)
                                , (2, 4)
                                , (3, 5)
                                , (1, 6)
                                , (2, 7)
                                , (3, 8)
                                , (1, 9)
                                ]
    describe "takeTurn" $ do
      it "inserts the next marble when it is not a multiple of 25" $ do
        let circle = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        let scores = Map.empty
        let game = Game circle scores
        takeTurn game (3, 13) `shouldBe` (Game (insertNew 13 circle) scores)
      it "adds the marble to the elf's score and scores the 7th counter-clockwise marble" $ do
        let circle = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        let scores = Map.empty
        let game = Game circle scores
        takeTurn game (3, 23) `shouldBe`
          (Game (snd $ score circle)
                (Map.singleton 3 (23 + 4)))
    describe "day9" $ do
      it "returns the maximum score of the winning elf" $ do
        day9 ["10 players; last marble is worth 1618 points"] `shouldBe` "8317"
        day9 ["13 players; last marble is worth 7999 points"] `shouldBe` "146373"
        day9 ["17 players; last marble is worth 1104 points"] `shouldBe` "2764"
        day9 ["21 players; last marble is worth 6111 points"] `shouldBe` "54718"
        day9 ["30 players; last marble is worth 5807 points"] `shouldBe` "37305"
