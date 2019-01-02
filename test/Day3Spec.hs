{-# LANGUAGE OverloadedStrings #-}

module Day3Spec where

import Test.Hspec
import Day3

spec :: Spec
spec = do
    describe "Parsing input" $ do
      it "returns a Claim for the input line" $ do
        let input = [ "#1 @ 1,3: 4x4"
                    , "#2 @ 3,1: 4x4"
                    , "#3 @ 5,5: 2x2"
                    ]
        parseInput input `shouldBe`
            Right [ Claim 1 1 3 4 4
                  , Claim 2 3 1 4 4
                  , Claim 3 5 5 2 2
                  ]
    describe "enumerating points for a claim" $ do
      it "returns a list of all points (distance from left, distance from top) in a claim" $ do
        let claim = Claim 1 1 3 4 4
        points claim `shouldBe` [ (1, 3)
                                , (1, 4)
                                , (1, 5)
                                , (1, 6)
                                , (2, 3)
                                , (2, 4)
                                , (2, 5)
                                , (2, 6)
                                , (3, 3)
                                , (3, 4)
                                , (3, 5)
                                , (3, 6)
                                , (4, 3)
                                , (4, 4)
                                , (4, 5)
                                , (4, 6)
                                ]
    describe "day3" $ do
      it "returns the number of points that are included in multiple claims" $ do
        let input = [ "#1 @ 1,3: 4x4"
                    , "#2 @ 3,1: 4x4"
                    , "#3 @ 5,5: 2x2"
                    ]
        day3 input `shouldBe` "4"
    describe "day3p2" $ do
      it "returns the id of the claim that doesn't overlap" $ do
        let input = [ "#1 @ 1,3: 4x4"
                    , "#2 @ 3,1: 4x4"
                    , "#3 @ 5,5: 2x2"
                    ]
        day3p2 input `shouldBe` "3"
