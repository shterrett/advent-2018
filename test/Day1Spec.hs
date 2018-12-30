{-# LANGUAGE OverloadedStrings #-}

module Day1Spec where

import Test.Hspec
import Day1

spec :: Spec
spec = do
    describe "Day 1" $ do
      it "example 1" $ do
        let lines = ["+1", "-2", "+3", "+1"]
        day1 lines `shouldBe` "3"
      it "example 2" $ do
        let lines = ["+1", "+1", "+1"]
        day1 lines `shouldBe` "3"
      it "example 3" $ do
        let lines = ["+1", "+1", "-2"]
        day1 lines `shouldBe` "0"
      it "example 4" $ do
        let lines = ["-1", "-2", "-3"]
        day1 lines `shouldBe` "-6"
    describe "Day 1 part 2" $ do
      it "examaple 1" $ do
        let lines = ["+1", "-1"]
        day1p2 lines `shouldBe` "0"
      it "example 2" $ do
        let lines = ["+3", "+3", "+4", "-2", "-4"]
        day1p2 lines `shouldBe` "10"
      it "example 3" $ do
        let lines = ["-6", "+3", "+8", "+5", "-6"]
        day1p2 lines `shouldBe` "5"
      it "example 4" $ do
        let lines = ["+7", "+7", "-2", "-7", "-4"]
        day1p2 lines `shouldBe` "14"
