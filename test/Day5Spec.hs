{-# LANGUAGE OverloadedStrings #-}

module Day5Spec where

import Test.Hspec
import qualified Data.Text as T
import Day5

spec :: Spec
spec = do
    describe "Monomer Construction" $ do
      it "treats lowercase as positive" $ do
        fromChar 'a' `shouldBe` (Pos 'a')
      it "treats uppercase as negative and normalizes to lowercase" $ do
        fromChar 'A' `shouldBe` (Neg 'a')
    describe "building polymer" $ do
      it "forms a list of monomers, eliminating adjacent inverses" $ do
        condense (fromChar <$> "dabAcCaCBAcCcaDA") `shouldBe`
          (fromChar <$> "dabCBAcaDA")
    describe "day5p2" $ do
      it "returns the length of the shortest polymer yielded by removing a single element" $ do
        let input = "dabAcCaCBAcCcaDA"
        day5p2 input `shouldBe` "4"
