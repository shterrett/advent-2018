{-# LANGUAGE OverloadedStrings #-}

module Day11Spec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Day11

spec :: Spec
spec = do
    describe "generate 3x3 points" $ do
      it "generates a list of points that's a 3x3 grid from the top left" $ do
        grid 3 (5, 8) `shouldBe`
          [ (5, 8), (5, 9), (5, 10)
          , (6, 8), (6, 9), (6, 10)
          , (7, 8), (7, 9), (7, 10)
          ]

    describe "powerRemaining" $ do
      it "returns the formula specified in the problem and caches it" $ do
        let serialNumber = 8
        let point = (3, 5)
        powerRemaining serialNumber Map.empty point `shouldBe` ( Map.singleton (3, 5) 4
                                                               , 4
                                                               )
    describe "gridPower" $ do
      it "calculates the total power for a given grid" $ do
        let serialNo = 18
        let corner = (33, 45)
        let g = grid 3 corner
        snd (gridPower serialNo Map.empty g) `shouldBe` 29
    describe "day11" $ do
      it "returns the (x,y) coordinates of the top left corner of the grid with the highest power" $ do
        day11 18 `shouldBe` (33, 45)
        day11 42 `shouldBe` (21, 61)
