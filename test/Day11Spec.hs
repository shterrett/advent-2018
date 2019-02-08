{-# LANGUAGE OverloadedStrings #-}

module Day11Spec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Day11

spec :: Spec
spec = do
    describe "calculating power of a grid" $ do
      it "runs the calculation for a 1x1 grid" $ do
        let point = (3, 5)
        let problem = Problem 8 1
        gridPower problem Map.empty point
          `shouldBe` ( Map.singleton (1, point) 4
                     , 4
                     )
      it "adds the power of the lower-sized grid to the leading edge" $ do
        let cells = Map.insert (2, (1, 1)) 4 $
                    Map.insert (2, (1, 2)) 3 $
                    Map.insert (2, (2, 1)) (-1) $
                    Map.insert (2, (2, 2)) 1 $
                    Map.insert (1, (1, 1)) 1 $
                    Map.insert (1, (1, 2)) 3 $
                    Map.insert (1, (1, 3)) 8 $
                    Map.insert (1, (2, 1)) 2 $
                    Map.insert (1, (2, 2)) (-5) $
                    Map.insert (1, (2, 3)) 2 $
                    Map.insert (1, (3, 1)) 8 $
                    Map.insert (1, (3, 2)) 4 $
                    Map.insert (1, (3, 3)) 9 Map.empty
        let problem = Problem 1 3
        let point = (1, 1)
        gridPower problem cells point
          `shouldBe` ( Map.insert (3, (1, 1))
                                  (4 + 8 + 2 + 9 + 8 + 4)
                                  cells
                      , (4 + 8 + 2 + 9 + 8 + 4)
                     )
    describe "day11" $ do
      it "returns the (x,y) coordinates of the top left corner of the grid with the highest power" $ do
        day11 18 `shouldBe` (33, 45)
        day11 42 `shouldBe` (21, 61)
