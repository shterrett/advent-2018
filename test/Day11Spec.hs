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
        gridPower problem newCache point
          `shouldBe` ( newCache
                     , 4
                     )
      it "adds the power of the lower-sized grid to the leading edge" $ do
        let cells = Map.insert ((2, 2), (1, 1)) 4 $
                    Map.insert ((2, 1), (1, 3)) 3 $ -- Bottom Edge
                    Map.insert ((1, 2), (3, 1)) (-1) $ -- Forward Edge
                    Map.empty
        let cache = Cache Map.empty cells
        let problem = Problem 1 3
        let point = (1, 1)
        let pointPower = calculatePower 1 (3, 3)
        let power = 4 + 3 - 1 + pointPower
        gridPower problem cache point
          `shouldBe` ( cache { currentDim = Map.insert ((3, 3), (1, 1)) power $
                                            Map.insert ((3, 1), (1, 3)) (3 + pointPower) $ -- Bottom Edge
                                            Map.insert ((1, 3), (3, 1)) (-1 + pointPower) $ --- Forward Edge
                                            Map.empty
                             }
                     , power
                     )
    -- describe "day11" $ do
    --   it "returns the (x,y) coordinates of the top left corner of the grid with the highest power" $ do
    --     day11 18 `shouldBe` (33, 45)
