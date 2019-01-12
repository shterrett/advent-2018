{-# LANGUAGE OverloadedStrings #-}

module Day6Spec where

import Test.Hspec
import Data.HashMap.Strict (empty, insert)
import qualified Data.HashSet as Set
import Day6

spec :: Spec
spec = do
    describe "parsing input" $ do
      let input = [ "1, 1"
                  , "1, 6"
                  , "8, 3"
                  , "3, 4"
                  , "5, 5"
                  , "8, 9"
                  ]
      it "parses a list of points" $ do
        parseInput input `shouldBe`
          (Right [ Point 1 1
                 , Point 1 6
                 , Point 8 3
                 , Point 3 4
                 , Point 5 5
                 , Point 8 9
                 ])
    describe "bounding box" $ do
      it "finds a bounding box of a set of points" $ do
        let points = [ Point 1 1
                     , Point 1 6
                     , Point 8 3
                     , Point 3 4
                     , Point 5 5
                     , Point 8 9
                     ]
        boundingBox points `shouldBe`
          (Point 1 1, Point 8 9)
      it "finds a bounding box when the points aren't the bound" $ do
        let points = [ Point 1 5
                     , Point 1 6
                     , Point 8 3
                     , Point 3 4
                     , Point 5 5
                     , Point 2 9
                     ]
        boundingBox points `shouldBe`
          (Point 1 3, Point 8 9)
    describe "points in box" $ do
      it "returns a list of all the points in a bounding box" $ do
        let box = (Point 1 2, Point 3 4)
        pointsInBox box `shouldBe`
          [ Point 1 2
          , Point 1 3
          , Point 1 4
          , Point 2 2
          , Point 2 3
          , Point 2 4
          , Point 3 2
          , Point 3 3
          , Point 3 4
          ]
    describe "closest point" $ do
      it "returns the closest reference point to a point in the box" $ do
        let ref = [ Point 1 2
                  , Point 3 4
                  ]
        closestPoint ref (Point 1 1) `shouldBe` (Single 1 $ Point 1 2)
      it "returns all of the closest reference points in the event of a tie" $ do
        let ref = [ Point 1 2
                  , Point 3 4
                  ]
        closestPoint ref (Point 2 3)
          `shouldBe` (Tie 2 [Point 3 4, Point 1 2])
    describe "closest points" $ do
      it "returns the closest reference point to each point in the box" $ do
        let points = pointsInBox (Point 1 1, Point 4 4)
        let refs = [ Point 1 2
                   , Point 3 4
                   ]
        closestPoints points refs `shouldBe`
          (insert (Point 1 1) (Single 1 $ Point 1 2) $
           insert (Point 1 2) (Single 0 $ Point 1 2) $
           insert (Point 1 3) (Single 1 $ Point 1 2) $
           insert (Point 1 4) (Tie 2 [Point 3 4, Point 1 2]) $
           insert (Point 2 1) (Single 2 $ Point 1 2) $
           insert (Point 2 2) (Single 1 $ Point 1 2) $
           insert (Point 2 3) (Tie 2 [Point 3 4, Point 1 2]) $
           insert (Point 2 4) (Single 1 $ Point 3 4) $
           insert (Point 3 1) (Tie 3 [Point 3 4, Point 1 2]) $
           insert (Point 3 2) (Tie 2 [Point 3 4, Point 1 2]) $
           insert (Point 3 3) (Single 1 $ Point 3 4) $
           insert (Point 3 4) (Single 0 $ Point 3 4) $
           insert (Point 4 1) (Tie 4 [Point 3 4, Point 1 2]) $
           insert (Point 4 2) (Tie 3 [Point 3 4, Point 1 2]) $
           insert (Point 4 3) (Single 2 $ Point 3 4) $
           insert (Point 4 4) (Single 1 $ Point 3 4) $
           empty)
    describe "border points" $ do
      it "returns the points on the periphery of the box" $ do
        let points = pointsInBox (Point 1 1, Point 4 4)
        borderPoints (Point 1 1, Point 4 4) points `shouldBe`
          [ Point 1 1
          , Point 1 2
          , Point 1 3
          , Point 1 4
          , Point 2 1
          , Point 2 4
          , Point 3 1
          , Point 3 4
          , Point 4 1
          , Point 4 2
          , Point 4 3
          , Point 4 4
          ]
    describe "infinite points" $ do
      it "returns the reference points that have infinite area" $ do
        let ref = [ Point 1 1
                  , Point 1 6
                  , Point 8 3
                  , Point 3 4
                  , Point 5 5
                  , Point 8 9
                  ]
        let points = pointsInBox $ boundingBox ref
        let claimed = closestPoints points ref
        let border = borderPoints (boundingBox ref) points
        infinitePoints claimed border `shouldBe`
          Set.fromList [ Point 1 1
                       , Point 1 6
                       , Point 8 3
                       , Point 8 9
                       ]
    describe "greatest area" $ do
      it "returns the area covered by the reference point with the most reach" $ do
        let ref = [ Point 1 1
                  , Point 1 6
                  , Point 8 3
                  , Point 3 4
                  , Point 5 5
                  , Point 8 9
                  ]
        greatestArea ref `shouldBe` 17
    describe "distance to all refs" $ do
      it "accumulates the distance from a point to all the reference points" $ do
        let points = [ Point (-3) 4
                     , Point 3 (-4)
                     , Point (-3) (-4)
                     , Point 3 4
                     ]
        distToAllRefs points (Point 1 1) `shouldBe` 7 + 7 + 9 + 5
    describe "Region in boundary" $ do
      it "returns the number of points within the boundary of _all_ reference points" $ do
        let ref = [ Point 1 1
                  , Point 1 6
                  , Point 8 3
                  , Point 3 4
                  , Point 5 5
                  , Point 8 9
                  ]
        pointsWithinBoundary 32 ref `shouldBe` 16
