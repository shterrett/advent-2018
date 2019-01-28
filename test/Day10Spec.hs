{-# LANGUAGE OverloadedStrings #-}

module Day10Spec where

import Test.Hspec
import qualified Data.Text as T
import Day10

spec :: Spec
spec = do
    describe "parseInput" $ do
      it "parses the line into a point and velocity" $ do
        let input = [ "position=< 9,  1> velocity=< 0,  2>"
                    , "position=< 9,  1> velocity=< 0,  2>"
                    , "position=< 7,  0> velocity=<-1,  0>"
                    , "position=< 3, -2> velocity=<-1,  1>"
                    , "position=< 6, 10> velocity=<-2, -1>"
                    ]
        parseInput input `shouldBe`
          (Right [ (Point 9 1, Velocity 0 2)
                 , (Point 9 1, Velocity 0 2)
                 , (Point 7 0, Velocity (-1) 0)
                 , (Point 3 (-2), Velocity (-1) 1)
                 , (Point 6 10, Velocity (-2) (-1))
                 ])
    describe "step" $ do
      it "moves a point by one step of velocity" $ do
        let point = Point 3 2
        let velocity = Velocity 2 (-3)
        step (point, velocity) `shouldBe`
          (Point 5 (-1), velocity)
    describe "boundingBox" $ do
      it "returns a bounding box for the set of points" $ do
        let points = [ Point 9 1
                     , Point 9 1
                     , Point 7 0
                     , Point 3 (-2)
                     , Point 6 10
                     ]
        boundingBox points `shouldBe` (Point 3 (-2), Point 9 10)
      it "finds the area of a the bounding box" $ do
        let box = (Point 3 (-2), Point 9 10)
        area box `shouldBe` (6 * 12)
    describe "day10" $ do
      it "returns the number of seconds until the message appears" $ do
        let input = [ "position=< 9,  1> velocity=< 0,  2>"
                    , "position=< 7,  0> velocity=<-1,  0>"
                    , "position=< 3, -2> velocity=<-1,  1>"
                    , "position=< 6, 10> velocity=<-2, -1>"
                    , "position=< 2, -4> velocity=< 2,  2>"
                    , "position=<-6, 10> velocity=< 2, -2>"
                    , "position=< 1,  8> velocity=< 1, -1>"
                    , "position=< 1,  7> velocity=< 1,  0>"
                    , "position=<-3, 11> velocity=< 1, -2>"
                    , "position=< 7,  6> velocity=<-1, -1>"
                    , "position=<-2,  3> velocity=< 1,  0>"
                    , "position=<-4,  3> velocity=< 2,  0>"
                    , "position=<10, -3> velocity=<-1,  1>"
                    , "position=< 5, 11> velocity=< 1, -2>"
                    , "position=< 4,  7> velocity=< 0, -1>"
                    , "position=< 8, -2> velocity=< 0,  1>"
                    , "position=<15,  0> velocity=<-2,  0>"
                    , "position=< 1,  6> velocity=< 1,  0>"
                    , "position=< 8,  9> velocity=< 0, -1>"
                    , "position=< 3,  3> velocity=<-1,  1>"
                    , "position=< 0,  5> velocity=< 0, -1>"
                    , "position=<-2,  2> velocity=< 2,  0>"
                    , "position=< 5, -2> velocity=< 1,  2>"
                    , "position=< 1,  4> velocity=< 2,  1>"
                    , "position=<-2,  7> velocity=< 2, -2>"
                    , "position=< 3,  6> velocity=<-1, -1>"
                    , "position=< 5,  0> velocity=< 1,  0>"
                    , "position=<-6,  0> velocity=< 2,  0>"
                    , "position=< 5,  9> velocity=< 1, -2>"
                    , "position=<14,  7> velocity=<-2,  0>"
                    , "position=<-3,  6> velocity=< 2, -1>"
                    ]
        day10 input `shouldBe` (T.pack $ show ( 3
                                              , unlines [ "#...#..###"
                                                        , "#...#...#."
                                                        , "#...#...#."
                                                        , "#####...#."
                                                        , "#...#...#."
                                                        , "#...#...#."
                                                        , "#...#...#."
                                                        , "#...#..###"
                                                        ]
                                              ))
