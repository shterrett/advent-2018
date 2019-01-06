{-# LANGUAGE OverloadedStrings #-}

module Day4Spec where

import Test.Hspec
import qualified Data.Text as T
import Data.HashMap.Strict (empty, insert)
import Day4

spec :: Spec
spec = do
    describe "parsing input" $ do
      it "does a thing" $ do
        let input = [ "[1518-11-01 00:00] Guard #10 begins shift"
                    , "[1518-11-01 00:05] falls asleep"
                    , "[1518-11-01 00:25] wakes up"
                    ]
        parseInput input `shouldBe`
          Right [ Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                , Event (dateParser "1518-11-01 00:05") Sleep
                , Event (dateParser "1518-11-01 00:25") Wake
                ]
    describe "comparing events" $ do
      it "orders by event time" $ do
        let e1 = Event (dateParser "1518-11-01 00:05") Sleep
        let e2 = Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
        compare e1 e2 `shouldBe` GT
      it "orders by event type -- shift change < sleep < wake -- if the times are equal" $ do
        let e1 = Event (dateParser "1518-11-01 00:05") Sleep
        let e2 = Event (dateParser "1518-11-01 00:05") (ShiftChange 10)
        compare e1 e2 `shouldBe` GT
    describe "grouping events by guard" $ do
      it "returns a list of lists where the inner list is a single shift" $ do
        let events = [ Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                     , Event (dateParser "1518-11-01 00:05") Sleep
                     , Event (dateParser "1518-11-01 00:25") Wake
                     , Event (dateParser "1518-11-01 00:00") (ShiftChange 11)
                     , Event (dateParser "1518-11-01 00:05") Sleep
                     , Event (dateParser "1518-11-01 00:15") Wake
                     , Event (dateParser "1518-11-01 00:20") Sleep
                     , Event (dateParser "1518-11-01 00:25") Wake
                     , Event (dateParser "1518-11-01 00:00") (ShiftChange 12)
                     , Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                     , Event (dateParser "1518-11-01 00:05") Sleep
                     , Event (dateParser "1518-11-01 00:25") Wake
                     ]
        eventGroups events `shouldBe`
                     [ [ Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                       , Event (dateParser "1518-11-01 00:05") Sleep
                       , Event (dateParser "1518-11-01 00:25") Wake
                       ]
                     , [ Event (dateParser "1518-11-01 00:00") (ShiftChange 11)
                       , Event (dateParser "1518-11-01 00:05") Sleep
                       , Event (dateParser "1518-11-01 00:15") Wake
                       , Event (dateParser "1518-11-01 00:20") Sleep
                       , Event (dateParser "1518-11-01 00:25") Wake
                       ]
                     , [ Event (dateParser "1518-11-01 00:00") (ShiftChange 12) ]
                     , [ Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                       , Event (dateParser "1518-11-01 00:05") Sleep
                       , Event (dateParser "1518-11-01 00:25") Wake
                       ]
                     ]
    describe "adding a shift to the guards tracker" $ do
      it "adds 1 to each minute a guard was asleep for that shift" $ do
        let shift = [ Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                    , Event (dateParser "1518-11-01 00:05") Sleep
                    , Event (dateParser "1518-11-01 00:08") Wake
                    ]
        let minutes = insert 5 1 $
                      insert 6 1 $
                      insert 7 1 $
                      empty
        let guards = insert 10 minutes $ empty
        addShift shift empty `shouldBe` guards
      it "increments a shift that has the same minutes" $ do
        let shift = [ Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                    , Event (dateParser "1518-11-01 00:05") Sleep
                    , Event (dateParser "1518-11-01 00:08") Wake
                    ]
        let minutes = insert 4 1 $
                      insert 5 1 $
                      insert 6 1 $
                      insert 7 1 $
                      insert 8 1 $
                      empty
        let guards = insert 10 minutes $ empty
        addShift shift guards `shouldBe`
          (insert 10 (insert 4 1 $
                      insert 5 2 $
                      insert 6 2 $
                      insert 7 2 $
                      insert 8 1 $
                      empty) $
                  empty)
    describe "collecting all shift data" $ do
      it "adds each shift to the map of guards" $ do
        let shift_1 = [ Event (dateParser "1518-11-01 00:00") (ShiftChange 10)
                      , Event (dateParser "1518-11-01 00:05") Sleep
                      , Event (dateParser "1518-11-01 00:08") Wake
                      ]
        let shift_2 = [ Event (dateParser "1518-11-02 00:00") (ShiftChange 11)
                      , Event (dateParser "1518-11-02 00:04") Sleep
                      , Event (dateParser "1518-11-02 00:07") Wake
                      ]
        shiftData [shift_1, shift_2] `shouldBe`
          (insert 10 (insert 5 1 $
                      insert 6 1 $
                      insert 7 1 $
                      empty) $
           insert 11 (insert 4 1 $
                      insert 5 1 $
                      insert 6 1 $
                      empty)
                  empty)
    describe "day4" $ do
      it "returns the product of the guard id and minute of the sleepiest guard" $ do
        let input = [ "[1518-11-01 00:00] Guard #10 begins shift"
                    , "[1518-11-01 00:05] falls asleep"
                    , "[1518-11-01 00:25] wakes up"
                    , "[1518-11-01 00:30] falls asleep"
                    , "[1518-11-01 00:55] wakes up"
                    , "[1518-11-01 23:58] Guard #99 begins shift"
                    , "[1518-11-02 00:40] falls asleep"
                    , "[1518-11-02 00:50] wakes up"
                    , "[1518-11-03 00:05] Guard #10 begins shift"
                    , "[1518-11-03 00:24] falls asleep"
                    , "[1518-11-03 00:29] wakes up"
                    , "[1518-11-04 00:02] Guard #99 begins shift"
                    , "[1518-11-04 00:36] falls asleep"
                    , "[1518-11-04 00:46] wakes up"
                    , "[1518-11-05 00:03] Guard #99 begins shift"
                    , "[1518-11-05 00:45] falls asleep"
                    , "[1518-11-05 00:55] wakes up"
                    ]
        day4 input `shouldBe` (T.pack . show $ 10 * 24)
    describe "day4p2" $ do
      it "returns the guard who was asleep on the same minute the most times" $ do
        let input = [ "[1518-11-01 00:00] Guard #10 begins shift"
                    , "[1518-11-01 00:05] falls asleep"
                    , "[1518-11-01 00:25] wakes up"
                    , "[1518-11-01 00:30] falls asleep"
                    , "[1518-11-01 00:55] wakes up"
                    , "[1518-11-01 23:58] Guard #99 begins shift"
                    , "[1518-11-02 00:40] falls asleep"
                    , "[1518-11-02 00:50] wakes up"
                    , "[1518-11-03 00:05] Guard #10 begins shift"
                    , "[1518-11-03 00:24] falls asleep"
                    , "[1518-11-03 00:29] wakes up"
                    , "[1518-11-04 00:02] Guard #99 begins shift"
                    , "[1518-11-04 00:36] falls asleep"
                    , "[1518-11-04 00:46] wakes up"
                    , "[1518-11-05 00:03] Guard #99 begins shift"
                    , "[1518-11-05 00:45] falls asleep"
                    , "[1518-11-05 00:55] wakes up"
                    ]
        day4p2 input `shouldBe` (T.pack . show $ 99 * 45)
