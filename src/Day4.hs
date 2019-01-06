{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.Text as T
import Data.HashMap.Strict (HashMap, empty, insertWith, insert, lookupDefault, toList, elems)
import Data.List (sort)
import Data.Time
import Data.Time.Format
import Data.Foldable (maximumBy)
import Text.Parsec
import Text.Parser.Token (integer)
import Control.Arrow ((***), (&&&))

type ElfId = Integer
type Minute = Integer

data EventType = ShiftChange ElfId
                 | Sleep
                 | Wake
                 deriving (Show, Eq, Ord)

data Event = Event { eventTime :: UTCTime , eventType :: EventType }
           deriving (Eq, Ord)

instance Show Event where
    show (Event t e) = (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t) ++ " " ++ show e

type Guards = HashMap ElfId (HashMap Minute Integer)

parseInput :: [T.Text] -> Either ParseError [Event]
parseInput = sequence . fmap ((parse lineparser "") . T.unpack )

lineparser :: Parsec String () Event
lineparser = Event <$>
             (dateParser <$> dateString) <*>
             (spaces >> eventTypeParser)
  where dateString = char '[' >> manyTill anyChar (char ']')

eventTypeParser :: Parsec String () EventType
eventTypeParser = shiftChange <|> sleep <|> wake
  where shiftChange = ShiftChange <$> (string "Guard #" >> integer)
        sleep = (const Sleep) <$> (string "falls asleep")
        wake = (const Wake) <$> (string "wakes up")

dateParser :: String -> UTCTime
dateParser = parseTimeOrError False defaultTimeLocale "%F %H:%M"

eventGroups :: [Event] -> [[Event]]
eventGroups [] = []
eventGroups es = let
                   (shift, rest) = nextShift es
                 in
                   shift : (eventGroups rest)

nextShift :: [Event] -> ([Event], [Event])
nextShift es = (((:) (head es)) *** id) (break isShiftStart (tail es))

isShiftStart :: Event -> Bool
isShiftStart (Event _ (ShiftChange _)) = True
isShiftStart _ = False

minute :: UTCTime -> Integer
minute t = div (diffTimeToPicoseconds (utctDayTime t)) (60 * 1000000000000)

sleepAt :: ElfId -> Minute -> Guards -> Guards
sleepAt i m gs = insert i (addMinute m $ lookupDefault empty i gs) gs
  where addMinute :: Minute -> HashMap Minute Integer -> HashMap Minute Integer
        addMinute m ms = insertWith (+) m 1 ms

addShift :: [Event] -> Guards -> Guards
addShift [] gs = gs
addShift ((Event _ (ShiftChange eid)):es) gs = addShift' eid es gs
  where addShift' :: ElfId -> [Event] -> Guards -> Guards
        addShift' _ [] gs = gs
        addShift' eid (sleep:wake:es) gs =
            let
              startMinute = minute . eventTime $ sleep
              endMinute = minute . eventTime $ wake
            in
              addShift' eid es $ foldr (sleepAt eid) gs [startMinute..endMinute - 1]

shiftData :: [[Event]] -> Guards
shiftData = foldr addShift empty

sleepiestGuard :: Guards -> (ElfId, HashMap Minute Integer)
sleepiestGuard gs = maximumBy sleepiest $ toList gs
  where minutesAsleep = sum . elems . snd
        sleepiest x y = compare (minutesAsleep x) (minutesAsleep y)

sleepiestMinute :: (ElfId, HashMap Minute Integer) -> (ElfId, Minute)
sleepiestMinute = fmap (fst . maximumBy repeats . toList)
  where repeats x y = compare (snd x) (snd y)

day4 :: [T.Text] -> T.Text
day4 lines = either (T.pack . show) (T.pack . show) $
             ((uncurry (*)) . sleepiestMinute . sleepiestGuard) <$>
             shiftData <$>
             eventGroups <$>
             sort <$>
             parseInput lines

mostConsistentGuard :: HashMap ElfId (Minute, Integer) -> (ElfId, (Minute, Integer))
mostConsistentGuard m = maximumBy minuteCount $ toList m
  where minuteCount (_, (_, x)) (_, (_, y)) = compare x y

maxSleptMinute :: Guards -> HashMap ElfId (Minute, Integer)
maxSleptMinute = fmap ((maximumBy compareSnd) . toList)
  where compareSnd (_, x) (_, y) = compare x y

day4p2 :: [T.Text] -> T.Text
day4p2 lines = either (T.pack . show) (T.pack . show) $
               (uncurry (*) . (fst &&& (fst . snd))) <$>
               mostConsistentGuard <$>
               maxSleptMinute <$>
               shiftData <$>
               eventGroups <$>
               sort <$>
               parseInput lines
