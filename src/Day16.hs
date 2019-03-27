module Day16 where

import Day16Opcode
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Data.List.Split (chunksOf)
import Text.Parsec
import Text.Parsec.Char (newline)
import Text.Parser.Token (integer, symbol)

type Args = (Int, Int, Int)
data Event = Event { before :: Register
                   , after :: Register
                   , opNum :: Int
                   , args :: Args
                   }
                   deriving (Eq, Show)

eventParser :: Parsec String () Event
eventParser = (\b op args a -> Event b a op args) <$>
              registerParser <*>
              opParser <*>
              argParser <*>
              registerParser

registerParser :: Parsec String () Register
registerParser = mkRegister' <$> (
                  (string "Before: " <|> string "After:  ") >>
                  (between (symbol "[")
                           (symbol "]")
                           (sepBy1 intParser (string ", ")))
                 )
  where mkRegister' (w:x:y:z:[]) = mkRegister w x y z

opParser :: Parsec String () Int
opParser = intParser

argParser :: Parsec String () Args
argParser = (,,) <$>
            (spaces >> intParser) <*>
            (spaces >> intParser) <*>
            (spaces >> intParser)

intParser :: Parsec String () Int
intParser = fromInteger <$> integer

parseInput :: [T.Text] -> Either ParseError [Event]
parseInput input = sequence . fmap (parse eventParser "") $
                   fmap unlines $
                   chunksOf 3 $
                   filter (not . null) $
                   fmap T.unpack input

execute :: Register -> Args -> OpCode -> Maybe Register
execute r (a, b, c) o = o a b c r

atLeastThree :: [Event] -> Int
atLeastThree es = length $
                  filter (3 <=) $
                  countOpcodes <$> es

countOpcodes :: Event -> Int
countOpcodes e = length $
                 filter ((==) $ after e) $
                 mapMaybe (execute (before e) (args e)) $
                 opcodes

day16 :: [T.Text] -> T.Text
day16 input = either (T.pack . show) (T.pack . show) $
              atLeastThree <$>
              parseInput input
