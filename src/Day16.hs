module Day16 where

import Day16Opcode
import qualified Data.Text as T
import Data.Maybe (mapMaybe, fromJust)
import Data.List.Split (chunksOf)
import Data.List (foldl')
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
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

type Command = (Int, Args)

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

execute :: Register -> Args -> OpFn -> Maybe Register
execute r (a, b, c) o = o a b c r

atLeastThree :: [Event] -> Int
atLeastThree es = length $
                  filter (3 <=) $
                  countOpcodes <$> es

countOpcodes :: Event -> Int
countOpcodes e = length $
                 filter ((==) $ after e) $
                 mapMaybe (execute (before e) (args e)) $
                 fn <$> opcodes

day16 :: [T.Text] -> T.Text
day16 input = either (T.pack . show) (T.pack . show) $
              atLeastThree <$>
              parseInput input

successfulOpcodes :: Event -> [OpCode]
successfulOpcodes e = mapMaybe (correctCode e) opcodes
  where correctCode e o = const o <$>
                          (runOp e o >>= maybeBool (eqAfter e))
        runOp e o = execute (before e) (args e) (fn o)
        eqAfter e = (==) (after e)

maybeBool :: (a -> Bool) -> a -> Maybe a
maybeBool f a = if f a
                  then Just a
                  else Nothing

winnowOpcodes :: [Event] -> HashMap Int (HashSet OpCode)
winnowOpcodes = foldr updateOpcodes Map.empty

updateOpcodes :: Event ->
                 HashMap Int (HashSet OpCode) ->
                 HashMap Int (HashSet OpCode)
updateOpcodes e m =
    let
      codes = successfulOpcodes e
    in
      Map.insertWith Set.intersection (opNum e) (Set.fromList codes) m

opcodeMapping :: HashMap Int (HashSet OpCode) -> HashMap Int OpCode
opcodeMapping m =
    let
      usedOpcodes = Set.empty
      mapping = Map.empty
    in
      opcodeMapping' m usedOpcodes mapping
  where opcodeMapping' :: HashMap Int (HashSet OpCode) ->
                          HashSet OpCode ->
                          HashMap Int OpCode ->
                          HashMap Int OpCode
        opcodeMapping' m used res | Map.null m = res
                                  | otherwise =
          let
            (num, opcode) = knownOpCode m
            used' = Set.insert opcode used
          in
            opcodeMapping' (trimMap m num used')
                           used'
                           (Map.insert num opcode res)
        knownOpCode :: HashMap Int (HashSet OpCode) -> (Int, OpCode)
        knownOpCode = fmap (head . Set.toList) .
                      head .
                      filter ((== 1) .  Set.size . snd) .
                      Map.toList
        trimMap m n used = Map.map ((flip Set.difference) used) $
                           Map.delete n $
                           m

buildOpCodeMap :: [Event] -> HashMap Int OpCode
buildOpCodeMap = opcodeMapping . winnowOpcodes

day16p2 :: [T.Text] -> Either ParseError (HashMap Int OpCode)
day16p2 input = buildOpCodeMap <$> parseInput input

day16p2' :: [T.Text] -> HashMap Int OpCode -> Either ParseError T.Text
day16p2' input m =
    T.pack . show <$>
    getAnswer <$>
    (runScript m) <$>
    (sequence $ (parse commandParser "") . T.unpack <$> input)
  where commandParser = (,) <$> opParser <*> argParser
        getAnswer r = fetchAt r 0

runScript :: HashMap Int OpCode -> [Command] -> Register
runScript m cs = foldl' (executeCommand m) (mkRegister 0 0 0 0) cs

executeCommand :: HashMap Int OpCode -> Register -> Command -> Register
executeCommand m r (op, args) =
    let
      opcode = fromJust $ Map.lookup op m
    in
    fromJust $ execute r args (fn opcode)

