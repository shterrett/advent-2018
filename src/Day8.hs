{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Text.Parsec hiding (State)
import Text.Parser.Token (whiteSpace, integer)
import Control.Monad.State.Lazy

type Metadata = [Integer]
data Node = Node { children :: [Node]
                 , metadata :: [Integer]
                 }
                 deriving (Show, Eq)

parseInput :: [T.Text] -> Either ParseError [Integer]
parseInput = (parse listOfInts "") . T.unpack . head

listOfInts :: Parsec String () [Integer]
listOfInts = many (whiteSpace >> integer)

extractMetadata :: Node -> Metadata
extractMetadata (Node [] m) = m
extractMetadata (Node ns m) = m ++ (foldr (++) [] $ extractMetadata <$> ns)

buildTree' :: State [Integer] Node
buildTree' = state buildTree

buildTree :: [Integer] -> (Node, [Integer])
buildTree (nodeC:metaC:ds)
  | nodeC == 0 = (Node [] (take' metaC ds), drop' metaC ds)
  | otherwise = let
    (ns, ds') = buildChildren nodeC ds
    in
      (Node ns (take' metaC ds'), drop' metaC ds')

buildChildren :: Integer -> [Integer] -> ([Node], [Integer])
buildChildren nodeC = runState (replicateM (fromIntegral nodeC) buildTree')

take' :: Integer -> [a] -> [a]
take' i = take (fromIntegral i)

drop' :: Integer -> [a] -> [a]
drop' i = drop (fromIntegral i)

day8 :: [T.Text] -> T.Text
day8 input = either (T.pack . show) (T.pack . show) $
             sum . extractMetadata . evalState buildTree' <$>
             parseInput input

nodeValue :: Node -> Integer
nodeValue (Node [] md) = sum md
nodeValue (Node cs md) =
    let
      cm = Map.fromList $ zip [1..] cs
    in
      foldr (+) 0 $ fmap (nodeValue . findChild cm) md
  where findChild :: HashMap Integer Node -> Integer -> Node
        findChild m = fromMaybe (Node [] []) . (flip Map.lookup) m

day8p2 :: [T.Text] -> T.Text
day8p2 input = either (T.pack . show) (T.pack . show) $
               nodeValue . evalState buildTree' <$>
               parseInput input
