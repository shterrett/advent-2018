{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Data.Text as T
import Text.Parsec
import Text.Parser.Token (whiteSpace, integer)

type Nodes = [Integer]
type Metadata = [Integer]

parseInput :: [T.Text] -> Either ParseError [Integer]
parseInput = (parse listOfInts "") . T.unpack . head

listOfInts :: Parsec String () Nodes
listOfInts = many (whiteSpace >> integer)

extractMetadata :: Nodes -> Metadata
extractMetadata = fst . processMetadata . (,) []

processMetadata :: (Metadata, Nodes) -> (Metadata, Nodes)
processMetadata (md, (nodeC:metaC:ns))
  | nodeC == 0 = (md ++ take' metaC ns, drop' metaC ns)
  | otherwise =
    let
      (md', ns') = (processNodes nodeC) (md, ns)
    in
      (md' ++ take' metaC ns', drop' metaC ns')
  where processNodes c = foldr (.) id $ take' c (repeat processMetadata)

take' :: Integer -> [a] -> [a]
take' i = take (fromIntegral i)

drop' :: Integer -> [a] -> [a]
drop' i = drop (fromIntegral i)

day8 :: [T.Text] -> T.Text
day8 input = either (T.pack . show) (T.pack . show) $
             sum . extractMetadata <$>
             parseInput input
