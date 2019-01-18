{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Text.Parsec

type Node = Char
type Edge = (Node, Node)
type Graph = HashMap Node [Node]

data SortState = SortState { graph :: Graph
                           , edgeCount :: HashMap Node Integer
                           , availableNodes :: MinHeap Node
                           , reversedSort :: [Node]
                           }

initState :: [Edge] -> SortState
initState es =
    let
      graph = buildGraph es
      edgeCount = buildEdgeCount es
      sources = Heap.fromList $ findSources graph edgeCount
    in SortState graph edgeCount sources []

findSources :: Graph -> HashMap Node Integer -> [Node]
findSources g ns = Set.toList $ Set.difference (keySet g) (keySet ns)
  where keySet = Set.fromList . Map.keys

parseInput :: [T.Text] -> Either ParseError [Edge]
parseInput = sequence . fmap ((parse edgeParser "") . T.unpack)
  where edgeParser = (,) <$>
                     (string "Step " >> anyChar) <*>
                     (string " must be finished before step " >> anyChar)

buildGraph :: [Edge] -> Graph
buildGraph = foldr appendNode Map.empty
  where appendNode :: Edge -> Graph -> Graph
        appendNode (f, t) g = Map.insertWith mappend f [t] g

buildEdgeCount :: [Edge] -> HashMap Node Integer
buildEdgeCount = foldr incNode Map.empty
  where incNode :: Edge -> HashMap Node Integer -> HashMap Node Integer
        incNode (_, t) g = Map.insertWith (+) t 1 g

topologicalSort :: [Edge] -> [Node]
topologicalSort es = let s = initState es in
  reverse . reversedSort $ doSort s

doSort :: SortState -> SortState
doSort s | Heap.isEmpty (availableNodes s) = s
         | otherwise = fromMaybe s $ (doSort .
                                      updateHeap .
                                      pruneEdges .
                                      pushNode s
                                     ) <$>
                                     (Heap.view (availableNodes s))

pushNode :: SortState -> (Node, MinHeap Node) -> (Node, SortState)
pushNode s (n, h) = (n, s { reversedSort = n:(reversedSort s)
                          , availableNodes = h
                          })

pruneEdges :: (Node, SortState) -> SortState
pruneEdges (n, s) =
    let neighbors = Map.lookup n (graph s) in
      fromMaybe s (decrementEdges s <$> neighbors)
  where decrementEdges s ns = s {
      edgeCount = foldr (Map.adjust (\c -> c - 1)) (edgeCount s) ns
    }

updateHeap :: SortState -> SortState
updateHeap s =
    let
      (sources, rest) = splitEdgeCount (edgeCount s)
    in
      s { edgeCount = rest
        , availableNodes = foldr Heap.insert (availableNodes s) sources
        }
  where splitEdgeCount = (Map.keys . Map.filter (== 0)) &&& (Map.filter (/= 0))

day7 :: [T.Text] -> T.Text
day7 lines = either (T.pack . show) T.pack $
              topologicalSort <$>
              parseInput lines
