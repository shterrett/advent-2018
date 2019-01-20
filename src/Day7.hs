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
import Data.List (elemIndex, sort, maximumBy)
import Control.Arrow ((&&&))
import Text.Parsec

type Node = Char
type Edge = (Node, Node)
type Graph = HashMap Node [Node]
type Seconds = Int
type Workers = [Seconds]

data Task = Task { startsAt :: Seconds, node :: Node }
          deriving (Show)
instance Eq Task where
    (==) (Task _ n1) (Task _ n2) = n1 == n2
instance Ord Task where
    compare (Task s1 n1) (Task s2 n2) = case compare s1 s2 of
                                          EQ -> compare n1 n2
                                          r -> r

data SortState = SortState { graph :: Graph
                           , edgeCount :: HashMap Node Integer
                           , availableNodes :: MinHeap Task
                           , reversedSort :: [Node]
                           , workers :: Workers
                           , nextStart :: Seconds
                           , costFn :: Node -> Seconds
                           }

initState :: Int -> Seconds -> [Edge] -> SortState
initState wc base es =
    let
      graph = buildGraph es
      edgeCount = buildEdgeCount es
      sources = findSources graph edgeCount
      workers = take wc $ repeat 0
    in SortState { graph = graph
                 , edgeCount = edgeCount
                 , availableNodes = Heap.fromList $ fmap (Task 0) sources
                 , reversedSort = []
                 , workers = workers
                 , nextStart = 0
                 , costFn = taskDuration base
                 }

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

topologicalSort :: SortState -> [Node]
topologicalSort s = reverse . reversedSort $ doSort s

doSort :: SortState -> SortState
doSort s | Heap.isEmpty (availableNodes s) = s
         | otherwise = fromMaybe s $ (doSort .
                                      updateHeap .
                                      pruneEdges .
                                      updateWorkers .
                                      pushNode s
                                     ) <$>
                                     (Heap.view (availableNodes s))

pushNode :: SortState -> (Task, MinHeap Task) -> (Task, SortState)
pushNode s (t, h) = (t, s { reversedSort = (node t):(reversedSort s)
                          , availableNodes = h
                          })

updateWorkers :: (Task, SortState) -> (Task, SortState)
updateWorkers (t, s) =
    let
      ws = sort $ workers s
      taskTime = costFn s $ node t
      taskStart = max (startsAt t) $ head ws
    in
      ( t
      , s { nextStart = taskTime + taskStart
          , workers = (taskTime + taskStart) : (tail ws)
          }
      )

pruneEdges :: (Task, SortState) -> SortState
pruneEdges (t, s) =
    let neighbors = Map.lookup (node t) (graph s) in
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
        , availableNodes = foldr Heap.insert (availableNodes s) $
                                             fmap (Task (nextStart s)) sources
        }
  where splitEdgeCount = (Map.keys . Map.filter (== 0)) &&& (Map.filter (/= 0))

day7 :: [T.Text] -> T.Text
day7 lines = either (T.pack . show) T.pack $
              topologicalSort <$>
              initState 1 0 <$>
              parseInput lines

taskDuration :: Seconds -> Node -> Seconds
taskDuration base n = base + (cost n)
  where cost n = fromMaybe 0 $ (+1) <$> elemIndex n "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

workTasks :: SortState -> Seconds
workTasks s = maximumBy compare $ workers $ doSort s

day7p2 :: Int -> Seconds -> [T.Text] -> T.Text
day7p2 wc base lines = either (T.pack . show) (T.pack . show) $
                        workTasks <$>
                        initState wc base <$>
                        parseInput lines
