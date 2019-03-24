{-# LANGUAGE TupleSections #-}

module Day15 where

import qualified Data.Text as T
import Data.List (minimumBy, sortBy)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import Control.Monad (foldM)
import Control.Arrow ((&&&))


type Point = (Int, Int)
type Bound = (Int, Int)
type HP = Int
data Square = Open
            | Combatant HP
            | Wall
            deriving (Show, Eq)

data Game =
    Game { round :: Int
         , bound :: Bound
         , goblins :: HashSet Point
         , elves :: HashSet Point
         , elfAttk :: HP
         , corpses :: HashSet Point
         , cave :: HashMap Point Square
         }
         deriving (Show, Eq)

insertSquare :: Point -> Square -> Game -> Game
insertSquare p s g = g { cave = Map.insert p s (cave g) }

insertElf :: Point -> Game -> Game
insertElf p g = g { elves = Set.insert p (elves g) }

insertGoblin :: Point -> Game -> Game
insertGoblin p g = g { goblins = Set.insert p (goblins g) }

isElf :: Game -> Point -> Bool
isElf g p = Set.member p $ elves g

updatePlayer :: Point -> Point -> Game -> Game
updatePlayer p p' g =
    let
      s = fromJust $ Map.lookup p (cave g)
      g' = insertSquare p Open $
           insertSquare p' s g
    in
      if isElf g p
        then insertElf p' $ g' { elves = Set.delete p (elves g') }
        else insertGoblin p' $ g' { goblins = Set.delete p (goblins g') }

lookupSquare :: Point -> Game -> Square
lookupSquare p g = fromJust $ Map.lookup p (cave g)

emptyGame :: Point -> Game
emptyGame b = Game { Day15.round = 0
                   , bound = b
                   , goblins = Set.empty
                   , elves = Set.empty
                   , elfAttk = 3
                   , corpses = Set.empty
                   , cave = Map.empty
                   }

labelPoints :: [[a]] -> [((Int, Int), a)]
labelPoints rs = concat $ labelRow <$> zip [0, 1..] rs
  where labelRow (n, ps) = zip ((, n) <$> [0, 1..]) ps

initGame :: [String] -> Game
initGame m =
    let
      bounds = (length $ head m, length m)
    in
    foldr updateGameStart (emptyGame bounds) $ labelPoints m
  where updateGameStart :: ((Int, Int), Char) -> Game -> Game
        updateGameStart (p, c) g =
          case c of
            '#' -> insertSquare p Wall g
            '.' -> insertSquare p Open g
            'E' -> insertElf p $ insertSquare p (Combatant 200) g
            'G' -> insertGoblin p $ insertSquare p (Combatant 200) g

adjacentPoints :: Bound -> Point -> [Point]
adjacentPoints b (x, y) = filter (inBounds b) [ (x - 1, y)
                                              , (x, y - 1)
                                              , (x + 1, y)
                                              , (x, y + 1)
                                              ]
  where inBounds (xMax, yMax) (x, y) = x >= 0 && y >= 0 && x < xMax && y < yMax

readingOrder :: Point -> Point -> Ordering
readingOrder (x1, y1) (x2, y2) = case compare y1 y2 of
                                   EQ -> compare x1 x2
                                   o -> o

choosePoint :: [Point] -> Maybe Point
choosePoint [] = Nothing
choosePoint ps = Just $ minimumBy readingOrder ps

containsEnemy :: HashSet Point -> [Point] -> [Point]
containsEnemy es ps = filter ((flip Set.member) es) ps

enemies :: Point -> Game -> HashSet Point
enemies p g =
    if isElf g p
      then goblins g
      else elves g

target :: Point -> Game -> Maybe Point
target p g =
    let
      es = enemies p g
    in
      choosePoint $
      minHP (cave g) $
      containsEnemy es $
      adjacentPoints (bound g) p
  where minHP c es =
          let
            hp = minimumBy compare $ fmap (combatantHP c) es
          in
            filter (\p -> hp == combatantHP c p) es

combatantHP :: HashMap Point Square -> Point -> Int
combatantHP c p = case Map.lookup p c of
                    Just (Combatant hp) -> hp

attack :: HP -> Point -> Game -> Game
attack v p g =
    let
      (Combatant hp) = lookupSquare p g
      hp' = hp - v
    in
      if hp' > 0
        then woundCombatant p hp' g
        else killCombatant p g
  where woundCombatant :: Point -> HP -> Game -> Game
        woundCombatant p hp g = g {
          cave = Map.insert p (Combatant hp) (cave g)
        }
        killCombatant :: Point -> Game -> Game
        killCombatant p g = g {
          cave = Map.insert p Open (cave g)
        , elves = Set.delete p (elves g)
        , goblins = Set.delete p (goblins g)
        , corpses = Set.insert p (corpses g)
        }

destinations :: Point -> Game -> [Point]
destinations p g =
    let
      es = Set.toList $ enemies p g
    in
      filter (isOpen g) $ mconcat $ adjacentPoints (bound g) <$> es

isOpen :: Game -> Point -> Bool
isOpen g p = lookupSquare p g == Open

turn :: Game -> Point -> Either Game Game
turn g p = if isDead p g then Right g
                         else turn' g p
  where isDead p g = Set.member p (corpses g)

turn' :: Game -> Point -> Either Game Game
turn' g p =
    if Set.null (enemies p g)
      then Left g
      else Right $ attackTarget $ takeStep p g

takeStep :: Point -> Game -> (Point, Game)
takeStep p g = case target p g of
                 Nothing -> takeStep' p g
                 (Just _) -> (p, g)

takeStep' :: Point -> Game -> (Point, Game)
takeStep' p g =
    let
      result = dijkstra $ initDijkstra p g
      p' = head <$>
           path (predecessors result) <$>
           (nearestPoint (destinations p g) result)
    in
      case p' of
        (Just p') -> (p', updatePlayer p p' g)
        Nothing -> (p, g)

attackTarget :: (Point, Game) -> Game
attackTarget (p, g) = case target p g of
                        Nothing -> g
                        (Just t) -> attack (attackValue p g) t g
  where attackValue p g = if (Set.member p $ elves g)
                            then elfAttk g
                            else 3

playRound :: Game -> Either Game Game
playRound g =
    let
      r = 1 + Day15.round g
      g' = g { Day15.round = 1 + (Day15.round g)
             , corpses = Set.empty
             }
      cs = combatants g
    in
      foldM turn g' cs

combatants :: Game -> [Point]
combatants g =
  sortBy readingOrder $
  fmap fst $
  filter (\(_, s) -> case s of
                       (Combatant _) -> True
                       _ -> False
         ) $
  Map.toList $
  cave g

playGame :: Game -> Game
playGame g = case playRound g of
               Right g -> playGame g
               Left g -> g

totalHP :: Game -> Int
totalHP g =
    let
      combatants = Set.toList $ Set.union (elves g) (goblins g)
    in
      sum $ fmap (combatantHP (cave g)) combatants

score :: Game -> Int
score g = (Day15.round g - 1) * (totalHP g)

day15 :: [T.Text] -> T.Text
day15 input =
    let
      final = playGame $ initGame (T.unpack <$> input)
    in
      T.pack $ show $ score final

makeAllElvesLive :: Game -> Game
makeAllElvesLive initial =
    let
      final = playGame initial
    in
      if elfDied initial final
        then makeAllElvesLive $ incAttack initial
        else final
  where elfDied initial final  = Set.size (elves final) < Set.size (elves initial)
        incAttack g = g { elfAttk = 1 + (elfAttk g) }

day15p2 :: [T.Text] -> T.Text
day15p2 input =
    let
      final = makeAllElvesLive $ initGame (T.unpack <$> input)
    in
      T.pack $ show $ score final

--------------------------------------------------------------------------------
-- Dijkstra Implementation

data Predecessor = Predecessor { point :: Point
                               , predecessor :: Maybe Point
                               , firstStep :: Point
                               , cost :: Int
                               }
                               deriving (Show, Eq)
instance Ord Predecessor where
    compare (Predecessor p1 _ f1 c1) (Predecessor p2 _ f2 c2) =
      case compare c1 c2 of
        EQ -> case readingOrder f1 f2 of
                EQ -> readingOrder p1 p2
                o -> o
        o -> o

data Dijkstra = Dijkstra { minHeap :: MinHeap Predecessor
                         , predecessors :: HashMap Point Predecessor
                         , game :: Game
                         }

initDijkstra :: Point -> Game -> Dijkstra
initDijkstra start g =
    let
      start' = Predecessor start Nothing start 0
      firstSteps = fmap (id &&& firstStep start) $
                filter (isOpen g) $
                adjacentPoints (bound g) start
    in
      Dijkstra (Heap.fromList (snd <$> firstSteps))
               (Map.fromList $ [(start, start')] ++ firstSteps)
               g
  where firstStep start p = Predecessor p (Just start) p 1

dijkstra :: Dijkstra -> Dijkstra
dijkstra d@(Dijkstra h ps g) =
    case Heap.view h of
      Nothing -> d
      (Just (pt, h')) -> dijkstra $ visit (Dijkstra h' ps g) pt

visit :: Dijkstra -> Predecessor -> Dijkstra
visit d@(Dijkstra h ps g) p =
    let
      ns = neighbors d p
      predecessors = fmap (mkPredecessor p) ns
    in
      dijkstra $ Dijkstra (foldr Heap.insert h predecessors)
                          (Map.union (Map.fromList (zip ns predecessors)) ps)
                          g
  where mkPredecessor p pt = Predecessor pt (Just $ point p) (firstStep p) (1 + cost p)

neighbors :: Dijkstra -> Predecessor -> [Point]
neighbors (Dijkstra _ ps g) p = filter (notSeen ps) $
                                filter (isOpen g) $
                                adjacentPoints (bound g) (point p)
  where notSeen ps pt = Map.lookup pt ps == Nothing

path :: HashMap Point Predecessor -> Point -> [Point]
path preds p = path' preds p []

path' :: HashMap Point Predecessor -> Point -> [Point] -> [Point]
path' preds p ps =
    let
      pred = fromJust $ Map.lookup p preds
    in
      case predecessor pred of
        Nothing -> ps
        (Just p') -> path' preds p' (p:ps)

nearestPoint :: [Point] -> Dijkstra -> Maybe Point
nearestPoint destinations (Dijkstra _ preds _) =
    let
      preds' = mapMaybe ((flip Map.lookup) preds) destinations
    in
      case preds' of
        [] -> Nothing
        preds' -> Just $ point $ minimumBy closestTarget $ preds'
  where closestTarget (Predecessor p1 _ _ c1) (Predecessor p2 _ _ c2) =
          case compare c1 c2 of
            EQ -> readingOrder p1 p2
            o -> o
