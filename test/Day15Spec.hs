{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15Spec where

import Test.Hspec
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Day15

spec :: Spec
spec = do
    describe "parsing the cave" $ do
      it "creates a Game state with the correct positions" $ do
        let caveMap = [ "#######"
                      , "#.G.E.#"
                      , "#E.G.E#"
                      , "#.G.E.#"
                      , "#######"
                      ]
        let elfLocs = [(4, 1), (1, 2), (5, 2), (4, 3)]
        let goblinLocs = [(2, 1), (3, 2), (2, 3)]
        let walls = ((,0) <$> [0..6]) ++
                    ((,4) <$> [0..6]) ++
                    ((0,) <$> [0..4]) ++
                    ((6,) <$> [0..4])
        let open = [(1, 1), (3, 1), (5, 1), (2, 2), (4, 2), (1, 3), (3, 3), (5, 3)]
        let board = foldr Map.union Map.empty [
                        Map.fromList $ zip walls $ repeat Wall
                      , Map.fromList $ zip open $ repeat Open
                      , Map.fromList $ zip elfLocs $ repeat (Combatant 200)
                      , Map.fromList $ zip goblinLocs $ repeat (Combatant 200)
                      ]
        initGame caveMap `shouldBe` (Game 0
                                          (7, 5)
                                          (Set.fromList goblinLocs)
                                          (Set.fromList elfLocs)
                                          3
                                          Set.empty
                                          board)
    describe "attacking" $ do
      it "decrements the HP of the target by 3" $ do
        let game = Game 0
                        (1, 1)
                        Set.empty
                        (Set.singleton (1, 1))
                        3
                        Set.empty
                        (Map.singleton (1, 1) (Combatant 200))
        attack 3 (1, 1) game `shouldBe` (Game 0
                                            (1, 1)
                                            Set.empty
                                            (Set.singleton (1, 1))
                                            3
                                            Set.empty
                                            (Map.singleton (1, 1) (Combatant 197)))
      it "removes a piece from the board if the HP reaches 0" $ do
        let game = Game 0
                        (1, 1)
                        (Set.singleton (1, 1))
                        (Set.singleton (2, 1))
                        3
                        Set.empty
                        (Map.singleton (1, 1) (Combatant 3))
        attack 3 (1, 1) game `shouldBe` (Game 0
                                            (1, 1)
                                            Set.empty
                                            (Set.singleton (2, 1))
                                            3
                                            (Set.singleton (1, 1))
                                            (Map.singleton (1, 1) Open))
      it "removes a piece from the board if the HP drops below 0" $ do
        let game = Game 0
                        (1, 1)
                        (Set.singleton (1, 1))
                        Set.empty
                        3
                        Set.empty
                        (Map.singleton (1, 1) (Combatant 1))
        attack 3 (1, 1) game `shouldBe` (Game 0
                                            (1, 1)
                                            Set.empty
                                            Set.empty
                                            3
                                            (Set.singleton (1, 1))
                                            (Map.singleton (1, 1) Open))
    describe "choosePoint" $ do
      it "returns the point that comes first in 'reading order'" $ do
        let points = [ (2, 3)
                     , (6, 2)
                     , (5, 3)
                     , (4, 2)
                     ]
        choosePoint points `shouldBe` Just (4, 2)
    describe "target" $ do
      it "returns the next (in reading order) (enemy) target to attack, if one is in range" $ do
        let game = Game 0
                        (3, 3)
                        (Set.fromList [(2, 1), (1, 2)])
                        (Set.fromList [(2, 2)])
                        3
                        Set.empty
                        (Map.fromList [ ((2, 1), Combatant 200)
                                      , ((1, 2), Combatant 200)
                                      , ((2, 2), Combatant 200)
                                      ])
        target (2, 2) game `shouldBe` Just (2, 1)
        target (2, 1) game `shouldBe` Just (2, 2)
        target (1, 2) game `shouldBe` Just (2, 2)
      it "returns the enemy with the lowest hit points" $ do
        let game = Game 0
                        (3, 3)
                        (Set.fromList [(2, 1), (1, 2)])
                        (Set.fromList [(2, 2)])
                        3
                        Set.empty
                        (Map.fromList [ ((2, 1), Combatant 100)
                                      , ((1, 2), Combatant 50)
                                      , ((2, 2), Combatant 200)
                                      ])
        target (2, 2) game `shouldBe` Just (1, 2)
      it "handles combinations of lowest HP and reading order" $ do
        let game = Game 0
                        (3, 3)
                        (Set.fromList [(2, 1), (1, 2), (3, 2)])
                        (Set.fromList [(2, 2)])
                        3
                        Set.empty
                        (Map.fromList [ ((2, 1), Combatant 100)
                                      , ((1, 2), Combatant 50)
                                      , ((3, 2), Combatant 50)
                                      , ((2, 2), Combatant 200)
                                      ])
        target (2, 2) game `shouldBe` Just (1, 2)
    describe "destinations" $ do
      it "returns all open squares adjacent to enemeies" $ do
        let caveMap = [ "#######"
                      , "#.E..E#"
                      , "#.....#"
                      , "#...G.#"
                      , "#######"
                      ]
        let game = initGame caveMap
        destinations (2, 1) game `shouldBe` [(3, 3), (4, 2), (5, 3)]
        destinations (5, 1) game `shouldBe` [(3, 3), (4, 2), (5, 3)]
        destinations (4, 3) game `shouldBe` [ (1, 1), (3, 1), (2, 2)
                                       , (4, 1), (5, 2)
                                       ]
    describe "taking a step" $ do
      it "does nothing if there is an enemy in range" $ do
        let caveMap = [ "#######"
                      , "#.....#"
                      , "#.....#"
                      , "#..EG.#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (3, 3)
        takeStep elf game `shouldBe` (elf, game)
      it "takes one step along the shortest path to the nearest enemy" $ do
        let caveMap = [ "#######"
                      , "#....G#"
                      , "#.G...#"
                      , "#..E..#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (3, 3)
        let newGame = game { elves = Set.insert (3, 2) $
                                     Set.delete (3, 3) $
                                     elves game
                           , cave = Map.insert (3, 3) Open $
                                    Map.insert (3, 2) (Combatant 200) $
                                    cave game
                           }
        takeStep elf game `shouldBe` ((3, 2), newGame)
      it "takes a step on the shortest reading order path" $ do
        let startingMap = [ "#######"
                          , "#.E...#"
                          , "#.....#"
                          , "#...G.#"
                          , "#######"
                          ]
        let endingMap = [ "#######"
                        , "#..E..#"
                        , "#.....#"
                        , "#...G.#"
                        , "#######"
                        ]
        takeStep (2, 1) (initGame startingMap) `shouldBe` ((3, 1), (initGame endingMap))
      it "picks the same square as in the given example" $ do
        let startingMap = [ "#######"
                          , "#E..G.#"
                          , "#...#.#"
                          , "#.G.#G#"
                          , "#######"
                          ]
        let endingMap = [ "#######"
                        , "#.E.G.#"
                        , "#...#.#"
                        , "#.G.#G#"
                        , "#######"
                        ]
        takeStep (1, 1) (initGame startingMap) `shouldBe` ((2, 1), (initGame endingMap))
      it "chooses the target that with the square that is lowest in reading order" $ do
        let caveMap = [ "#######"
                      , "#E....#"
                      , "#...G.#"
                      , "#..G..#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (1, 1)
        let newGame = game { elves = Set.insert (2, 1) $
                                     Set.delete (1, 1) $
                                     elves game
                           , cave = Map.insert (1, 1) Open $
                                    Map.insert (2, 1) (Combatant 200) $
                                    cave game
                           }
        takeStep elf game `shouldBe` ((2, 1), newGame)
      it "chooses the square that is first in reading order when a single target has multiple equidistant squares" $ do
        let caveMap = [ "#######"
                      , "#..E..#"
                      , "#.....#"
                      , "#..#..#"
                      , "#..G..#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (3, 1)
        let newGame = game { elves = Set.insert (2, 1) $
                                     Set.delete (3, 1) $
                                     elves game
                           , cave = Map.insert (3, 1) Open $
                                    Map.insert (2, 1) (Combatant 200) $
                                    cave game
                           }
        takeStep elf game `shouldBe` ((2, 1), newGame)
      it "takes its first step in reading order when multiple equidistant paths exist" $ do
        let caveMap = [ "#######"
                      , "#E...##"
                      , "#....G#"
                      , "#.....#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (1, 1)
        let newGame = game { elves = Set.insert (2, 1) $
                                     Set.delete (1, 1) $
                                     elves game
                           , cave = Map.insert (1, 1) Open $
                                    Map.insert (2, 1) (Combatant 200) $
                                    cave game
                           }
        takeStep elf game `shouldBe` ((2, 1), newGame)
      it "choses the target square that is first in reading order when the first step is not first in reading order" $ do
        let caveMap = [ "#####"
                      , "###G#"
                      , "###.#"
                      , "#.E.#"
                      , "#.###"
                      , "#G###"
                      , "#####"
                      ]

        let game = initGame caveMap
        let elf = (2, 3)
        let newGame = game { elves = Set.insert (3, 3) $
                                     Set.delete (2, 3) $
                                     elves game
                           , cave = Map.insert (2, 3) Open $
                                    Map.insert (3, 3) (Combatant 200) $
                                    cave game
                           }
        takeStep elf game `shouldBe` ((3, 3), newGame)
      it "moves to the right instead of down" $ do
        let caveMap = [ "########"
                      , "###G...#"
                      , "###.G..#"
                      , "##.....#"
                      , "##G...##"
                      , "##..G.##"
                      , "#..GG.##"
                      , "#.....##"
                      , "#.....##"
                      , "#...E..#"
                      , "########"
                      ]
        let game = initGame caveMap
        let goblin = (3, 1)
        let newGame = game { goblins = Set.insert (4, 1) $
                                       Set.delete (3, 1) $
                                       goblins game
                           , cave = Map.insert (3, 1) Open $
                                    Map.insert (4, 1) (Combatant 200) $
                                    cave game
                           }
        takeStep goblin game `shouldBe` ((4, 1), newGame)
    describe "turn" $ do
      it "attacks immediately if within range" $ do
        let caveMap = [ "#######"
                      , "#.....#"
                      , "#.GE..#"
                      , "#.....#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let goblin = (2, 2)
        let newGame = game { cave = Map.insert (3, 2) (Combatant 197) (cave game) }
        turn game goblin `shouldBe` Right newGame
      it "ends the game when there are no enemies" $ do
        let caveMap = [ "#######"
                      , "#.....#"
                      , "#.G...#"
                      , "#.....#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let goblin = (2, 2)
        turn game goblin `shouldBe` Left game
      it "takes a step when no enemies are in range and attacks the resultant adjacent enemy" $ do
        let caveMap = [ "#######"
                      , "#....G#"
                      , "#.G...#"
                      , "#..E..#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (3, 3)
        let newGame = game { elves = Set.insert (3, 2) $
                                     Set.delete (3, 3) $
                                     elves game
                           , cave = Map.insert (3, 3) Open $
                                    Map.insert (3, 2) (Combatant 200) $
                                    Map.insert (2, 2) (Combatant 197) $
                                    cave game
                           }
        turn game elf `shouldBe` Right newGame
      it "takes a step when no enemies are in range and ends if no enemy is in range" $ do
        let caveMap = [ "#######"
                      , "#....G#"
                      , "#.....#"
                      , "#..E..#"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (3, 3)
        let newGame = game { elves = Set.insert (3, 2) $
                                     Set.delete (3, 3) $
                                     elves game
                           , cave = Map.insert (3, 3) Open $
                                    Map.insert (3, 2) (Combatant 200) $
                                    cave game
                           }
        turn game elf `shouldBe` Right newGame
      it "moves one step to the right" $ do
        let caveMap = [ "#######"
                      , "#######"
                      , "#.E..G#"
                      , "#.#####"
                      , "#G#####"
                      , "#######"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (2, 2)
        let newGame = game { elves = Set.insert (3, 2) $
                                     Set.delete (2, 2) $
                                     elves game
                           , cave = Map.insert (2, 2) Open $
                                    Map.insert (3, 2) (Combatant 200) $
                                    cave game
                           }
        turn game elf `shouldBe` Right newGame
      it "attacks the goblin" $ do
        let caveMap = [ "####"
                      , "#GG#"
                      , "#.E#"
                      , "####"
                      ]
        let game = initGame caveMap
        let elf = (2, 2)
        let newGame = game { cave = Map.insert (2, 1) (Combatant 197) $
                                    cave game
                           }
        turn game elf `shouldBe` Right newGame
      it "moves one step to the left" $ do
        let caveMap = [ "########"
                      , "#..E..G#"
                      , "#G######"
                      , "########"
                      ]
        let game = initGame caveMap
        let elf = (3, 1)
        let newGame = game { cave = Map.insert (3, 1) Open $
                                    Map.insert (2, 1) (Combatant 200) $
                                    cave game
                           , elves = Set.insert (2, 1) $
                                     Set.delete (3, 1) $
                                     elves game
                           }
        turn game elf `shouldBe` Right newGame
      it "moves to the right" $ do
        let caveMap = [ "#######"
                      , "#######"
                      , "#####G#"
                      , "#..E..#"
                      , "#G#####"
                      , "#######"
                      , "#######"
                      ]
        let game = initGame caveMap
        let elf = (3, 3)
        let newGame = game { cave = Map.insert (2, 3) (Combatant 200) $
                                    Map.insert (3, 3) Open $
                                    cave game
                           , elves = Set.insert (2, 3) $
                                     Set.delete (3, 3) $
                                     elves game
                           }
        turn game elf `shouldBe` Right newGame
    describe "round" $ do
      it "plays a single round of the game" $ do
        let caveMap = [ "#########"
                      , "#G..G..G#"
                      , "#.......#"
                      , "#.......#"
                      , "#G..E..G#"
                      , "#.......#"
                      , "#.......#"
                      , "#G..G..G#"
                      , "#########"
                      ]
        let game = initGame caveMap
        let newMap = [ "#########"
                     , "#.G...G.#"
                     , "#...G...#"
                     , "#...E..G#"
                     , "#.G.....#"
                     , "#.......#"
                     , "#G..G..G#"
                     , "#.......#"
                     , "#########"
                     ]
        let newGame = initGame newMap
        let newGame' = newGame { cave = Map.insert (4, 2)
                                                   (Combatant 197)
                                                   (cave newGame)
                               , Day15.round = 1
                               }
        playRound game `shouldBe` Right newGame'
    describe "play game" $ do
      it "correctly plays through a game" $ do
        let startingCave = [ "#######"
                           , "#G..#E#"
                           , "#E#E.E#"
                           , "#G.##.#"
                           , "#...#E#"
                           , "#...E.#"
                           , "#######"
                           ]
        let startingGame = initGame startingCave

        let endingCave = [ "#######"
                         , "#...#E#"
                         , "#E#...#"
                         , "#.E##.#"
                         , "#E..#E#"
                         , "#.....#"
                         , "#######"
                         ]
        let endingGame = initGame endingCave
        let endingGame' = endingGame { cave = Map.insert (1, 2) (Combatant 197) $
                                              Map.insert (2, 3) (Combatant 185) $
                                              (cave endingGame)
                                     , corpses = Set.singleton (1, 3)
                                     , Day15.round = 38
                                     }
        playGame startingGame `shouldBe` endingGame'
        score endingGame' `shouldBe` 36334

      it "correctly plays game 2" $ do
        let startingCave = [ "#######" 
                           , "#E..EG#" 
                           , "#.#G.E#" 
                           , "#E.##E#" 
                           , "#G..#.#" 
                           , "#..E#.#" 
                           , "#######"
                           ]
        let startingGame = initGame startingCave

        let endingCave = [ "#######"
                         , "#.E.E.#"
                         , "#.#E..#"
                         , "#E.##.#"
                         , "#.E.#.#"
                         , "#...#.#"
                         , "#######"
                         ]
        let endingGame = initGame endingCave
        let endingGame' = endingGame { cave = Map.insert (2, 1) (Combatant 164) $
                                              Map.insert (4, 1) (Combatant 197) $
                                              Map.insert (3, 2) (Combatant 200) $
                                              Map.insert (1, 3) (Combatant 98) $
                                              Map.insert (2, 4) (Combatant 200) $
                                              (cave endingGame)
                                     , corpses = Set.singleton (3, 1)
                                     , Day15.round = 47
                                     }
        playGame startingGame `shouldBe` endingGame'
        score endingGame' `shouldBe` 39514

      it "correctly plays game 3" $ do
        let startingCave = [ "#######"
                           , "#E.G#.#"
                           , "#.#G..#"
                           , "#G.#.G#"
                           , "#G..#.#"
                           , "#...E.#"
                           , "#######"
                           ]
        let startingGame = initGame startingCave

        let endingCave = [ "#######"
                         , "#G.G#.#"
                         , "#.#G..#"
                         , "#..#..#"
                         , "#...#G#"
                         , "#...G.#"
                         , "#######"
                         ]
        let endingGame = initGame endingCave
        let endingGame' = endingGame { cave = Map.insert (1, 1) (Combatant 200) $
                                              Map.insert (3, 1) (Combatant 98) $
                                              Map.insert (3, 2) (Combatant 200) $
                                              Map.insert (5, 4) (Combatant 95) $
                                              Map.insert (4, 5) (Combatant 200) $
                                              (cave endingGame)
                                     , corpses = Set.singleton (5, 5)
                                     , Day15.round = 36
                                     }
        playGame startingGame `shouldBe` endingGame'
        score endingGame' `shouldBe` 27755

      it "correctly plays game 4" $ do
        let startingCave = [ "#######"
                           , "#.E...#"
                           , "#.#..G#"
                           , "#.###.#"
                           , "#E#G#G#"
                           , "#...#G#"
                           , "#######"
                           ]
        let startingGame = initGame startingCave

        let endingCave = [ "#######"
                         , "#.....#"
                         , "#.#G..#"
                         , "#.###.#"
                         , "#.#.#.#"
                         , "#G.G#G#"
                         , "#######"
                         ]
        let endingGame = initGame endingCave
        let endingGame' = endingGame { cave = Map.insert (3, 2) (Combatant 200) $
                                              Map.insert (1, 5) (Combatant 98) $
                                              Map.insert (3, 5) (Combatant 38) $
                                              Map.insert (5, 5) (Combatant 200) $
                                              (cave endingGame)
                                     , corpses = Set.singleton (2, 5)
                                     , Day15.round = 55
                                     }
        playGame startingGame `shouldBe` endingGame'
        score endingGame' `shouldBe` 28944

      it "correctly plays game 5" $ do
        let startingCave = [ "#########"
                           , "#G......#"
                           , "#.E.#...#"
                           , "#..##..G#"
                           , "#...##..#"
                           , "#...#...#"
                           , "#.G...G.#"
                           , "#.....G.#"
                           , "#########"
                           ]
        let startingGame = initGame startingCave

        let endingCave = [ "#########"
                         , "#.G.....#"
                         , "#G.G#...#"
                         , "#.G##...#"
                         , "#...##..#"
                         , "#.G.#...#"
                         , "#.......#"
                         , "#.......#"
                         , "#########"
                         ]
        let endingGame = initGame endingCave
        let endingGame' = endingGame { cave = Map.insert (2, 1) (Combatant 137) $
                                              Map.insert (1, 2) (Combatant 200) $
                                              Map.insert (3, 2) (Combatant 200) $
                                              Map.insert (2, 3) (Combatant 200) $
                                              Map.insert (2, 5) (Combatant 200) $
                                              (cave endingGame)
                                     , corpses = Set.singleton (2, 2)
                                     , Day15.round = 21
                                     }
        playGame startingGame `shouldBe` endingGame'
        score endingGame' `shouldBe` 18740
    describe "guarantee elf win" $ do
      it "correctly plays through game 1" $ do
        let startingCave = [ "#######"
                          , "#.G...#"
                          , "#...EG#"
                          , "#.#.#G#"
                          , "#..G#E#"
                          , "#.....#"
                          , "#######"
                          ]

        let startingGame = initGame startingCave

        let endingCave = [ "#######"
                        , "#..E..#"
                        , "#...E.#"
                        , "#.#.#.#"
                        , "#...#.#"
                        , "#.....#"
                        , "#######"
                        ]
        let endingGame = initGame endingCave
        let endingGame' = endingGame { Day15.round = 30
                                     , cave = Map.insert (3, 1) (Combatant 158) $
                                              Map.insert (4, 2) (Combatant 14) $
                                              (cave endingGame)
                                     , elfAttk = 15
                                     , corpses = Set.singleton (3, 2)
                                     }
        makeAllElvesLive startingGame `shouldBe` endingGame'
        score endingGame' `shouldBe` 4988
