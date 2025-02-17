{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Set ((\\))
import Data.Set qualified as Set
import PebblingGame.AStar
import PebblingGame.FloydWarshall
import PebblingGame.Pebbling
import PebblingGame.TinyIrreversible
import PebblingGame.XAG

trivialPeb :: TIStates XorAndGraph
trivialPeb =
  fromComputations
    ( xagFromLists
        [ And 10 11 12,
          Xor 10 12 13
        ]
        [10, 11]
        [13]
    )

epflFig2Peb :: TIStates XorAndGraph
epflFig2Peb =
  fromComputations
    ( xagFromLists
        [ And 2 3 11,
          And 11 3 12,
          And 3 4 13,
          And 13 3 14,
          And 12 14 21,
          And 1 11 22
        ]
        [1, 2, 3, 4]
        [21, 22]
    )

testPebbling :: TIStates XorAndGraph -> (TIStates XorAndGraph -> Maybe ([Vertex (TIStates XorAndGraph)], Int)) -> IO ()
testPebbling graph pathF = do
  for_ (prettyComputationGraph cmpts) putStrLn
  case pathF graph of
    Just (path, maxPebbles) -> do
      putStrLn ("Minimum pebbling found with " ++ show maxPebbles ++ " pebbles")
      putStrLn "Sequence:"
      for_
        path
        ( \v -> do
            putStrLn ("  " ++ prettyVertex graph v)
        )
    Nothing -> putStrLn "No pebbling found"
  where
    cmpts = tiComputations graph

doFloydWarshall :: (ComputationGraph d) => TIStates d -> Maybe ([Vertex (TIStates d)], Int)
doFloydWarshall graph =
  floydWarshallMinimaxShortestPath graph initV goalV (toPebbleCount graph)
  where
    initV = vertexFromResultState Set.empty
    goalV = vertexFromResultState (Set.fromList (outputs cmpts))
    cmpts = tiComputations graph

doAStarWithPebbleLimit :: (ComputationGraph d) => Int -> TIStates d -> Maybe ([Vertex (TIStates d)], Int)
doAStarWithPebbleLimit limit graph =
  -- aStarShortestPath :: (Ord c) => c -> v -> (v -> Bool) -> (v -> [(e, v)]) -> (c -> v -> c) -> (c -> v -> e -> c) -> Maybe ([v], c)
  aStarShortestPath
    (0 :: Int, 0 :: Int)
    (vertexFromResultState Set.empty)
    goalF
    edgeF
    heuristicF
    costF
    >>= (\(path, (pebbles, _)) -> Just (path, pebbles))
  where
    goalF v = goalRs == resultStateFromVertex v
    edgeF v =
      [ (edge, vertexTo graph edge)
        | edge <- edgesFrom graph v,
          toPebbleCount graph edge <= limit
      ]
    heuristicF (peb, dist) v =
      ( max peb goalRsSize,
        dist + Set.size (goalRs \\ vRs) + Set.size (vRs \\ goalRs)
      )
      where
        vRs = resultStateFromVertex v
    costF (peb, dist) _ e = (max peb (toPebbleCount graph e), dist + 1)
    goalRsSize = Set.size goalRs
    goalRs = Set.fromList (outputs cmpts)
    cmpts = tiComputations graph

main :: IO ()
main = do
  mapM_ putStrLn (prettyStateGraph trivialPeb)
  testPebbling trivialPeb doFloydWarshall
  testPebbling trivialPeb (doAStarWithPebbleLimit 0)
  testPebbling trivialPeb (doAStarWithPebbleLimit 1)
  testPebbling trivialPeb (doAStarWithPebbleLimit 2)
  testPebbling trivialPeb (doAStarWithPebbleLimit 3)
  testPebbling epflFig2Peb doFloydWarshall
  testPebbling epflFig2Peb (doAStarWithPebbleLimit 4)
