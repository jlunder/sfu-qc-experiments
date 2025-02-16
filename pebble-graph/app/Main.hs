{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Set qualified as Set
import PebblingGame.FloydWarshall
import PebblingGame.Pebbling
import PebblingGame.TinyIrreversible
import PebblingGame.XAG

testPebbling :: TIStates XorAndGraph -> IO ()
testPebbling graph = do
  for_ (prettyComputationGraph cmpts) putStrLn
  let initV = vertexFromResultState Set.empty
      goalV = vertexFromResultState (Set.fromList (outputs cmpts))
      result = floydWarshallMinimaxShortestPath graph initV goalV (toPebbleCount graph)
  case result of
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

main :: IO ()
main = do
  mapM_ putStrLn (prettyStateGraph trivialPeb)
  testPebbling trivialPeb
  testPebbling epflFig2Peb
