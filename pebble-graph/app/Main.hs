{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use all" #-}

module Main (main) where

import Data.Bits ((.|.))
import Data.Foldable (Foldable (foldl'), for_)
import Data.IntMap.Strict qualified as IntMap
import PebblingGame.FloydWarshall
import PebblingGame.Graph
import PebblingGame.TinyXorAnd

testPebbling :: PebblingGraph -> IO ()
testPebbling graph = do
  -- printGraph g
  putStrLn "Pebbling computation:"
  putStrLn ("  Inputs: " ++ show (pgInputs graph))
  putStrLn ("  Outputs: " ++ show (IntMap.keys (pgOutputBits graph)))
  for_ (pgComputations graph) (\c -> putStrLn ("  " ++ show c))
  let initV = V 0
      goalV = V (foldl' (.|.) 0 (IntMap.elems (pgOutputBits graph)))
      result = floydWarshallMinimaxShortestPath graph initV goalV (edgePebbleCount graph)
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

trivialPeb :: PebblingGraph
trivialPeb =
  fromComputations
    [ And 10 11 12,
      Xor 10 12 13
    ]
    [10, 11]
    [13]

epflFig2Peb :: PebblingGraph
epflFig2Peb =
  fromComputations
    [ And 2 3 11,
      And 11 3 12,
      And 3 4 13,
      And 13 3 14,
      And 12 14 21,
      And 1 11 22
    ]
    [1, 2, 3, 4]
    [21, 22]

main :: IO ()
main = do
  testPebbling trivialPeb
  testPebbling epflFig2Peb
