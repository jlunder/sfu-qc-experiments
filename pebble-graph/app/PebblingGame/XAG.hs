{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PebblingGame.XAG
  ( XorAnd (..),
    XorAndGraph (..),
    xagFromLists,
  )
where

import Control.Exception (assert)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import PebblingGame.Pebbling

data XorAnd
  = Not Int Int
  | Xor Int Int Int
  | And Int Int Int
  deriving (Eq, Show)

xaResult :: XorAnd -> Int
xaResult (Not _ nID) = nID
xaResult (Xor _ _ nID) = nID
xaResult (And _ _ nID) = nID

xaDependencies :: XorAnd -> Set Int
xaDependencies (Not xID _) = Set.singleton xID
xaDependencies (Xor xID yID _) = Set.fromList [xID, yID]
xaDependencies (And xID yID _) = Set.fromList [xID, yID]

data XorAndGraph = XorAndGraph
  { xagNodes :: Map Int XorAnd,
    xagInputs :: [Int],
    xagOutputs :: [Int]
  }

xagFromLists :: [XorAnd] -> [Int] -> [Int] -> XorAndGraph
xagFromLists xas ins outs =
  -- Assert all outs, deps are computable (either in ins or results)
  -- Assert no cycles (nodes must have a topo order matching ID order)
  assert
    ( all
        (`Set.member` Set.fromList (ins ++ map xaResult xas))
        (outs ++ concatMap (Set.toList . xaDependencies) xas)
    )
    $ assert
      (all (\xa -> all (< xaResult xa) (xaDependencies xa)) xas)
    $ XorAndGraph
      { xagNodes = Map.fromList (zip (map xaResult xas) xas),
        xagInputs = ins,
        xagOutputs = outs
      }

instance ComputationGraph XorAndGraph where
  data Computation XorAndGraph = C Int deriving (Eq, Ord)
  data Result XorAndGraph = R Int deriving (Eq, Ord)
  inputs = map R . xagInputs
  intermediates dag = map (R . xaResult) (Map.elems (xagNodes dag))
  outputs = map R . xagOutputs
  requirements dag (C nID) = Set.map R (xaDependencies (xagNodes dag ! nID))
  results _ (C nID) = Set.singleton (R nID)
  satisfying _ (R nID) = Set.singleton (C nID)

  satisfiedBy dag res =
    Set.fromList
      [ C (xaResult xa)
        | xa <- Map.elems (xagNodes dag),
          xaDependencies xa `Set.isSubsetOf` resIDs
      ]
    where
      resIDs =
        Set.map (\(R nID) -> nID) res
          `Set.union` Set.fromList (xagInputs dag)

instance PrettyComputationGraph XorAndGraph where
  prettyComputation _ = show . (\(C nID) -> nID)
  prettyResult _ = show . (\(R nID) -> nID)
  prettyComputationGraph dag =
    [ "Pebbling computation:",
      "  Inputs: " ++ show (map (\(R nID) -> nID) (inputs dag)),
      "  Outputs: " ++ show (map (\(R nID) -> nID) (outputs dag))
    ]
      ++ map (("  " ++) . show) (Map.elems (xagNodes dag))
