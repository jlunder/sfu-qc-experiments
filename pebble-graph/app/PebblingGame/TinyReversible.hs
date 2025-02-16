{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use all" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PebblingGame.TinyReversible
  ( TIStates (..),
    toPebbleCount,
    fromComputations,
  )
where

import Data.Set (Set, (\\))
import Data.Set qualified as Set
import PebblingGame.Pebbling

newtype (ComputationGraph d) => TIStates d
  = TIStates
  { tiComputations :: d
  }

deriving instance (Eq (Result d), Ord (Result d)) => Eq (Vertex (TIStates d))

deriving instance (Eq (Result d), Ord (Result d)) => Ord (Vertex (TIStates d))

deriving instance (Eq (Result d), Ord (Result d)) => Eq (Edge (TIStates d))

deriving instance (Eq (Result d), Ord (Result d)) => Ord (Edge (TIStates d))

instance (Eq (Result d), Ord (Result d), ComputationGraph d) => StateGraph (TIStates d) where
  data Vertex (TIStates d) = V (Set (Result d))
  data Edge (TIStates d) = E (Set (Result d), Set (Result d))

  edgesFrom graph v =
    [ E (avail, (avail `Set.union` reached) \\ (avail `Set.intersection` reached))
      | reached <- map (results cmpts) (Set.toList (satisfiedBy cmpts avail))
    ]
    where
      avail = available v
      cmpts = tiComputations graph

  edgesTo graph v = [E (b, a) | E (a, b) <- edgesFrom graph v]
  vertexFrom _ (E (a, _)) = V a
  vertexTo _ (E (_, b)) = V b
  allVertexes graph = map V (allResults graph)

available :: Vertex (TIStates d) -> Set (Result d)
available (V idx) = idx

allResults :: (ComputationGraph d) => TIStates d -> [Set (Result d)]
allResults graph =
  Set.toList
    ( Set.powerSet
        ( Set.fromList (intermediates cmpts ++ outputs cmpts)
            \\ Set.fromList (inputs cmpts)
        )
    )
  where
    cmpts = tiComputations graph

instance (ComputationGraph d, PrettyComputationGraph d) => PrettyStateGraph (TIStates d) where
  prettyVertex graph (V res) = show (map (prettyResult (tiComputations graph)) (Set.toList res))

  prettyEdge graph (E (from, to)) =
    prettyResList (Set.toList from) ++ " -> " ++ prettyResList (Set.toList to) ++ "; " ++ addStr ++ remStr
    where
      addStr = if null added then "" else " +" ++ prettyResList added
      remStr = if null removed then "" else " -" ++ prettyResList removed
      added = Set.toList (to \\ from)
      removed = Set.toList (from \\ to)
      prettyResList = show . map prettyThisRes
      prettyThisRes = prettyResult (tiComputations graph)

  prettyStateGraph graph = do
    concatMap
      (\v -> prettyVertex graph v : map (("  " ++) . prettyEdge graph) (edgesFrom graph v))
      (allVertexes graph)

toPebbleCount :: TIStates d -> Edge (TIStates d) -> Int
toPebbleCount _ (E (_, b)) = Set.size b

fromComputations :: (ComputationGraph d) => d -> TIStates d
fromComputations cmpts = TIStates {tiComputations = cmpts}
