{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use all" #-}

module PebblingGame.TinyXorAnd where

import Control.Exception (assert)
import Data.Bits (Bits (popCount), bit, xor, (.&.), (.|.))
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap
import PebblingGame.Graph

data XorAnd
  = Not Int Int
  | Xor Int Int Int
  | And Int Int Int
  deriving (Eq, Show)

data PebblingGraph
  = PebblingGraph
  { pgSize :: Int,
    pgInputSize :: Int,
    pgOutputSize :: Int,
    -- Dependencies are one-hot bits -> bits
    pgDependencies :: IntMap Int,
    -- GateIDs are one-hot bits -> gate ID
    pgComputationIDs :: IntMap Int,
    pgComputations :: IntMap XorAnd,
    pgInputs :: [Int],
    pgOutputBits :: IntMap Int
  }
  deriving (Eq, Show)

newtype PebblingVertex = V Int deriving (Eq, Ord, Show)

newtype PebblingEdge = E (Int, Int) deriving (Eq, Ord, Show)

index :: PebblingVertex -> Int
index (V idx) = idx

instance Graph PebblingGraph where
  type Vertex PebblingGraph = PebblingVertex
  type Edge PebblingGraph = PebblingEdge
  edgesFrom g v =
    [ E (index v, index v `xor` b)
      | b <- map bit [0 .. pgSize g - 1],
        let depBits = pgDependencies g ! b
         in (index v .&. depBits) == depBits
    ]
  edgesTo g v = [E (b, a) | E (a, b) <- edgesFrom g v]
  vertexFrom _ (E (a, _)) = V a
  vertexTo _ (E (_, b)) = V b
  allVertexes g = [V i | i <- [0 .. bit (pgSize g) - 1]]

instance PrettyGraph PebblingGraph where
  prettyVertex graph (V idx) =
    "(" ++ show idx ++ ") " ++ show [c | (b, c) <- gateIDs, (idx .&. b) == b]
    where
      gateIDs = IntMap.toList (pgComputationIDs graph)

  prettyEdge graph (E (fromIdx, toIdx)) =
    ("(" ++ show fromIdx ++ " -> " ++ show toIdx ++ ") ")
      ++ (plusMinus ++ show [c | (b, c) <- gateIDs, (chIdx .&. b) == b])
    where
      plusMinus = if fromIdx < toIdx then "+" else "-"
      chIdx = fromIdx `xor` toIdx
      gateIDs = IntMap.toList (pgComputationIDs graph)

  prettyGraph graph = do
    concatMap
      (\v -> prettyVertex graph v : map (("  " ++) . prettyEdge graph) (edgesFrom graph v))
      (allVertexes graph)

edgePebbleCount :: PebblingGraph -> PebblingEdge -> Int
edgePebbleCount _ (E (_, b)) = popCount b

fromComputations :: [XorAnd] -> [Int] -> [Int] -> PebblingGraph
fromComputations computations ins outs =
  PebblingGraph
    { pgSize = length computations,
      pgInputSize = length ins,
      pgOutputSize = IntMap.size outputBits,
      pgDependencies = deps,
      pgComputationIDs =
        IntMap.fromList [(b, c) | (c, b) <- IntMap.toList gateBits, b /= 0],
      pgComputations = IntMap.fromList [(targetID c, c) | c <- computations],
      pgInputs = ins,
      pgOutputBits = outputBits
    }
  where
    outputBits = IntMap.fromList [(tgt, gateBits ! tgt) | tgt <- outs]

    -- Maps a pebbling bit -> the dependency bits that need to be set for it
    -- to be pebbled or unpebbled
    deps =
      assert
        -- assert all gate IDs have bits mappings, and are all distinct
        ( all (`IntMap.member` gateBits) (concatMap depIDs computations)
            && (length ins + length computations) == IntMap.size gateBits
        )
        $ IntMap.fromList
          [ ( gateBits ! targetID g,
              foldl' (.|.) 0 (map (gateBits !) (depIDs g))
            )
            | g <- computations
          ]

    -- Maps gate ID -> pebbling bits; for inputs these are 0 because pebbles
    -- are only for computations, not inputs
    -- Specifically, each mapped bit here is either in the form (bit i) or 0,
    -- so (hammingWeight (gateBits k)) is always either 1 or 0
    gateBits =
      IntMap.fromList
        ( map (\inID -> (inID, 0)) ins
            ++ [(targetID g, bit i) | (g, i) <- zip computations [0 ..]]
        )

    depIDs (Not x _) = [x]
    depIDs (Xor x y _) = [x, y]
    depIDs (And x y _) = [x, y]

    targetID (Not _ tgt) = tgt
    targetID (Xor _ _ tgt) = tgt
    targetID (And _ _ tgt) = tgt
