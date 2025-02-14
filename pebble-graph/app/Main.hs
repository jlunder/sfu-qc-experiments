{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Exception (assert)
import Data.Bits (Bits (popCount), bit, xor, (.&.))
import Data.Foldable (foldl', for_)
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (mapMaybe)

data Gate
  = Input Int
  | CNot Int Int Int
  | Tof Int Int Int
  deriving (Eq, Show)

class WeightedGraph g where
  type VertexID g :: Type
  type EdgeID g :: Type
  type EdgeWeight g :: Type
  edgesFrom :: g -> VertexID g -> [EdgeID g]
  edgesTo :: g -> VertexID g -> [EdgeID g]
  vertexFrom :: g -> EdgeID g -> VertexID g
  vertexTo :: g -> EdgeID g -> VertexID g
  edgeWeight :: g -> EdgeID g -> EdgeWeight g
  allVertices :: g -> [VertexID g]

data PebblingGraph
  = PebblingGraph
  { pebblingSize :: Int,
    pebblingInputSize :: Int,
    pebblingOutputSize :: Int,
    pebblingDependencies :: IntMap Int,
    pebblingComputation :: [Gate],
    pebblingInputIdxs :: IntMap Int,
    pebblingOutputIdxs :: IntMap Int
  }
  deriving (Eq, Show)

instance WeightedGraph PebblingGraph where
  type VertexID PebblingGraph = Int
  type EdgeID PebblingGraph = (Int, Int)
  type EdgeWeight PebblingGraph = Int

  edgesFrom g v =
    [ (v, v `xor` bit i)
      | i <- [0 .. pebblingSize g - 1],
        v .&. (pebblingDependencies g ! i) == (pebblingDependencies g ! i)
    ]
  edgesTo g v =
    [ (v `xor` bit i, v)
      | i <- [0 .. pebblingSize g - 1],
        v .&. (pebblingDependencies g ! i) == (pebblingDependencies g ! i)
    ]

  vertexFrom _ (a, _) = a

  vertexTo _ (_, b) = b

  edgeWeight _ (a, b) = max (popCount a) (popCount b)

  allVertices g = [0 .. bit (pebblingSize g) - 1]

pebblingFromGates :: [Gate] -> [Int] -> PebblingGraph
pebblingFromGates gates outs =
  PebblingGraph
    { pebblingSize = length gates,
      pebblingInputSize = IntMap.size inputIdxs,
      pebblingOutputSize = IntMap.size outputIdxs,
      pebblingDependencies = deps,
      pebblingComputation = gates,
      pebblingInputIdxs = inputIdxs,
      pebblingOutputIdxs = outputIdxs
    }
  where
    inputIdxs = IntMap.fromList (mapMaybe inputGateIdxs gates)
    inputGateIdxs (Input tgt) = Just (tgt, gateIdx ! tgt)
    inputGateIdxs _ = Nothing

    outputIdxs = IntMap.fromList [(tgt, gateIdx ! tgt) | tgt <- outs]

    deps =
      assert
        ( all (all (`IntMap.member` gateIdx) . depIDs) gates
            && length gates == IntMap.size gateIdx
        )
        $ IntMap.fromList
          [ ( gateIdx ! targetID g,
              foldl' (flip (xor . bit . (gateIdx !))) 0 (depIDs g)
            )
            | g <- gates
          ]

    gateIdx = IntMap.fromList (zip [targetID g | g <- gates] [0 ..])

    depIDs (Input _) = []
    depIDs (CNot x y _) = [x, y]
    depIDs (Tof x y _) = [x, y]

    targetID (Input tgt) = tgt
    targetID (CNot _ _ tgt) = tgt
    targetID (Tof _ _ tgt) = tgt

main :: IO ()
main = do
  let g = pebblingFromGates [Input 10, Input 11, Tof 10 11 12, CNot 10 12 13] [13]
  print "Hello"
  print g
  for_
    (allVertices g)
    ( \v -> do
        putStrLn $ "Vertex " ++ show v
        for_
          (edgesFrom g v)
          ( \e -> do
              putStrLn $
                "  Edge "
                  ++ show (vertexFrom g e)
                  ++ " -> "
                  ++ show (vertexTo g e)
                  ++ ": "
                  ++ show (edgeWeight g e)
          )
    )
