{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use all" #-}

module Main (main) where

import Control.Exception (assert)
import Data.Bits (Bits (popCount), bit, xor, (.&.), (.|.))
import Data.Foldable (foldl', for_)
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Math.NumberTheory.Logarithms (intLog2')

data Gate
  = CNot Int Int Int
  | Tof Int Int Int
  deriving (Eq, Show)

data PebblingGraph
  = PebblingGraph
  { pebblingSize :: Int,
    pebblingInputSize :: Int,
    pebblingOutputSize :: Int,
    pebblingDependencies :: IntMap Int,
    pebblingComputation :: [Gate],
    pebblingInputs :: [Int],
    pebblingOutputBits :: IntMap Int
  }
  deriving (Eq, Show)

-- instance WeightedGraph PebblingGraph where
--   type VertexID PebblingGraph = Int
--   type EdgeID PebblingGraph = (Int, Int)
--   type EdgeWeight PebblingGraph = Int

newtype PebblingVertex = V Int deriving (Eq, Ord, Show)

newtype PebblingEdge = E (Int, Int) deriving (Eq, Ord, Show)

index :: PebblingVertex -> Int
index (V idx) = idx

hasEdge :: PebblingGraph -> PebblingVertex -> PebblingVertex -> Bool
hasEdge g u v =
  (popCount flipped == 1)
    && ((index v .&. depBits) == depBits)
  where
    depBits = pebblingDependencies g ! i
    i = intLog2' flipped
    flipped = index u `xor` index v

edgesFrom :: PebblingGraph -> PebblingVertex -> [PebblingEdge]
edgesFrom g v =
  [ E (index v, index v `xor` bit i)
    | i <- [0 .. pebblingSize g - 1],
      let depBits = pebblingDependencies g ! bit i
       in (index v .&. depBits) == depBits
  ]

edgesTo :: PebblingGraph -> PebblingVertex -> [PebblingEdge]
edgesTo g v =
  [ E (index v `xor` bit i, index v)
    | i <- [0 .. pebblingSize g - 1],
      let depBits = pebblingDependencies g ! bit i
       in (index v .&. depBits) == depBits
  ]

vertexFrom :: PebblingGraph -> PebblingEdge -> PebblingVertex
vertexFrom _ (E (a, _)) = V a

vertexTo :: PebblingGraph -> PebblingEdge -> PebblingVertex
vertexTo _ (E (_, b)) = V b

edgeWeight :: PebblingGraph -> PebblingEdge -> Int
edgeWeight _ (E (_, b)) = popCount b

allVertices :: PebblingGraph -> [PebblingVertex]
allVertices g = [V i | i <- [0 .. bit (pebblingSize g) - 1]]

pebblingFromGates :: [Gate] -> [Int] -> [Int] -> PebblingGraph
pebblingFromGates gates ins outs =
  PebblingGraph
    { pebblingSize = length gates,
      pebblingInputSize = length ins,
      pebblingOutputSize = IntMap.size outputBits,
      pebblingDependencies = deps,
      pebblingComputation = gates,
      pebblingInputs = ins,
      pebblingOutputBits = outputBits
    }
  where
    outputBits = IntMap.fromList [(tgt, gateBits ! tgt) | tgt <- outs]

    deps =
      assert
        ( all (`IntMap.member` gateBits) (concatMap depIDs gates)
            && (length ins + length gates) == IntMap.size gateBits
        )
        $ IntMap.fromList
          [ ( gateBits ! targetID g,
              foldl' (flip (xor . (gateBits !))) 0 (depIDs g)
            )
            | g <- gates
          ]

    gateBits =
      IntMap.fromList
        ( map (\inID -> (inID, 0)) ins
            ++ [(targetID g, bit i) | (g, i) <- zip gates [0 ..]]
        )

    depIDs (CNot x y _) = [x, y]
    depIDs (Tof x y _) = [x, y]

    targetID (CNot _ _ tgt) = tgt
    targetID (Tof _ _ tgt) = tgt

-- aStarMinimaxShortestPath :: PebblingGraph -> PebblingVertex -> PebblingVertex -> (PebblingEdge -> Int) -> (PebblingVertex -> Int) -> Maybe [PebblingVertex]
-- aStarMinimaxShortestPath graph initV goalV cost heuristic = undefined
--   where
--     initBottleneck :: Map (Int, Int) Int
--     initBottleneck =
--       Map.fromList
--         [ ((u, v), edgeWeight graph (findEdge' graph u v))
--           | u <- allVertices graph,
--             v <- allVertices graph,
--             hasEdge graph u v
--         ]
--     initPrev :: Map (Int, Int) Int
--     initPrev =
--       Map.fromList
--         [ ((u, v), u)
--           | u <- allVertices graph,
--             v <- allVertices graph
--         ]

--     vertices = allVertices graph

--     (minimax', prev') = foldl' (\w wState->
--                               foldl' (\u uState->
--                                           foldl' (\v vState@(minimax, prev)->
--                                             if Map.member (u, v) minimax && minimax Map.! (u, v) >
--                                             ) uState vertices
--                                 ) wState vertices
--                             ) (initBottleneck, initPrev) vertices

floydWarshallMinimaxShortestPath :: PebblingGraph -> PebblingVertex -> PebblingVertex -> (PebblingEdge -> Int) -> (PebblingEdge -> Int) -> Maybe ([PebblingVertex], Int)
floydWarshallMinimaxShortestPath graph initV goalV _ weight =
  walkFinPrevToInit goalV >>= (\l -> Just (l, finBottleneck Map.! (initV, goalV)))
  where
    walkFinPrevToInit :: PebblingVertex -> Maybe [PebblingVertex]
    walkFinPrevToInit v
      | v == initV = Just [v]
      | Map.notMember (initV, v) finPrev = Nothing
      | otherwise = walkFinPrevToInit (finPrev Map.! (initV, v)) >>= (\remain -> Just (v : remain))

    initBottleneck :: Map (PebblingVertex, PebblingVertex) Int
    initBottleneck =
      Map.fromList
        [ ((vertexFrom graph e, vertexTo graph e), weight e)
          | e <- edges
        ]
    initPrev :: Map (PebblingVertex, PebblingVertex) PebblingVertex
    initPrev = Map.fromList [((vertexFrom graph e, vertexTo graph e), vertexFrom graph e) | e <- edges]

    vertices = allVertices graph
    edges = concatMap (edgesFrom graph) vertices

    (finBottleneck, finPrev) =
      foldl'
        ( \wState w ->
            foldl'
              ( \uState u ->
                  foldl'
                    ( \vState@(bottleneck, prev) v ->
                        if Map.notMember (u, w) bottleneck
                          || Map.notMember (w, v) bottleneck
                          then vState
                          else
                            let uwvBottleneck = min (bottleneck Map.! (u, w)) (bottleneck Map.! (w, v))
                             in if Map.notMember (u, v) bottleneck || (bottleneck Map.! (u, v) < uwvBottleneck)
                                  then
                                    ( Map.insert (u, v) uwvBottleneck bottleneck,
                                      Map.insert (u, v) (prev Map.! (w, v)) prev
                                    )
                                  else vState
                    )
                    uState
                    vertices
              )
              wState
              vertices
        )
        (initBottleneck, initPrev)
        vertices

main :: IO ()
main = do
  let g = pebblingFromGates [Tof 10 11 12, CNot 10 12 13] [10, 11] [13]
  print g
  for_
    (allVertices g)
    ( \v -> do
        putStrLn $ "PebblingVertex " ++ show v
        for_
          (edgesFrom g v)
          ( \e -> do
              putStrLn $
                "  PebblingEdge "
                  ++ show (vertexFrom g e)
                  ++ " -> "
                  ++ show (vertexTo g e)
                  ++ ": "
                  ++ show (edgeWeight g e)
          )
    )
  let initV = V 0
      goalV = V (foldl' (.|.) 0 (IntMap.elems (pebblingOutputBits g)))
  print $ floydWarshallMinimaxShortestPath g initV goalV (const 1) (edgeWeight g)
