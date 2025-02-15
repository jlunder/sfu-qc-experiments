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

data Gate
  = CNot Int Int Int
  | Tof Int Int Int
  deriving (Eq, Show)

data PebblingGraph
  = PebblingGraph
  { pebblingSize :: Int,
    pebblingInputSize :: Int,
    pebblingOutputSize :: Int,
    -- Dependencies are one-hot bits -> bits
    pebblingDependencies :: IntMap Int,
    -- GateIDs are one-hot bits -> gate ID
    pebblingGateIDs :: IntMap Int,
    pebblingComputation :: [Gate],
    pebblingInputs :: [Int],
    pebblingOutputBits :: IntMap Int
  }
  deriving (Eq, Show)

newtype PebblingVertex = V Int deriving (Eq, Ord, Show)

newtype PebblingEdge = E (Int, Int) deriving (Eq, Ord, Show)

index :: PebblingVertex -> Int
index (V idx) = idx

edgesFrom :: PebblingGraph -> PebblingVertex -> [PebblingEdge]
edgesFrom g v =
  [ E (index v, index v `xor` b)
    | b <- map bit [0 .. pebblingSize g - 1],
      let depBits = pebblingDependencies g ! b
       in (index v .&. depBits) == depBits
  ]

edgesTo :: PebblingGraph -> PebblingVertex -> [PebblingEdge]
edgesTo g v = [E (b, a) | E (a, b) <- edgesFrom g v]

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
      pebblingGateIDs =
        IntMap.fromList [(b, c) | (c, b) <- IntMap.toList gateBits, b /= 0],
      pebblingComputation = gates,
      pebblingInputs = ins,
      pebblingOutputBits = outputBits
    }
  where
    outputBits = IntMap.fromList [(tgt, gateBits ! tgt) | tgt <- outs]

    -- Maps a pebbling bit -> the dependency bits that need to be set for it
    -- to be pebbled or unpebbled
    deps =
      assert
        -- assert all gate IDs have bits mappings, and are all distinct
        ( all (`IntMap.member` gateBits) (concatMap depIDs gates)
            && (length ins + length gates) == IntMap.size gateBits
        )
        $ IntMap.fromList
          [ ( gateBits ! targetID g,
              foldl' (.|.) 0 (map (gateBits !) (depIDs g))
            )
            | g <- gates
          ]

    -- Maps gate ID -> pebbling bits; for inputs these are 0 because pebbles
    -- are only for computations, not inputs
    -- Specifically, each mapped bit here is either in the form (bit i) or 0,
    -- so (hammingWeight (gateBits k)) is always either 1 or 0
    gateBits =
      IntMap.fromList
        ( map (\inID -> (inID, 0)) ins
            ++ [(targetID g, bit i) | (g, i) <- zip gates [0 ..]]
        )

    depIDs (CNot x y _) = [x, y]
    depIDs (Tof x y _) = [x, y]

    targetID (CNot _ _ tgt) = tgt
    targetID (Tof _ _ tgt) = tgt

floydWarshallMinimaxShortestPath :: PebblingGraph -> PebblingVertex -> PebblingVertex -> (PebblingEdge -> Int) -> (PebblingEdge -> Int) -> Maybe ([PebblingVertex], Int)
floydWarshallMinimaxShortestPath graph initV goalV _ weight =
  walkFinPrevToInit goalV >>= (\l -> Just (reverse l, finBottleneck Map.! (initV, goalV)))
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
                            let uwvBottleneck = max (bottleneck Map.! (u, w)) (bottleneck Map.! (w, v))
                             in if Map.notMember (u, v) bottleneck || (uwvBottleneck < bottleneck Map.! (u, v))
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

prettyVertex :: PebblingGraph -> PebblingVertex -> String
prettyVertex g (V idx) =
  "(" ++ show idx ++ ") " ++ show [c | (b, c) <- gateIDs, (idx .&. b) == b]
  where
    gateIDs = IntMap.toList (pebblingGateIDs g)

prettyEdge :: PebblingGraph -> PebblingEdge -> String
prettyEdge g (E (fromIdx, toIdx)) =
  ("(" ++ show fromIdx ++ " -> " ++ show toIdx ++ ") ")
    ++ (plusMinus ++ show [c | (b, c) <- gateIDs, (chIdx .&. b) == b])
  where
    plusMinus = if fromIdx < toIdx then "+" else "-"
    chIdx = fromIdx `xor` toIdx
    gateIDs = IntMap.toList (pebblingGateIDs g)

printGraph :: PebblingGraph -> IO ()
printGraph g = do
  for_
    (allVertices g)
    ( \v -> do
        putStrLn (prettyVertex g v)
        for_
          (edgesFrom g v)
          (\e -> putStrLn ("  " ++ prettyEdge g e))
    )

testPebbling :: PebblingGraph -> IO ()
testPebbling g = do
  -- printGraph g
  putStrLn "Pebbling computation:"
  putStrLn ("  Inputs: " ++ show (pebblingInputs g))
  putStrLn ("  Outputs: " ++ show (IntMap.keys (pebblingOutputBits g)))
  for_ (pebblingComputation g) (\c -> putStrLn ("  " ++ show c))
  let initV = V 0
      goalV = V (foldl' (.|.) 0 (IntMap.elems (pebblingOutputBits g)))
      result = floydWarshallMinimaxShortestPath g initV goalV (const 1) (edgeWeight g)
  case result of
    Just (path, maxPebbles) -> do
      putStrLn ("Minimum pebbling found with " ++ show maxPebbles ++ " pebbles")
      putStrLn "Sequence:"
      for_
        path
        ( \v -> do
            putStrLn ("  " ++ prettyVertex g v)
        )
    Nothing -> putStrLn "No pebbling found"

main :: IO ()
main = do
  testPebbling
    ( pebblingFromGates
        [ Tof 10 11 12,
          CNot 10 12 13
        ]
        [10, 11]
        [13]
    )
  testPebbling
    ( pebblingFromGates
        [ Tof 2 3 11,
          Tof 11 3 12,
          Tof 3 4 13,
          Tof 13 3 14,
          Tof 12 14 21,
          Tof 1 11 22
        ]
        [1, 2, 3, 4]
        [21, 22]
    )
