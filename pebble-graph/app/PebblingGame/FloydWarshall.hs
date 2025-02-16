{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use all" #-}

module PebblingGame.FloydWarshall where

import Data.Foldable (foldl')
import Data.Map.Strict qualified as Map
import PebblingGame.Graph

floydWarshallMinimaxShortestPath :: (Graph g, Ord (Vertex g), Ord w) => g -> Vertex g -> Vertex g -> (Edge g -> w) -> Maybe ([Vertex g], w)
floydWarshallMinimaxShortestPath graph initV goalV weight =
  walkFinPrevToInit goalV
    >>= (\path -> Just (reverse path, finBottleneck Map.! (initV, goalV)))
  where
    --walkFinPrevToInit :: Vertex g -> Maybe [Vertex g]
    walkFinPrevToInit v
      | v == initV = Just [v]
      | Map.notMember (initV, v) finPrev = Nothing
      | otherwise = walkFinPrevToInit (finPrev Map.! (initV, v)) >>= (\remain -> Just (v : remain))

    --initBottleneck :: Map (Vertex g, Vertex g) Int
    initBottleneck =
      Map.fromList
        [ ((vertexFrom graph e, vertexTo graph e), weight e)
          | e <- edges
        ]
    --initPrev :: Map (Vertex g, Vertex g) (Vertex g)
    initPrev = Map.fromList [((vertexFrom graph e, vertexTo graph e), vertexFrom graph e) | e <- edges]

    vertexes = allVertexes graph
    edges = concatMap (edgesFrom graph) vertexes

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
                    vertexes
              )
              wState
              vertexes
        )
        (initBottleneck, initPrev)
        vertexes
