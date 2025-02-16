{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use all" #-}

module PebblingGame.Graph where

import Data.Kind (Type)

class Graph g where
  type Vertex g :: Type
  type Edge g :: Type
  edgesFrom :: g -> Vertex g -> [Edge g]
  edgesTo :: g -> Vertex g -> [Edge g]
  vertexFrom :: g -> Edge g -> Vertex g
  vertexTo :: g -> Edge g -> Vertex g
  allVertexes :: g -> [Vertex g]

class (Graph g) => PrettyGraph g where
  prettyVertex :: g -> Vertex g -> String
  prettyEdge :: g -> Edge g -> String
  prettyGraph :: g -> [String]
