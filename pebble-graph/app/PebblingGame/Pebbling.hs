{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PebblingGame.Pebbling
  ( ComputationGraph (..),
    StateGraph (..),
    PrettyComputationGraph (..),
    PrettyStateGraph (..),
  )
where

import Data.Kind (Type)
import Data.Set (Set)

class (Eq (Result d), Ord (Result d)) => ComputationGraph d where
  data Computation d :: Type
  data Result d :: Type
  inputs :: d -> [Result d]
  intermediates :: d -> [Result d]
  outputs :: d -> [Result d]
  requirements :: d -> Computation d -> Set (Result d)
  results :: d -> Computation d -> Set (Result d)
  satisfying :: d -> Result d -> Set (Computation d)
  satisfiedBy :: d -> Set (Result d) -> Set (Computation d)

class (Eq (Vertex g), Ord (Vertex g)) => StateGraph g where
  data Vertex g :: Type
  data Edge g :: Type
  edgesFrom :: g -> Vertex g -> [Edge g]
  edgesTo :: g -> Vertex g -> [Edge g]
  vertexFrom :: g -> Edge g -> Vertex g
  vertexTo :: g -> Edge g -> Vertex g
  allVertexes :: g -> [Vertex g]

class PrettyComputationGraph d where
  prettyComputation :: d -> Computation d -> String
  prettyResult :: d -> Result d -> String
  prettyComputationGraph :: d -> [String]

class PrettyStateGraph g where
  prettyVertex :: g -> Vertex g -> String
  prettyEdge :: g -> Edge g -> String
  prettyStateGraph :: g -> [String]
