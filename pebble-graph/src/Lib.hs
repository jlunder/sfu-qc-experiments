module Lib
    ( someFunc
    ) where

import Data.Kind (Type)

class WeightedGraph g where
  type VertexID g :: Type
  type EdgeID g :: Type
  type EdgeWeight g :: Type
  edgesFrom :: g -> VertexID g -> [EdgeID g]
  edgesTo :: g -> VertexID g -> [EdgeID g]
  hasEdge :: g -> VertexID g -> VertexID g -> Bool
  findEdge :: g -> VertexID g -> VertexID g -> Maybe (EdgeID g)
  findEdge' :: g -> VertexID g -> VertexID g -> EdgeID g
  vertexFrom :: g -> EdgeID g -> VertexID g
  vertexTo :: g -> EdgeID g -> VertexID g
  edgeWeight :: g -> EdgeID g -> EdgeWeight g
  allVertices :: g -> [VertexID g]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
