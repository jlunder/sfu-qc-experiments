{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use all" #-}

module PebblingGame.AStar (aStarShortestPath) where

import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict qualified as Map
import Data.PQueue.Min (MinQueue (..))
import Data.PQueue.Min qualified as MinQ

newtype FrontierNode c v = F (c, c, [v])

instance (Eq c, Eq v) => Eq (FrontierNode c v) where
  (==) (F (xh, xc, xv)) (F (yh, yc, yv)) = xh == yh && xc == yc && xv == yv

instance (Ord c, Ord v) => Ord (FrontierNode c v) where
  compare (F xt) (F yt) = compare xt yt
  (<) (F xt) (F yt) = xt < yt

-- v = vertex, e = edge, c = cost
aStarShortestPath :: (Ord c, Ord v) => c -> v -> (v -> Bool) -> (v -> [(e, v)]) -> (c -> v -> c) -> (c -> v -> e -> c) -> Maybe ([v], c)
aStarShortestPath initC initV goalF edgesF heuristicF costF =
  search (MinQ.singleton (F (heuristicF initC initV, initC, [initV]))) Map.empty
  where
    search Empty _ = Nothing
    search ((F (_, curCost, curPath)) :< remain) visited
      | goalF (head curPath) = Just (reverse curPath, curCost)
      | otherwise = search newFrontier newVisited
      where
        curV = head curPath
        newVisited =
          foldl'
            (\m (nextV, nextCost) -> Map.insert nextV nextCost m)
            visited
            worthwhile
        newFrontier =
          foldl'
            ( \m (nextV, nextCost) ->
                F (heuristicF nextCost nextV, nextCost, nextV : curPath) :< m
            )
            remain
            worthwhile
        worthwhile =
          filter
            ( \(nextV, nextCost) ->
                Map.notMember nextV visited || (nextCost < (visited Map.! nextV))
            )
            [(nextV, costF curCost curV nextE) | (nextE, nextV) <- edgesF curV]
