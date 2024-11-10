{-# LANGUAGE ImportQualifiedPost #-}

module Xag.Optimize
  ( canReduce,
    findAndIds,
    findReducible,
    findReducibleNodes,
    reduceAndToNotXor,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sort)
import Data.Maybe (isNothing, mapMaybe)
-- import Debug.Trace (trace)
import SAT.MiniSat
import Xag.Graph qualified as Xag

canReduce :: Int -> [Xag.Node] -> Bool
canReduce reduceId allNodes =
  case findInputs allNodes of
    Nothing -> False
    Just (xId, yId) -> isNothing (solve clause)
      where
        clause = coverClause :&&: sdcClause :&&: odcClause
        -- SDC is UNSAT if we can't even produce the input in the first place
        sdcClause = (Var xId :<->: No) :&&: (Var yId :<->: No)
        -- ODC is UNSAT if the input can't possibly affect the dominators
        -- (this is a little tricky: the input of the And which we _didn't_
        -- reach has to be true, otherwise the output will be false no matter
        -- what input we're driving it with)
        -- If the dominators are empty, Some will be inherently false making
        -- the whole formula UNSAT, so in that case we need to produce Yes
        -- which will cause the formulat to fall back on just the SDC
        odcClause =
          if null dominators
            then Yes
            else Some (map (\(_, (_, nId)) -> Var nId) dominators)
  where
    findInputs :: [Xag.Node] -> Maybe (Int, Int)
    findInputs [] = Nothing
    findInputs (Xag.And nId xId yId : nodes)
      | nId == reduceId = Just (xId, yId)
      | nId > reduceId = Nothing
      | otherwise = findInputs nodes
    findInputs (node : nodes)
      | Xag.nodeId node >= reduceId = Nothing
      | otherwise = findInputs nodes

    coverClause = makeTseytin optCoverNodes

    optCoverNodes = optimizeCover 1000 coverNodes
    criticalIds = downstreamOf (IntSet.singleton reduceId) coverNodes
    -- cover of dominatorSet should implicitly include reduceId
    coverNodes = Xag.cover dominatorIds allNodes
    dominatorIds = IntSet.fromList $ map fst dominators
    dominators = Xag.dominatingAnds reduceId allNodes

    downstreamOf :: IntSet.IntSet -> [Xag.Node] -> IntSet.IntSet
    downstreamOf searchSet [] = searchSet
    downstreamOf searchSet (node : nodes)
      | IntSet.member (Xag.nodeId node) searchSet =
          downstreamOf (insertDeps intSetF node searchSet) nodes
      | otherwise = downstreamOf searchSet nodes

    optimizeCover :: Int -> [Xag.Node] -> [Xag.Node]
    optimizeCover maxCount optNodes
      | length optNodes <= maxCount = optNodes
      | IntSet.size criticalIds >= maxCount =
          filter (\n -> IntSet.member (Xag.nodeId n) criticalIds) optNodes
      -- do a second cover here just in case we orphaned some nodes along the way
      | otherwise = Xag.cover criticalIds mostImportantNodes
      where
        mostImportantNodes = filter (\n -> IntSet.member (Xag.nodeId n) toKeepIds) optNodes
        toKeepIds = IntSet.union criticalIds (IntSet.fromList (take maxCount (map snd byImportance)))
        byImportance = sort (mapMaybe lookupImportance optNodes)
          where
            lookupImportance node =
              let nId = Xag.nodeId node
               in fmap (\x -> (x, nId)) (IntMap.lookup nId importance)
        importance :: IntMap.IntMap Double
        importance =
          distImportance
            -- Intialize the importance list with the roots at very high priority
            (IntMap.fromList (map (\nId -> (nId, 10.0)) (IntSet.toList criticalIds)))
            optNodes

        distImportance :: IntMap.IntMap Double -> [Xag.Node] -> IntMap.IntMap Double
        distImportance impSoFar [] = impSoFar
        distImportance impSoFar (node : revNodes)
          | IntMap.member (Xag.nodeId node) impSoFar =
              let f = distF (impSoFar IntMap.! Xag.nodeId node)
               in distImportance (insertDeps f node impSoFar) revNodes
          | otherwise = distImportance impSoFar revNodes
          where
            distF :: Double -> Int -> Int -> IntMap.IntMap Double -> IntMap.IntMap Double
            distF impAmt xId (-1) theMap = IntMap.insertWith (+) xId impAmt theMap
            distF impAmt xId yId theMap =
              let mapWithX = IntMap.insertWith (+) xId impAmt theMap
               in IntMap.insertWith (+) yId impAmt mapWithX

    intSetF :: Int -> Int -> IntSet.IntSet -> IntSet.IntSet
    intSetF xId (-1) theSet = IntSet.insert xId theSet
    intSetF xId yId theSet = IntSet.insert yId (IntSet.insert xId theSet)

    insertDeps :: (Int -> Int -> a -> a) -> Xag.Node -> a -> a
    insertDeps _ (Xag.Const _ _) idSet = idSet
    insertDeps insertF (Xag.Not _ xId) idSet = insertF xId (-1) idSet
    insertDeps insertF (Xag.Xor _ xId yId) idSet = insertF xId yId idSet
    insertDeps insertF (Xag.And _ xId yId) idSet = insertF xId yId idSet

makeTseytin :: [Xag.Node] -> Formula Int
makeTseytin allNodes = All (map toClause allNodes)
  where
    toClause (Xag.Const nId True) = Var nId :<->: Yes
    toClause (Xag.Const nId False) = Var nId :<->: No
    toClause (Xag.Not nId xId) = Var nId :<->: Not (Var xId)
    toClause (Xag.Xor nId xId yId) = Var nId :<->: (Var xId :++: Var yId)
    toClause (Xag.And nId xId yId) = Var nId :<->: (Var xId :&&: Var yId)

findAndIds :: [Xag.Node] -> [Int]
findAndIds [] = []
findAndIds ((Xag.And {Xag.nodeId = nId}) : found) = nId : findAndIds found
findAndIds (_ : found) = findAndIds found

findReducibleNodes :: [Xag.Node] -> [Int]
findReducibleNodes allNodes =
  allReducibleOf (findAndIds allNodes)
  where
    allReducibleOf [] = []
    allReducibleOf (tryId : idsRemaining) =
      if canReduce tryId allNodes
        then tryId : allReducibleOf idsRemaining
        else allReducibleOf idsRemaining

renumberNodes :: Int -> Int -> Int -> [Xag.Node] -> [Xag.Node]
renumberNodes atId shiftByEq shiftByGt = renumberNodesAux
  where
    renumberNodesAux [] = []
    renumberNodesAux (node : nodes) = renumberOne node : renumberNodesAux nodes

    renumberOne (Xag.Const nId value) = Xag.Const (mapId nId) value
    renumberOne (Xag.Not nId xId) = Xag.Not (mapId nId) (mapId xId)
    renumberOne (Xag.Xor nId xId yId) = Xag.Xor (mapId nId) (mapId xId) (mapId yId)
    renumberOne (Xag.And nId xId yId) = Xag.And (mapId nId) (mapId xId) (mapId yId)

    mapId = renumberId atId shiftByEq shiftByGt

renumberIds :: Int -> Int -> Int -> [Int] -> [Int]
renumberIds atId shiftByEq shiftByGt = map (renumberId atId shiftByEq shiftByGt)

renumberId :: Int -> Int -> Int -> Int -> Int
renumberId atId shiftByEq shiftByGt nId
  | nId < atId = nId
  | nId == atId = nId + shiftByEq
  | otherwise = nId + shiftByGt

splitNodes :: Int -> [Xag.Node] -> ([Xag.Node], [Xag.Node])
splitNodes atId allNodes = (take atIndex allNodes, rightNodes)
  where
    (atIndex, rightNodes) = findSplit 0 allNodes
    findSplit index [] = (index, [])
    findSplit index (node : nodes)
      | Xag.nodeId node < atId = findSplit (index + 1) nodes
      | otherwise = (index, node : nodes)

-- spliceNodes :: Int -> Int -> [Xag.Node] -> Int -> Int -> [Xag.Node] -> [Xag.Node]
-- spliceNodes fromId toId insNodes shiftByEq shiftByGt allNodes =
--   leftNodes ++ insNodes ++ renumberNodes toId shiftByEq shiftByGt rightNodes
--   where
--     (_, rightNodes) = splitNodes toId notLeftNodes
--     (leftNodes, notLeftNodes) = splitNodes fromId allNodes

findReducible :: Xag.Graph -> [Int]
findReducible (Xag.Graph allNodes _ _) = findReducibleNodes allNodes

reduceAndToNotXor :: Int -> Xag.Graph -> Maybe Xag.Graph
reduceAndToNotXor andId (Xag.Graph allNodes inOrd outOrd) =
  case splitNodes andId allNodes of
    (leftNodes, Xag.And _ xId yId : rightNodes) -> Just (updateGraph leftNodes xId yId rightNodes)
    (_, _) -> Nothing
  where
    updateGraph leftNodes xId yId rightNodes =
      Xag.Graph updatedNodes inOrd (renumberIds andId 1 1 outOrd)
      where
        updatedNodes =
          leftNodes
            ++ [Xag.Xor andId xId yId, Xag.Not (andId + 1) andId]
            ++ renumberNodes andId 1 1 rightNodes
