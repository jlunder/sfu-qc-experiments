{-# LANGUAGE ImportQualifiedPost #-}

module Xag.Optimize (canReduce) where

-- import Data.Map qualified as Map

import Data.IntSet qualified as IntSet
import Data.Maybe (isNothing)
import Debug.Trace (trace)
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

    coverClause = makeTseytin coverNodes

    -- The cover of the dominators implicitly includes the node
    coverNodes = Xag.cover (IntSet.fromList $ map fst dominators) allNodes

    dominators = Xag.dominatingAnds reduceId allNodes

makeTseytin :: [Xag.Node] -> Formula Int
makeTseytin allNodes = All (map toClause allNodes)
  where
    toClause (Xag.Const nId True) = Var nId :<->: Yes
    toClause (Xag.Const nId False) = Var nId :<->: No
    toClause (Xag.Not nId xId) = Var nId :<->: Not (Var xId)
    toClause (Xag.Xor nId xId yId) = Var nId :<->: (Var xId :++: Var yId)
    toClause (Xag.And nId xId yId) = Var nId :<->: (Var xId :&&: Var yId)
