module Main (main) where

import Data.Bits
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort)
import Debug.Trace (trace)
import qualified Test.QuickCheck as QC
import qualified Xag.Benchmarks
import qualified Xag.Graph as Xag

-- import Xag.Optimize

{-
  xag2 <- generate (resize 0 (arbitrary :: Gen Xag))
  print xag2
  xag3 <- generate (resize 1 (arbitrary :: Gen Xag))
  print xag3
  xag4 <- generate (resize 2 (arbitrary :: Gen Xag))
  print xag4
  xag5 <- generate (resize 3 (arbitrary :: Gen Xag))
  print xag5
  xag6 <- generate (resize 4 (arbitrary :: Gen Xag))
  print xag6
  xag7 <- generate (resize 5 (arbitrary :: Gen Xag))
  print xag7
-}

prop_valid :: Xag.Graph -> Bool
prop_valid = Xag.valid

prop_freeVarsNotInOutputs :: Xag.Graph -> Bool
prop_freeVarsNotInOutputs g =
  IntSet.intersection (Xag.freeVariables g) (Xag.outputs g) == IntSet.empty

prop_coverIsComplete :: Xag.Graph -> QC.Property
prop_coverIsComplete g = QC.forAll gen prop
  where
    Xag.Graph nodes = g
    gen = QC.oneof (map (return . Xag.nodeId) nodes)
    prop someId =
      let cov = Xag.cover (IntSet.fromList [someId]) g
       in IntSet.intersection (Xag.freeVariables cov) (Xag.outputs g) == IntSet.empty

prop_coverIsMinimal :: Xag.Graph -> QC.Property
prop_coverIsMinimal (Xag.Graph []) = QC.property True
prop_coverIsMinimal g@(Xag.Graph nodes) = QC.forAll gen prop
  where
    gen = QC.oneof (map (return . Xag.nodeId) nodes)
    prop someId =
      let cov@(Xag.Graph covNodes) = Xag.cover (IntSet.fromList [someId]) g
       in IntSet.difference
            (Xag.outputs cov)
            -- This fold finds all the refs in the (Xag.cover) Xag.Graph
            (foldr (IntSet.union . Xag.nodeRefs) IntSet.empty covNodes)
            -- The only output left after accounting for internal refs should
            --  be the one that initiated the Xag.cover
            == IntSet.fromList [someId]

eval :: Xag.Graph -> [Int] -> [Int] -> [Bool] -> Maybe [Bool]
eval (Xag.Graph nodes) inOrd outOrd inVec
  | not (Xag.valid $ Xag.Graph simNodes) = Nothing
  | not (IntSet.null (Xag.freeVariables $ Xag.Graph simNodes)) = Nothing
  | otherwise =
      let resMap = foldl doEval IntMap.empty simNodes
       in Just $ map (resMap IntMap.!) outOrd
  where
    doEval :: IntMap.IntMap Bool -> Xag.Node -> IntMap.IntMap Bool
    doEval res (Xag.Const nid val) = IntMap.insert nid val res
    doEval res (Xag.Not nid xId) = IntMap.insert nid (not $ res IntMap.! xId) res
    doEval res (Xag.Xor nid xId yId) = IntMap.insert nid ((res IntMap.! xId) `xor` (res IntMap.! yId)) res
    doEval res (Xag.And nid xId yId) = IntMap.insert nid ((res IntMap.! xId) .&. (res IntMap.! yId)) res

    simNodes = sort (fixVars ++ nodes)
    fixVars = zipWith Xag.Const inOrd inVec

-- prop_normalizePreservesFreeVariables :: Xag.Graph -> Bool
-- prop_normalizePreservesFreeVariables g = Xag.freeVariables g == Xag.freeVariables (normalize g)

main :: IO ()
main = do
  QC.quickCheck prop_valid
  QC.quickCheck prop_freeVarsNotInOutputs
  QC.quickCheck prop_coverIsComplete
  QC.quickCheck prop_coverIsMinimal
  -- quickCheck prop_normalizePreservesFreeVariables
  adder <- Xag.Benchmarks.adder
  let Xag.Graph nodes = Xag.Benchmarks.xag adder
  let (inVec, outVec) = head (Xag.Benchmarks.testVectors adder)
  print (length nodes)
  let result =
        eval
          (Xag.Benchmarks.xag adder)
          (Xag.Benchmarks.inputOrder adder)
          (Xag.Benchmarks.outputOrder adder)
          inVec
  print $ Just outVec
  print $ Just outVec == result
  print result

  xag2 <- QC.generate (QC.resize 0 (QC.arbitrary :: QC.Gen Xag.Graph))
  print xag2
  xag3 <- QC.generate (QC.resize 1 (QC.arbitrary :: QC.Gen Xag.Graph))
  print xag3
  xag4 <- QC.generate (QC.resize 2 (QC.arbitrary :: QC.Gen Xag.Graph))
  print xag4
  xag5 <- QC.generate (QC.resize 3 (QC.arbitrary :: QC.Gen Xag.Graph))
  print xag5
  xag6 <- QC.generate (QC.resize 4 (QC.arbitrary :: QC.Gen Xag.Graph))
  print xag6
  xag7 <- QC.generate (QC.resize 5 (QC.arbitrary :: QC.Gen Xag.Graph))
  print xag7
