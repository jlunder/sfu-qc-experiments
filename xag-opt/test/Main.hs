{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.Bits
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sort)
import Debug.Trace (trace)
import System.IO
import Test.QuickCheck qualified as QC
import Xag.Benchmarks qualified
import Xag.Graph qualified as Xag

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

eval :: Xag.Benchmarks.BenchmarkInput -> [Bool] -> Maybe [Bool]
eval (Xag.Benchmarks.BenchmarkInput {Xag.Benchmarks.xag = Xag.Graph nodes, Xag.Benchmarks.inputOrder = inOrd, Xag.Benchmarks.outputOrder = outOrd}) inVec
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
  verify "adder" Xag.Benchmarks.adder
  verify "bar" Xag.Benchmarks.bar
  verify "div" Xag.Benchmarks.div
  verify "hyp" Xag.Benchmarks.hyp
  verify "log2" Xag.Benchmarks.log2
  verify "max" Xag.Benchmarks.max
  verify "multiplier" Xag.Benchmarks.multiplier
  verify "sin" Xag.Benchmarks.sin
  verify "sqrt" Xag.Benchmarks.sqrt
  verify "square" Xag.Benchmarks.square
  where
    verify name benchReader = do
      bench <- benchReader
      let Xag.Graph nodes = Xag.Benchmarks.xag bench
      putStrLn $ "Validating " ++ name ++ ": " ++ show (length nodes) ++ " nodes"
      hFlush stdout
      mapM_ (uncurry $ uncurry $ validate bench) (zip (Xag.Benchmarks.testVectors bench) [0 ..])
      hFlush stdout

    validate :: Xag.Benchmarks.BenchmarkInput -> [Bool] -> [Bool] -> Int -> IO ()
    validate bench inVec outVec idx =
      case eval bench inVec of
        Nothing -> putStrLn ("  Invalid test or XAG, #" ++ show idx)
        Just result -> if result == outVec then return () else putStrLn ("  FAIL! #" ++ show idx)
