{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Exception
import Control.Monad (foldM, when)
import Data.IntSet qualified as IntSet
import System.IO
import Test.QuickCheck qualified as QC
import XAG.Benchmarks (BenchmarkInput (..))
import XAG.Benchmarks qualified
import XAG.Graph qualified as XAG
import XAG.MinMultSat qualified as XAG
import XAG.SDCODC qualified as XAG
import Data.Maybe (fromJust)

-- import XAG.Simplify qualified as XAG

-- import XAG.Optimize

{-
  xag2 <- generate (resize 0 (arbitrary :: Gen XAG))
  print xag2
  xag3 <- generate (resize 1 (arbitrary :: Gen XAG))
  print xag3
  xag4 <- generate (resize 2 (arbitrary :: Gen XAG))
  print xag4
  xag5 <- generate (resize 3 (arbitrary :: Gen XAG))
  print xag5
  xag6 <- generate (resize 4 (arbitrary :: Gen XAG))
  print xag6
  xag7 <- generate (resize 5 (arbitrary :: Gen XAG))
  print xag7
-}

prop_valid :: [XAG.Node] -> Bool
prop_valid = XAG.validNodes

prop_freeVarsNotInOutputs :: [XAG.Node] -> Bool
prop_freeVarsNotInOutputs g =
  IntSet.intersection (XAG.freeVariables g) (XAG.outputs g) == IntSet.empty

prop_coverIsComplete :: [XAG.Node] -> QC.Property
prop_coverIsComplete nodes = QC.forAll gen prop
  where
    gen = QC.oneof (map (return . XAG.nodeID) nodes)
    prop someId =
      let cov = XAG.cover (IntSet.fromList [someId]) nodes
       in IntSet.intersection (XAG.freeVariables cov) (XAG.outputs nodes) == IntSet.empty

prop_coverIsMinimal :: [XAG.Node] -> QC.Property
prop_coverIsMinimal [] = QC.property True
prop_coverIsMinimal nodes = QC.forAll gen prop
  where
    gen = QC.oneof (map (return . XAG.nodeID) nodes)
    prop someId =
      let cov = XAG.cover (IntSet.fromList [someId]) nodes
       in IntSet.difference
            (XAG.outputs cov)
            -- This fold finds all the refs in the (XAG.cover) [XAG.Node]
            (foldr (IntSet.union . XAG.nodeRefs) IntSet.empty cov)
            -- The only output left after accounting for internal refs should
            --  be the one that initiated the XAG.cover
            == IntSet.fromList [someId]

-- prop_normalizePreservesFreeVariables :: XAG.Graph -> Bool
-- prop_normalizePreservesFreeVariables g = XAG.freeVariables g == XAG.freeVariables (normalize g)

doQuickCheckTests :: IO ()
doQuickCheckTests = do
  putStrLn "doQuickCheckTests"
  hFlush stdout

  QC.quickCheck (prop_valid . XAG.nodes)
  QC.quickCheck (prop_freeVarsNotInOutputs . XAG.nodes)
  QC.quickCheck (prop_coverIsComplete . XAG.nodes)
  QC.quickCheck (prop_coverIsMinimal . XAG.nodes)
  -- QC.quickCheck prop_normalizePreservesFreeVariables
  return ()

doVerifyTests :: IO ()
doVerifyTests = do
  putStrLn "doVerifyTests"
  hFlush stdout

  readAndVerify "adder" XAG.Benchmarks.adder
  readAndVerify "bar" XAG.Benchmarks.bar
  readAndVerify "div" XAG.Benchmarks.div
  -- hyp is just... really really big. It's probably a great stress test but I don't want to constantly run it
  -- readAndVerify "hyp" XAG.Benchmarks.hyp
  readAndVerify "log2" XAG.Benchmarks.log2
  readAndVerify "max" XAG.Benchmarks.max
  readAndVerify "multiplier" XAG.Benchmarks.multiplier
  readAndVerify "sin" XAG.Benchmarks.sin
  readAndVerify "sqrt" XAG.Benchmarks.sqrt
  readAndVerify "square" XAG.Benchmarks.square
  return ()
  where
    readAndVerify name benchReader = do
      bench <- benchReader
      verify name bench

verify :: String -> XAG.Benchmarks.BenchmarkInput -> IO ()
verify name bench = do
  let g@(XAG.Graph {XAG.nodes = nodes}) = xag bench
  putStrLn $
    "Validating "
      ++ name
      ++ ": "
      ++ show (length nodes)
      ++ " nodes, "
      ++ show (length (XAG.findAndIDs nodes))
      ++ " And"
  hFlush stdout
  mapM_ (uncurry $ uncurry $ validate g) (zip (testVectors bench) [0 ..])
  hFlush stdout

validate :: XAG.Graph -> [Bool] -> [Bool] -> Int -> IO ()
validate g inVec outVec idx =
  case XAG.eval g inVec of
    Nothing -> putStrLn ("  Invalid test or XAG, #" ++ show idx)
    Just result -> if result == outVec then return () else putStrLn ("  FAIL! #" ++ show idx)

-- XAG.Const {nodeID = 0, value = False},
-- XAG.Const {nodeID = 1, value = True},
-- XAG.And {nodeID = 258, xIn = 2, yIn = 130},
-- XAG.Xor {nodeID = 259, xIn = 2, yIn = 130},

doSimpleReduction :: IO ()
doSimpleReduction = do
  putStrLn "doSimpleReduction"
  hFlush stdout

  putStrLn $ "cover: " ++ show (XAG.cover (IntSet.singleton 10) (XAG.nodes reducible1))
  putStrLn $ "free: " ++ show (XAG.freeVariables (XAG.nodes reducible1))
  putStrLn $ "reduce: " ++ show (XAG.canReduce 8 (XAG.nodes reducible1))
  let reductions = XAG.findReducible reducible1
  putStrLn $ "findReducible: " ++ show reductions
  hFlush stdout
  where
    -- let simpleTvs = [[a, b, c] | a <- [False, True], b <- [False, True], c <- [False, True]]
    -- let reducible1Outs = XAG.outputIDs reducible1
    -- print reducible1
    -- mapM_
    --   ( \tv -> do
    --       putStrLn $
    --         "eval reducible1 "
    --           ++ show tv
    --           ++ " = "
    --           ++ show (zip reducible1Outs $ fromMaybe [] $ XAG.eval reducible1 tv)
    --       return ()
    --   )
    --   simpleTvs
    -- reduced <- foldl (flip reduceAndToNotXor) (Just reducible1) [10]
    -- let reducedOuts = XAG.outputIDs reduced
    -- print reduced
    -- mapM_
    --   ( \tv -> do
    --       putStrLn $
    --         "eval reduced "
    --           ++ show tv
    --           ++ " = "
    --           ++ show (zip reducedOuts $ fromMaybe [] $ XAG.eval reduced tv)
    --       return ()
    --   )
    --   simpleTvs

    -- print $ reduceAndToNotXor 10 reducible1
    -- print $ reduceAndToNotXor 8 reducible1
    -- hFlush stdout

    reducible1 =
      XAG.Graph
        [ XAG.Xor 3 0 2,
          XAG.Not 4 0,
          XAG.And 5 4 1,
          XAG.Not 6 3,
          XAG.Not 7 5,
          XAG.And 8 6 7,
          XAG.Xor 9 5 2,
          XAG.And 10 8 9
        ]
        [0, 1, 2]
        [10]

data ReductionFail
  = ReductionFail
  deriving (Show)

instance Exception ReductionFail

doBenchReduction :: String -> IO XAG.Benchmarks.BenchmarkInput -> IO ()
doBenchReduction name benchReader = do
  putStrLn $ "doBenchReduction " ++ name
  hFlush stdout

  bench <- benchReader
  let original = XAG.Benchmarks.xag bench

  verify name bench

  putStrLn "finding reducible"
  hFlush stdout
  let reducibleIds = XAG.findReducible original
  putStrLn $ "findReducible " ++ name ++ ": " ++ show reducibleIds

  putStrLn "performing reduction"
  hFlush stdout
  reduced <-
    foldM
      (\g rId -> return (XAG.reduceAndToNotXor rId g))
      original
      (reverse reducibleIds)

  verify
    ("reduced " ++ name)
    (XAG.Benchmarks.BenchmarkInput reduced (XAG.Benchmarks.testVectors bench))
  hFlush stdout

doSatSynthesis :: IO ()
doSatSynthesis = do
  putStrLn "doSatSynthesis"
  hFlush stdout

  putStrLn "synthesizing a + b, b + c"
  hFlush stdout
  let ab_bc_tt =
        [ ([False, False, False], [False, False]),
          ([False, False, True], [False, True]),
          ([False, True, False], [True, True]),
          ([False, True, True], [True, False]),
          ([True, False, False], [True, False]),
          ([True, False, True], [True, True]),
          ([True, True, False], [False, True]),
          ([True, True, True], [False, False])
        ]
  let g1 = XAG.synthesizeFromTruthTable 3 2 ab_bc_tt
  putStrLn $ "synthesizeFromTruthTable ab_bc_tt: " ++ show g1
  mapM_ putStrLn ["  " ++ show inputs ++ ": " ++ show (XAG.eval (fromJust g1) (inputs)) ++ ", expect " ++ show  outputs | (inputs, outputs) <- ab_bc_tt]
  hFlush stdout

  let ab_maj_tt =
        [ ([False, False, False], [False, False]),
          ([False, False, True], [False, False]),
          ([False, True, False], [True, False]),
          ([False, True, True], [True, True]),
          ([True, False, False], [True, False]),
          ([True, False, True], [True, True]),
          ([True, True, False], [False, True]),
          ([True, True, True], [False, True])
        ]
  let g2 = XAG.synthesizeFromTruthTable 3 2 ab_maj_tt
  putStrLn $ "synthesizeFromTruthTable ab_maj_tt: " ++ show g2
  mapM_ putStrLn ["  " ++ show inputs ++ ": " ++ show (XAG.eval (fromJust g2) (inputs)) ++ ", expect " ++ show  outputs | (inputs, outputs) <- ab_maj_tt]
  hFlush stdout

main :: IO ()
main = do
  when False doQuickCheckTests
  when False doVerifyTests
  when False doSimpleReduction

  when False $ doBenchReduction "adder" XAG.Benchmarks.adder
  when False $ doBenchReduction "bar" XAG.Benchmarks.bar
  when False $ doBenchReduction "div" XAG.Benchmarks.div
  -- hyp is just... really really big. It's probably a great stress test but I don't want to constantly run it
  when False $ doBenchReduction "hyp" XAG.Benchmarks.hyp
  when False $ doBenchReduction "log2" XAG.Benchmarks.log2
  when False $ doBenchReduction "max" XAG.Benchmarks.max
  when False $ doBenchReduction "multiplier" XAG.Benchmarks.multiplier
  when False $ doBenchReduction "sin" XAG.Benchmarks.sin
  when False $ doBenchReduction "sqrt" XAG.Benchmarks.sqrt
  when False $ doBenchReduction "square" XAG.Benchmarks.square

  when True doSatSynthesis
