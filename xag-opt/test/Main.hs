{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Exception
import Control.Monad (foldM, when)
import Data.IntSet qualified as IntSet
import System.IO
import Test.QuickCheck qualified as QC
import Xag.Benchmarks (BenchmarkInput (..))
import Xag.Benchmarks qualified
import Xag.Graph qualified as Xag
import Xag.Optimize qualified as Xag

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

prop_valid :: [Xag.Node] -> Bool
prop_valid = Xag.validNodes

prop_freeVarsNotInOutputs :: [Xag.Node] -> Bool
prop_freeVarsNotInOutputs g =
  IntSet.intersection (Xag.freeVariables g) (Xag.outputs g) == IntSet.empty

prop_coverIsComplete :: [Xag.Node] -> QC.Property
prop_coverIsComplete nodes = QC.forAll gen prop
  where
    gen = QC.oneof (map (return . Xag.nodeId) nodes)
    prop someId =
      let cov = Xag.cover (IntSet.fromList [someId]) nodes
       in IntSet.intersection (Xag.freeVariables cov) (Xag.outputs nodes) == IntSet.empty

prop_coverIsMinimal :: [Xag.Node] -> QC.Property
prop_coverIsMinimal [] = QC.property True
prop_coverIsMinimal nodes = QC.forAll gen prop
  where
    gen = QC.oneof (map (return . Xag.nodeId) nodes)
    prop someId =
      let cov = Xag.cover (IntSet.fromList [someId]) nodes
       in IntSet.difference
            (Xag.outputs cov)
            -- This fold finds all the refs in the (Xag.cover) [Xag.Node]
            (foldr (IntSet.union . Xag.nodeRefs) IntSet.empty cov)
            -- The only output left after accounting for internal refs should
            --  be the one that initiated the Xag.cover
            == IntSet.fromList [someId]

-- prop_normalizePreservesFreeVariables :: Xag.Graph -> Bool
-- prop_normalizePreservesFreeVariables g = Xag.freeVariables g == Xag.freeVariables (normalize g)

doQuickCheckTests :: IO ()
doQuickCheckTests = do
  putStrLn "doQuickCheckTests"
  hFlush stdout

  QC.quickCheck (prop_valid . Xag.xagNodes)
  QC.quickCheck (prop_freeVarsNotInOutputs . Xag.xagNodes)
  QC.quickCheck (prop_coverIsComplete . Xag.xagNodes)
  QC.quickCheck (prop_coverIsMinimal . Xag.xagNodes)
  -- QC.quickCheck prop_normalizePreservesFreeVariables
  return ()

doVerifyTests :: IO ()
doVerifyTests = do
  putStrLn "doVerifyTests"
  hFlush stdout

  readAndVerify "adder" Xag.Benchmarks.adder
  readAndVerify "bar" Xag.Benchmarks.bar
  readAndVerify "div" Xag.Benchmarks.div
  -- hyp is just... really really big. It's probably a great stress test but I don't want to constantly run it
  -- readAndVerify "hyp" Xag.Benchmarks.hyp
  readAndVerify "log2" Xag.Benchmarks.log2
  readAndVerify "max" Xag.Benchmarks.max
  readAndVerify "multiplier" Xag.Benchmarks.multiplier
  readAndVerify "sin" Xag.Benchmarks.sin
  readAndVerify "sqrt" Xag.Benchmarks.sqrt
  readAndVerify "square" Xag.Benchmarks.square
  return ()
  where
    readAndVerify name benchReader = do
      bench <- benchReader
      verify name bench

verify :: String -> Xag.Benchmarks.BenchmarkInput -> IO ()
verify name bench = do
  let g@(Xag.Graph {Xag.xagNodes = nodes}) = xag bench
  putStrLn $
    "Validating "
      ++ name
      ++ ": "
      ++ show (length nodes)
      ++ " nodes, "
      ++ show (length (Xag.findAndIds nodes))
      ++ " And"
  hFlush stdout
  mapM_ (uncurry $ uncurry $ validate g) (zip (testVectors bench) [0 ..])
  hFlush stdout

validate :: Xag.Graph -> [Bool] -> [Bool] -> Int -> IO ()
validate g inVec outVec idx =
  case Xag.eval g inVec of
    Nothing -> putStrLn ("  Invalid test or XAG, #" ++ show idx)
    Just result -> if result == outVec then return () else putStrLn ("  FAIL! #" ++ show idx)

-- Xag.Const {nodeId = 0, value = False},
-- Xag.Const {nodeId = 1, value = True},
-- Xag.And {nodeId = 258, xIn = 2, yIn = 130},
-- Xag.Xor {nodeId = 259, xIn = 2, yIn = 130},

doSimpleReduction :: IO ()
doSimpleReduction = do
  putStrLn "doSimpleReduction"
  hFlush stdout

  putStrLn $ "cover: " ++ show (Xag.cover (IntSet.singleton 10) (Xag.xagNodes reducible1))
  putStrLn $ "free: " ++ show (Xag.freeVariables (Xag.xagNodes reducible1))
  putStrLn $ "reduce: " ++ show (Xag.canReduce 8 (Xag.xagNodes reducible1))
  let reductions = Xag.findReducible reducible1
  putStrLn $ "findReducible: " ++ show reductions
  hFlush stdout
  where
    -- let simpleTvs = [[a, b, c] | a <- [False, True], b <- [False, True], c <- [False, True]]
    -- let reducible1Outs = Xag.outputOrder reducible1
    -- print reducible1
    -- mapM_
    --   ( \tv -> do
    --       putStrLn $
    --         "eval reducible1 "
    --           ++ show tv
    --           ++ " = "
    --           ++ show (zip reducible1Outs $ fromMaybe [] $ Xag.eval reducible1 tv)
    --       return ()
    --   )
    --   simpleTvs
    -- reduced <- foldl (flip reduceAndToNotXor) (Just reducible1) [10]
    -- let reducedOuts = Xag.outputOrder reduced
    -- print reduced
    -- mapM_
    --   ( \tv -> do
    --       putStrLn $
    --         "eval reduced "
    --           ++ show tv
    --           ++ " = "
    --           ++ show (zip reducedOuts $ fromMaybe [] $ Xag.eval reduced tv)
    --       return ()
    --   )
    --   simpleTvs

    -- print $ reduceAndToNotXor 10 reducible1
    -- print $ reduceAndToNotXor 8 reducible1
    -- hFlush stdout

    reducible1 =
      Xag.Graph
        [ Xag.Xor 3 0 2,
          Xag.Not 4 0,
          Xag.And 5 4 1,
          Xag.Not 6 3,
          Xag.Not 7 5,
          Xag.And 8 6 7,
          Xag.Xor 9 5 2,
          Xag.And 10 8 9
        ]
        [0, 1, 2]
        [10]

data ReductionFail
  = ReductionFail
  deriving (Show)

instance Exception ReductionFail

doBenchReduction :: String -> IO Xag.Benchmarks.BenchmarkInput -> IO ()
doBenchReduction name benchReader = do
  putStrLn $ "doBenchReduction " ++ name
  hFlush stdout

  bench <- benchReader
  let original = Xag.Benchmarks.xag bench

  verify name bench

  putStrLn "finding reducible"
  hFlush stdout
  let reducibleIds = Xag.findReducible original
  putStrLn $ "findReducible " ++ name ++ ": " ++ show reducibleIds

  putStrLn "performing reduction"
  hFlush stdout
  reduced <-
    foldM
      (\g rId -> maybe undefined return (Xag.reduceAndToNotXor rId g))
      original
      (reverse reducibleIds)

  verify
    ("reduced " ++ name)
    (Xag.Benchmarks.BenchmarkInput reduced (Xag.Benchmarks.testVectors bench))
  hFlush stdout

main :: IO ()
main = do
  when False doQuickCheckTests
  when False doVerifyTests
  when False doSimpleReduction
  when True $ doBenchReduction "adder" Xag.Benchmarks.adder
  when True $ doBenchReduction "bar" Xag.Benchmarks.bar
  when True $ doBenchReduction "div" Xag.Benchmarks.div
  -- hyp is just... really really big. It's probably a great stress test but I don't want to constantly run it
  when False $ doBenchReduction "hyp" Xag.Benchmarks.hyp
  when True $ doBenchReduction "log2" Xag.Benchmarks.log2
  when True $ doBenchReduction "max" Xag.Benchmarks.max
  when True $ doBenchReduction "multiplier" Xag.Benchmarks.multiplier
  when True $ doBenchReduction "sin" Xag.Benchmarks.sin
  when True $ doBenchReduction "sqrt" Xag.Benchmarks.sqrt
  when True $ doBenchReduction "square" Xag.Benchmarks.square
