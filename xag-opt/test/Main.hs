{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Data.IntSet qualified as IntSet
import System.IO
import Test.QuickCheck qualified as QC
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

-- Xag.Const {nodeId = 0, value = False},
-- Xag.Const {nodeId = 1, value = True},
-- Xag.And {nodeId = 258, xIn = 2, yIn = 130},
-- Xag.Xor {nodeId = 259, xIn = 2, yIn = 130},

reducible1 :: [Xag.Node]
reducible1 =
  [ Xag.Xor 3 0 2,
    Xag.Not 4 0,
    Xag.And 5 4 1,
    Xag.Not 6 3,
    Xag.Not 7 5,
    Xag.And 8 6 7,
    Xag.Xor 9 5 2,
    Xag.And 10 8 9
  ]

main :: IO ()
main = do
  putStrLn $ "cover: " ++ show (Xag.cover (IntSet.singleton 10) reducible1)
  putStrLn $ "free: " ++ show (Xag.freeVariables reducible1)
  putStrLn $ "reduce: " ++ show (Xag.canReduce 8 reducible1)

  QC.quickCheck (prop_valid . Xag.xagNodes)
  QC.quickCheck (prop_freeVarsNotInOutputs . Xag.xagNodes)
  QC.quickCheck (prop_coverIsComplete . Xag.xagNodes)
  QC.quickCheck (prop_coverIsMinimal . Xag.xagNodes)
  -- quickCheck prop_normalizePreservesFreeVariables
  verify "adder" Xag.Benchmarks.adder
  verify "bar" Xag.Benchmarks.bar
  verify "div" Xag.Benchmarks.div
  -- hyp is just... really really big. It's probably a great stress test but I don't want to constantly run it
  -- verify "hyp" Xag.Benchmarks.hyp
  verify "log2" Xag.Benchmarks.log2
  verify "max" Xag.Benchmarks.max
  verify "multiplier" Xag.Benchmarks.multiplier
  verify "sin" Xag.Benchmarks.sin
  verify "sqrt" Xag.Benchmarks.sqrt
  verify "square" Xag.Benchmarks.square
  where
    verify name benchReader = do
      bench <- benchReader
      let g@(Xag.Graph {Xag.xagNodes = nodes}) = Xag.Benchmarks.xag bench
      putStrLn $ "Validating " ++ name ++ ": " ++ show (length nodes) ++ " nodes"
      hFlush stdout
      mapM_ (uncurry $ uncurry $ validate g) (zip (Xag.Benchmarks.testVectors bench) [0 ..])
      hFlush stdout

    validate :: Xag.Graph -> [Bool] -> [Bool] -> Int -> IO ()
    validate g inVec outVec idx =
      case Xag.eval g inVec of
        Nothing -> putStrLn ("  Invalid test or XAG, #" ++ show idx)
        Just result -> if result == outVec then return () else putStrLn ("  FAIL! #" ++ show idx)
