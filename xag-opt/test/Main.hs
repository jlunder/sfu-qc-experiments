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
      ++ show (length (findAndIds nodes))
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
      if Xag.canReduce tryId allNodes
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

doSimpleReduction :: IO ()
doSimpleReduction = do
  putStrLn "doSimpleReduction"
  hFlush stdout

  putStrLn $ "cover: " ++ show (Xag.cover (IntSet.singleton 10) (Xag.xagNodes reducible1))
  putStrLn $ "free: " ++ show (Xag.freeVariables (Xag.xagNodes reducible1))
  putStrLn $ "reduce: " ++ show (Xag.canReduce 8 (Xag.xagNodes reducible1))
  let reductions = findReducible reducible1
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

doAdderReduction :: IO ()
doAdderReduction = do
  putStrLn "doAdderReduction"
  hFlush stdout

  bench <- Xag.Benchmarks.adder
  let init = Xag.Benchmarks.xag bench

  verify "adder" bench

  putStrLn "finding reducible"
  hFlush stdout
  let reducibleIds = findReducible init
  putStrLn $ "adder findReducible: " ++ show reducibleIds

  putStrLn "performing reduction"
  hFlush stdout
  reduced <-
    foldM
      (\g id -> maybe undefined return (reduceAndToNotXor id g))
      init
      (reverse reducibleIds)

  verify
    "reduced adder"
    (Xag.Benchmarks.BenchmarkInput reduced (Xag.Benchmarks.testVectors bench))
  hFlush stdout

-- putStrLn "finding reducible 2"
-- hFlush stdout
-- let reducibleIds2 = findReducible reduced

-- let reduced2 = foldl (flip reduceAndToNotXor) reduced (reverse reducibleIds2)
-- verify
--   "reduced adder 2"
--   (Xag.Benchmarks.BenchmarkInput reduced2 (Xag.Benchmarks.testVectors bench))
-- hFlush stdout

-- putStrLn "finding reducible 3"
-- hFlush stdout
-- let reducibleIds3 = findReducible reduced2

-- let reduced3 = foldl (flip reduceAndToNotXor) reduced (reverse reducibleIds3)
-- verify
--   "reduced adder 3"
--   (Xag.Benchmarks.BenchmarkInput reduced3 (Xag.Benchmarks.testVectors bench))
-- hFlush stdout

main :: IO ()
main = do
  when False doQuickCheckTests
  when False doVerifyTests
  when False doSimpleReduction
  when True doAdderReduction
