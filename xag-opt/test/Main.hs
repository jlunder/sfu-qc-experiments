module Main (main) where

import qualified Data.IntSet as IntSet
import Test.QuickCheck
import Xag.Graph as Xag
import Xag.Optimize

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

-- IntSet.intersection (freeVariables g) (outputs g) == IntSet.empty

-- xag <- generate (arbitrary :: Gen Graph)
-- print xag
-- testGraph :: Graph
-- testGraph =
--   Graph
--     [ Const 0 True,
--       Xor 6 [0, 0, 2, 0],
--       Xor 7 [0, 2, 6, 3],
--       Xor 8 [1, 7, 4, 6],
--       Xor 9 [0, 1],
--       And 10 [1, 7, 5, 3],
--       And 11 [2, 1, 1, 1, 1],
--       Xor 12 [1, 6],
--       And 13 [1, 6, 0],
--       And 14 [1, 1, 0],
--       Xor 15 [1, 0, 9, 7],
--       Xor 16 [7, 1, 1, 0, 0],
--       Xor 17 [7, 1],
--       Xor 18 [9, 1, 12, 0],
--       Xor 19 [16, 0, 5, 2],
--       Xor 20 [0, 18],
--       Xor 21 [15, 1, 0],
--       Xor 22 [0, 13, 1, 1],
--       And 23 [0, 18, 0, 12],
--       Xor 24 [8, 0, 1, 3],
--       Xor 25 [1, 21, 0, 0],
--       Xor 26 [21, 1, 4],
--       And 27 [0, 0],
--       And 28 [0, 19],
--       And 29 [1, 20, 23, 0],
--       And 30 [13, 29],
--       And 31 [24, 1, 0, 1],
--       And 32 [1, 4, 1, 1, 10],
--       Xor 33 [5, 0, 24, 20, 10],
--       And 34 [1, 1]
--     ]
-- print (Xag.cover (IntSet.fromList [13]) testGraph)
-- print (Xag.cover (IntSet.fromList [33]) testGraph)
-- print (Xag.cover (IntSet.fromList [34]) testGraph)
-- print $ normalize (Graph [Const 0 True, Xor 2 (IntSet.fromList [0, 1])])
-- print (Xag.cover (IntSet.fromList [0]) (Graph [Const 0 True]))

prop_valid :: Graph -> Bool
prop_valid = valid

prop_freeVarsNotInOutputs :: Graph -> Bool
prop_freeVarsNotInOutputs g =
  IntSet.intersection (freeVariables g) (outputs g) == IntSet.empty

prop_coverIsComplete :: Graph -> Property
prop_coverIsComplete g = forAll gen prop
  where
    Graph nodes = g
    gen = oneof (map (return . nodeId) nodes)
    prop someId =
      let cov = Xag.cover (IntSet.fromList [someId]) g
       in IntSet.intersection (freeVariables cov) (outputs g) == IntSet.empty

prop_coverIsMinimal :: Graph -> Property
prop_coverIsMinimal (Graph []) = property True
prop_coverIsMinimal g@(Graph nodes) = forAll gen prop
  where
    gen = oneof (map (return . nodeId) nodes)
    prop someId =
      let cov@(Graph covNodes) = Xag.cover (IntSet.fromList [someId]) g
       in IntSet.difference
            (outputs cov)
            -- This fold finds all the refs in the (Xag.cover) graph
            (foldr IntSet.union IntSet.empty (map nodeRefs covNodes))
            -- The only output left after accounting for internal refs should
            --  be the one that initiated the Xag.cover
            == IntSet.fromList [someId]

prop_normalizePreservesFreeVariables :: Graph -> Bool
prop_normalizePreservesFreeVariables g = freeVariables g == freeVariables (normalize g)

main :: IO ()
main = do
  quickCheck prop_valid
  quickCheck prop_freeVarsNotInOutputs
  quickCheck prop_coverIsComplete
  quickCheck prop_coverIsMinimal
  quickCheck prop_normalizePreservesFreeVariables
  print dowork