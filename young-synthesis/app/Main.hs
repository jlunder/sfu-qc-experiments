{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-unused-top-binds -Wno-missing-signatures -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use unwords" #-}
module Main (main) where

import Data.Array.IArray  (Array, (!))
import Data.Array.IArray  qualified as Array
import Data.Array.Unboxed (UArray)
import Data.Foldable      (Foldable (..), forM_, for_)
import Data.List          (intercalate)
import Data.Map           (Map)
import Data.Map           qualified as Map
import Data.Maybe         (listToMaybe, mapMaybe)
import Data.Set           (Set)
import Data.Set           qualified as Set

import Debug.Trace        (trace)

import Multilinear

-- import YoungSubgroupDecomp (decompose)

-- import Test.QuickCheck qualified as QC

truthTable :: [Var] -> [[(Var, Bool)]]
truthTable [] = [[]]
truthTable (v : remain) = map ((v, False) :) remainTable ++ map ((v, True) :) remainTable
  where
    remainTable = truthTable remain

assignsTable :: [Var] -> [Assigns Bool]
assignsTable = map assigns . truthTable

allTerms :: [Var] -> [Term]
allTerms = map fromVarList . allTermVars . reverse
  where
    allTermVars []       = [[]]
    allTermVars (v : vs) = (allTermVars vs) ++ map (v :) (allTermVars vs)

allFuncs :: [Term] -> [Func]
allFuncs = map fromTermList . allFuncTerms . reverse
  where
    allFuncTerms []          = [[]]
    allFuncTerms (t : terms) = (allFuncTerms terms) ++ map (t :) (allFuncTerms terms)

findFunc :: Map (Assigns Bool) Bool -> Func
findFunc evalMap = head (filter fMatches (allFuncs (allTerms evalUsedVars)))
  where
    fMatches f = all (\(a, r) -> (eval f a) == r) (Map.toList evalMap)
    evalUsedVars = Set.toList (foldl' Set.union Set.empty (map assignedTrue (Map.keys evalMap)))

-- With f(x) a reversible function from F_2^n -> F_2^n, decompose every f_i(x) into
--   x_i + g_i(g_1..i-1, x_i+1..x_n) + h_i(g_1..i-1, f_i+1..n) = f_i(x)
-- Takes: a list of n items, each: the variables to use for x_i, f_i, s_i; and the multilinear representation of f_i in terms of x_i
-- Note: the f_i and g_i variables should not appear in the definitions of f_i, and there should be exactly n free variables in the f_i's
-- Returns: if a decomposition is found, Just a list of n items, each: the variables x_i, f_i, s_i; and g_i in terms of x and s, h_i in terms of f and s
decompose :: [(Var, Var, Var, Func)] -> Maybe [(Var, Var, Var, Func, Func)]
decompose funcs = decomposeAll funcs []
  where
    decomposeAll :: [(Var, Var, Var, Func)] -> [(Var, Var, Var, Func, Func, Func)] -> Maybe [(Var, Var, Var, Func, Func)]
    decomposeAll [] soFar = Just [(xiVar, fiVar, siVar, giFunc, hiFunc) | (xiVar, fiVar, siVar, giFunc, hiFunc, _) <- soFar]
    decomposeAll ((xiVar, fiVar, siVar, fiFunc):remain) soFar =
      decomposeOne xiVar fiFunc piqiFuncs >>=
          (\(giFunc, hiFunc) -> decomposeAll remain ((xiVar, fiVar, siVar, giFunc, hiFunc, makeSiInX giFunc) : soFar))
      where
        piqiSoFar = [(sjVar, sjFunc, sjVar, sjFunc) | (_, _, sjVar, _, _, sjFunc) <- soFar]
        xfRemain = [(xjVar, monomial xjVar, fjVar, fjFunc) | (xjVar, fjVar, _, fjFunc) <- remain]
        piqiFuncs = piqiSoFar ++ xfRemain

        -- s_i = x_i + g_i
        makeSiInX :: Func -> Func
        makeSiInX giFunc = plus (monomial xiVar) (makeGiInX giFunc)
        -- Substitute all s_{j < i} into g_i, so that it's written just in x
        makeGiInX :: Func -> Func
        makeGiInX giFunc = foldl' (flip (uncurry substitute)) giFunc [(sjFunc, sjVar) | (_, _, sjVar, _, _, sjFunc) <- soFar]

-- With f(x) a reversible function from F_2^n -> F_2^n, decompose f_i(x) into
--   x_i + g_i(p_i) + h_i(q_i) = f_i(x)
--   where:
--     s_i = x_i + g_i(p_i),
--     p_i = (s_1, .., s_i-1, x_i+1, .., x_n),
--     q_i = (s_1, .., s_i-1, f_i+1, .., f_n).
-- Takes: the variable to use for s_i, the f_i we are decomposing, and the lists of functions p_i and q_i
-- Returns: if found, Just g_i and h_i in appropriate terms
decomposeOne :: Var -> Func -> [(Var, Func, Var, Func)] -> Maybe (Func, Func)
decomposeOne xiVar fiFunc piqiFuncs =
  -- f_i is written using x's
  -- p_ij is written using x's
  -- q_ij is written using x's
  -- g_i should be written using p_i's
  -- h_i should be written using q_i's
  trace ("decomposeOne " ++ show xiVar ++ " (" ++ show fiFunc ++ ") " ++ show piqiFuncs) $
    listToMaybe $ mapMaybe (\hiFunc -> giFuncMatching hiFunc >>= (\giFunc -> Just (giFunc, hiFunc))) (allFuncs qiTerms)
  where
    piSubs func = foldl' (flip . uncurry $ substitute) func [(pijFunc, pijVar) | (pijVar, pijFunc, _, _) <- piqiFuncs]
    qiSubs func = foldl' (flip . uncurry $ substitute) func [(qijFunc, qijVar) | (_, _, qijVar, qijFunc) <- piqiFuncs]
    piTerms = allTerms [pijVar | (pijVar, _, _, _) <- piqiFuncs]
    qiTerms = allTerms [qijVar | (_, _, qijVar, _) <- piqiFuncs]

    giFuncMatching hiFunc =
      trace ("giFuncMatching " ++ show hiFunc) $
        firstEquivalentFunc fiFunc (map (\giFunc -> sumOfFuncs [monomial xiVar, piSubs giFunc, qiSubs hiFunc]) (allFuncs piTerms))

    firstEquivalentFunc _ [] = Nothing
    firstEquivalentFunc wantedFunc (checkFunc : remain)
      | trace ("firstEquivalentFunc " ++ show wantedFunc ++ " [" ++ show checkFunc ++ "..]") False = undefined
      | wantedFunc == checkFunc = Just checkFunc
      | otherwise = firstEquivalentFunc wantedFunc remain


main :: IO ()
main = do
  --let makeFunc = (fromTermList . map fromVarList)
  let vf v = (Func . Set.singleton . Term . Set.singleton) v
  let (x1_, x2_, x3_) = (vf x1, vf x2, vf x3)
      (s1_, s2_, s3_) = (vf s1, vf s2, vf s3)
      -- (y1_, y2_, y3_) = (vf y1, vf y2, vf y3)
      y1_x = plus (times x2_ x3_) (plus (times x1_ x2_) (times x1_ x3_))
      y2_x = plus x1_ x2_
      y3_x = plus x1_ x3_
      s3_s = plus x3_ x1_
      s2_s = plus x2_ x1_
      s1_s = plus x1_ (times s2_ s3_)
      s3_x = s3_s
      s2_x = substitute s3_x s3 s2_s
      s1_x = substitute s2_x s2 (substitute s3_x s3 s1_s)
      y1_s = s1_
      y2_s = s2_
      y3_s = s3_
  putStrLn ("y1(x) = " ++ show y1_x)
  putStrLn ("y2(x) = " ++ show y2_x)
  putStrLn ("y3(x) = " ++ show y3_x)
  putStrLn ""
  putStrLn ("s1(x) = " ++ show s1_s)
  putStrLn ("s2(x) = " ++ show s2_s)
  putStrLn ("s3(x) = " ++ show s3_s)
  putStrLn ""
  putStrLn ("y1(s) = " ++ show y1_s)
  putStrLn ("y2(s) = " ++ show y2_s)
  putStrLn ("y3(s) = " ++ show y3_s)
  putStrLn ""
  putStrLn ("y1(s) -> x = " ++ show (substitute s1_x s1 (substitute s2_x s2 (substitute s3_x s3 y1_s))))
  putStrLn ("y2(s) -> x = " ++ show (substitute y1_x y1 (substitute s2_x s2 (substitute s3_x s3 y2_s))))
  putStrLn ("y3(s) -> x = " ++ show (substitute y1_x y1 (substitute y2_x y2 (substitute s3_x s3 y3_s))))
  putStrLn ""
  let res = decompose [(x1, y1, s1, y1_x), (x2, y2, s2, y2_x), (x3, y3, s3, y3_x)]
  case res of
    Nothing -> putStrLn("Failed to decompose")
    Just vghs -> forM_ vghs (\(xv, fv, sv, g, h) -> putStrLn (show fv ++ " = " ++ show xv ++ " (+) " ++ show g ++ " (= " ++ show sv ++ "; +) " ++ show h))


