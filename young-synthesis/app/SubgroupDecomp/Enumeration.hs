{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-unused-top-binds -Wno-missing-signatures -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use unwords" #-}
module SubgroupDecomp.Enumeration (decompose) where

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

import Algebra.Multilinear

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
  listToMaybe $ mapMaybe (\hiFunc -> giFuncMatching hiFunc >>= (\giFunc -> Just (giFunc, hiFunc))) (allFuncs qiTerms)
  where
    piSubs func = foldl' (flip . uncurry $ substitute) func [(pijFunc, pijVar) | (pijVar, pijFunc, _, _) <- piqiFuncs]
    qiSubs func = foldl' (flip . uncurry $ substitute) func [(qijFunc, qijVar) | (_, _, qijVar, qijFunc) <- piqiFuncs]
    piTerms = allTerms [pijVar | (pijVar, _, _, _) <- piqiFuncs]
    qiTerms = allTerms [qijVar | (_, _, qijVar, _) <- piqiFuncs]

    giFuncMatching hiFunc =
      firstEquivalentFunc fiFunc (map (\giFunc -> sumOfFuncs [monomial xiVar, piSubs giFunc, qiSubs hiFunc]) (allFuncs piTerms))

    firstEquivalentFunc _ [] = Nothing
    firstEquivalentFunc wantedFunc (checkFunc : remain)
      | wantedFunc == checkFunc = Just checkFunc
      | otherwise = firstEquivalentFunc wantedFunc remain

