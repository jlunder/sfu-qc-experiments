{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-unused-top-binds -Wno-missing-signatures -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use unwords" #-}
module SubgroupDecomp.SmarterEnum (decompose) where

import Data.Array.IArray   (Array, (!))
import Data.Array.IArray   qualified as Array
import Data.Array.Unboxed  (UArray)
import Data.Foldable       (Foldable (..), forM_, for_)
import Data.List           (intercalate)
import Data.Map            (Map)
import Data.Map            qualified as Map
import Data.Maybe          (listToMaybe, mapMaybe)
import Data.Set            (Set)
import Data.Set            qualified as Set

import Debug.Trace         (trace)

import Algebra.Multilinear

-- With f(x) a reversible function from F_2^n -> F_2^n, decompose every f_i(x) into
--   x_i + g_i(g_1..i-1, x_i+1..x_n) + h_i(g_1..i-1, f_i+1..n) = f_i(x)
-- Takes: a list of n items, each: the variables to use for x_i, f_i, s_i; and the multilinear representation of f_i in terms of x_i
-- Note: the f_i and g_i variables should not appear in the definitions of f_i, and there should be exactly n free variables in the f_i's
-- Returns: if a decomposition is found, Just a list of n items, each: the variables x_i, f_i, s_i; and g_i in terms of x and s, h_i in terms of f and s
decompose :: [(Var, Var, Var, Func)] -> Maybe [(Var, Var, Var, Func, Func)]
decompose funcs = decomposeAll funcs []
  where
    decomposeAll :: [(Var, Var, Var, Func)] -> [(Var, Var, Var, Func, Func)] -> Maybe [(Var, Var, Var, Func, Func)]
    decomposeAll [] soFar = Just [(xiVar, fiVar, siVar, giFunc, hiFunc) | (xiVar, fiVar, siVar, giFunc, hiFunc) <- soFar]
    decomposeAll ((xiVar, fiVar, siVar, fiFunc):remain) soFar =
      decomposeOne xiVar fiFunc sjVars fjFuncs >>=
          (\(giFunc, hiFunc) -> decomposeAll (substRemain giFunc) ((xiVar, fiVar, siVar, giFunc, hiFunc) : soFar))
      where
        sjVars = [sjVar | (_, _, sjVar, _, _) <- soFar]
        fjFuncs = [(fjVar, fjFunc) | (_, fjVar, _, fjFunc) <- remain]

        -- Rewrite the remaining f_j polynomials in terms of s_i, so that x_i
        -- doesn't appear in them anymore. This is fairly trivial only because
        -- x_i by definition doesn't appear in g_i, and we have defined
        -- s_i = x_i + g_i, therefore x_i = g_i + s_i.
        -- Doing this incrementally as we recurse leaves us in the happy
        -- position that we are working in a space where the f_i is written in
        -- the same variables as g_i, making g_i trivial to find once we have
        -- h_i (this is important in decomposeOne below -- well, more
        -- specifically it's going to give us something technically correct
        -- anyway, but if we're trying to synthesize logic and we have
        -- computed a bunch of qubits to s_j already, it's nice if the g_i
        -- we're synthesizing is written exactly in terms of what we have to
        -- hand; it saves us doing inversions.
        substRemain giFunc = [(xjVar, fjVar, sjVar, substitute (plus giFunc (monomial siVar)) xiVar fjFunc) | (xjVar, fjVar, sjVar, fjFunc) <- remain]

-- With f(x) a reversible function from F_2^n -> F_2^n, decompose f_i(x) into
--   x_i + g_i(p_i) + h_i(q_i) = f_i(x)
--   where:
--     s_i = x_i + g_i(p_i),
--     p_i = (s_1, .., s_i-1, x_i+1, .., x_n),
--     q_i = (s_1, .., s_i-1, f_i+1, .., f_n).
-- Takes: the f_i we are decomposing and its input x_i; the s_j's, j < i; and
--        f_j's vars and polynomials in x, j > i
-- Returns: if found, Just g_i and h_i in appropriate terms, otherwise Nothing
decomposeOne :: Var -> Func -> [Var] -> [(Var, Func)] -> Maybe (Func, Func)
decomposeOne xiVar fiFunc sjVars fjFuncs =
  -- g_i should be written using s_j's, x_j's
  -- h_i should be written using s_j's, f_j's
  trace ("i = " ++ show (length sjVars)) $
  trace ("trivial f_i: " ++ showFunc showVarDefault trivialFiPart) $
  trace ("nontrivial f_i: " ++ showFunc showVarDefault nonTrivialFiPart) $
  maybeHiFunc >>= (\(hiFunc, hiSubstFunc) -> Just (giFuncMatching hiSubstFunc, hiFunc))
  where
    trivialVars = Set.fromList sjVars
    isTrivialHiTerm t = Set.null (varSet t Set.\\ trivialVars)
    trivialHiFuncPart hF = Func (Set.filter isTrivialHiTerm (termSet hF))
    nonTrivialHiFuncPart hF = Func (Set.filter (not . isTrivialHiTerm) (termSet hF))
    differsOnlyByTrivial hA hB =
      trace ("differs? nt hA = " ++ (showFunc showVarDefault (nonTrivialHiFuncPart hA)) ++ "; nt hB = "
                                 ++ (showFunc showVarDefault (nonTrivialHiFuncPart hB)) ++ "; hA + hB = "
                                 ++ (showFunc showVarDefault (plus (nonTrivialHiFuncPart hA) (nonTrivialHiFuncPart hB)))) $
      plus (nonTrivialHiFuncPart hA) (nonTrivialHiFuncPart hB) == zeroFunc

    -- The "trivial" part of the function we're constructing, that is, the
    -- terms of g_i + h_i (or equivalently f + x_i), where the terms only
    -- contain s_j as variables (where j > i). (It's trivial because we can
    -- just synthesize these terms directly, we don't need to enumerate them
    -- because they're not substituted.)
    trivialFiPart = trivialHiFuncPart (plus (monomial xiVar) fiFunc)

    -- Everything else in the function we're constructing, that is, the
    -- terms of g_i + h_i (or equivalently f + x_i), where the terms contain
    -- at least one variable not in s_j (where j > i).
    nonTrivialFiPart = nonTrivialHiFuncPart (plus (monomial xiVar) fiFunc)

    relevantFjFuncs = fjFuncs
    relevantSjVars = filter (`Set.member` (Set.fromList sjVars)) (Set.toList (vars fiFunc))

    nonTrivialHiCandidates = allFuncsFromSubst (relevantFjFuncs ++ zip relevantSjVars (map monomial relevantSjVars))
    maybeHiFunc =
      listToMaybe (filter (differsOnlyByTrivial nonTrivialFiPart . snd) nonTrivialHiCandidates)
        >>= (\(nonTrivialHiPart, nonTrivialHiSubstPart) -> Just (plus nonTrivialHiPart trivialFiPart, plus nonTrivialHiSubstPart trivialFiPart))

    -- hiSubstFunc should be written in the same variables as the desired g_i
    giFuncMatching hiSubstFunc = plus (plus fiFunc hiSubstFunc) (monomial xiVar)

