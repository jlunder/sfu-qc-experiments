module YoungSubgroupDecomp (decompose) where

import Prelude              hiding (all, and, any, not, or, (&&), (||))

import Control.Exception    qualified as Exception
import Control.Monad        (forM_, replicateM, when)
import Control.Monad.Reader (ReaderT (..), asks)

import Data.Array.IArray    (Array, (!))
import Data.Array.IArray    qualified as Array
import Data.Array.Unboxed   (UArray)
import Data.Set             (Set)
import Data.Set             qualified as Set
import Data.Word
import Ersatz
import GHC.Generics

import Algebra.Multilinear


-- With f(x) a reversible function from F_2^n -> F_2^n, decompose every f_i(x) into
--   x_i + g_i(g_1..i-1, x_i+1..x_n) + h_i(g_1..i-1, f_i+1..n) = f_i(x)
-- Takes: a list of n items, each: the variables to use for x_i, f_i, s_i; and the multilinear representation of f_i in terms of x_i
-- Note: the f_i and g_i variables should not appear in the definitions of f_i, and there should be exactly n free variables in the f_i's
-- Returns: if a decomposition is found, Just a list of n items, each: the variables x_i, f_i, s_i; and g_i in terms of x and s, h_i in terms of f and s
decompose :: [(Var, Var, Var, Func)] -> IO (Maybe [(Var, Var, Var, Func, Func)])
decompose funcs = do
  decomposeAll funcs []
  where
    decomposeAll :: [(Var, Var, Var, Func)] -> [(Var, Var, Var, Func, Func)] -> IO (Maybe [(Var, Var, Var, Func, Func)])
    decomposeAll [] soFar = return (Just soFar)
    decomposeAll ((xiVar, fiVar, siVar, fiFunc):remain) soFar = do
      -- (giFunc, hiFunc) <- decomposeOne xiVar siVar fiFunc piqiFuncs
      let giFunc = undefined
          hiFunc = undefined
      decomposeAll remain ((xiVar, fiVar, siVar, giFunc, hiFunc) : soFar)
      where
        -- TODO substitute s's, f's in gj, hj?
        -- sSoFar = map (\(xjVar, _, sjVar, gjFunc, _) -> let sjFunc = plus (monomial xjVar) (substitute gjFunc) in (sjVar, sjFunc, sjVar, sjFunc)) soFar
        -- xfRemain = map (\(xjVar, fjVar, _, fjFunc) -> (xjVar, monomial xjVar, fjVar, fjFunc)) sSoFar
        -- piqiFuncs = sSoFar ++ xfRemain

-- With f(x) a reversible function from F_2^n -> F_2^n, decompose f_i(x) into
--   x_i + g_i(p_i) + h_i(q_i) = f_i(x)
--   where:
--     s_i = x_i + g_i(p_i),
--     p_i = (s_1, .., s_i-1, x_i+1, .., x_n),
--     q_i = (s_1, .., s_i-1, f_i+1, .., f_n).
-- Takes: the variable to use for s_i, the f_i we are decomposing, and the lists of functions p_i and q_i
-- Returns: if found, Just g_i and h_i in appropriate terms
decomposeOne :: Var -> Var -> Func -> [(Var, Func, Var, Func)] -> IO (Maybe (Func, Func))
decomposeOne xiVar siVar fiFunc piqiFuncs = do
  undefined

{--
  -- make bits for possible terms of h_i
  --   list all combinations of terms of p's (which are either s or y)
  -- make bits for possible terms of g_i
  --   list all combinations of terms of q's (which are either x or s)
  -- assert g_i === [[ g_i ]]
  -- assert h_i === [[ h_i ]]
  -- assert s_i === [[ x_i + g_i ]]
  -- assert f_i === [[ s_i + h_i ]]

  -- f_i is written using x's
  -- p_ij is written using x's
  -- q_ij is written using x's
  -- g_i is written using p_i's
  -- h_i is written using q_i's

  hiTermBits <- mapM (const exists) [1 .. 2 ** n]
  fiTermBits <- mapM (const exists) [1 .. 2 ** n]
  assert $ siTermBits === addMultilinearTerms (monomialTermBits xiVar) hiTermBits



  (res, msol) <- solveWith anyminisat (problem ) -- init)
  return (res == Satisfied, msol)
  where
    n = length piqiFuncs

    problem :: (Applicative m, MonadSAT s m) => [(Func, Func)] -> Func -> m ([Bit], [Bit])
    problem sAndYFuncs = do
      -- mapM var indexes to 2^k s term bits
      -- mapM var indexes to 2^k y term bits
      -- assert y term bits
      -- assert s term bits
      termBits <- mapM (\t -> do
                            b <- exists
                            return (t, b)) terms
      assert (not b)
      return b
      where
        usedVars = foldl' (\vs (sf, yf) -> Set.union vs (vars f)) Set.empty funcs
        -- map vars to var indexes
        --

--}