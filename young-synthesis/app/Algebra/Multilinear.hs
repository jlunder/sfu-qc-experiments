{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-incomplete-uni-patterns #-}
module Algebra.Multilinear (
  Var(..), Term(..), Func(..), Assigns(..),
  fromVarList, fromTermList, monomial, varList, varSet, termList, termSet,
  showFunc, showTerm, showVarDefault,
  zeroFunc, oneFunc,
  plus, sumOfFuncs, times, productOfFuncs,
  substitute, assigns, eval,
  assignment, vars,
  allTerms, allFuncs, allFuncsFromSubst) where

import Prelude     hiding (all, and, any, not, or, (&&), (||))

import Data.Map    (Map)
import Data.Map    qualified as Map
import Data.Set    (Set)
import Data.Set    qualified as Set
import Ersatz      (Boolean (..))

import Data.List   (intercalate)
import Debug.Trace (trace)


newtype Var = Var Int
  deriving (Eq, Ord)
newtype Term = Term (Set Var)
  deriving (Eq, Ord)
newtype Func = Func (Set Term)
  deriving (Eq, Ord)
newtype Assigns b = Assigns (Map Var b)
  deriving (Eq, Ord)

showFunc :: (Var -> [Char]) -> Func -> String
showFunc showVar (Func ts)
  | ts == Set.empty = "0"
  | otherwise = intercalate " + " (map (showTerm showVar) . Set.toList $ ts)

showTerm :: (Var -> [Char]) -> Term -> String
showTerm showVar (Term vs)
  | vs == Set.empty = "1"
  | otherwise = intercalate " " (map showVar . Set.toList $ vs)

showVarDefault :: Var -> [Char]
showVarDefault (Var i) = "X" ++ show i

fromVarList :: [Var] -> Term
fromVarList = Term . Set.fromList

fromTermList :: [Term] -> Func
fromTermList = Func . Set.fromList

monomial :: Var -> Func
monomial v = Func . Set.singleton . Term . Set.singleton $ v

varList :: Term -> [Var]
varList (Term vs) = Set.toList vs

varSet :: Term -> Set Var
varSet (Term vs) = vs

termList :: Func -> [Term]
termList (Func terms) = Set.toList terms

termSet :: Func -> Set Term
termSet (Func terms) = terms

zeroFunc :: Func
zeroFunc = Func Set.empty

oneFunc :: Func
oneFunc = Func (Set.singleton (Term Set.empty))

plus :: Func -> Func -> Func
plus (Func fTerms) (Func gTerms) = Func ((Set.union fTerms gTerms) Set.\\ (Set.intersection fTerms gTerms))

sumOfFuncs :: [Func] -> Func
sumOfFuncs []              = zeroFunc
sumOfFuncs (func : remain) = foldl' plus func remain

times :: Func -> Func -> Func
times f g = foldl' (\p pt -> plus p (Func (Set.singleton pt))) zeroFunc prodTerms
  where
    prodTerms = [Term (Set.union ft gt) | ft <- map varSet (termList f), gt <- map varSet (termList g)]

productOfFuncs :: [Func] -> Func
productOfFuncs []              = oneFunc
productOfFuncs (func : remain) = foldl' times func remain

substitute :: Func -> Var -> Func -> Func
substitute withF forV inF =
  plus (Func unmodTerms) (times withF (Func substTermsWithoutForV))
  where
    -- The terms not containing forV, which pass through unmodified
    unmodTerms   = Set.filter (Set.notMember forV . varSet) (termSet inF)
    -- The terms containing forV, to be stripped and combined with withF
    substTerms   = Set.filter (Set.member forV . varSet) (termSet inF)
    substTermsWithoutForV = Set.map (Term . (Set.delete forV) . varSet) substTerms

assigns :: [(Var, b)] -> Assigns b
assigns = Assigns . Map.fromList

eval :: Boolean b => Func -> Assigns b -> b
eval (Func terms) vals = sumTerms (Set.toList terms)
  where
    sumTerms [] = false
    sumTerms ts = (any (prodVars . varList) ts)

    prodVars [] = true
    prodVars vs = (all (assignment vals) vs)

assignment :: Assigns b -> Var -> b
assignment (Assigns vmap) var = vmap Map.! var

vars :: Func -> Set Var
vars f = foldl' (\vs t -> Set.union vs (varSet t)) Set.empty (termList f)

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

allFuncsFromSubst :: [(Var, Func)] -> [(Func, Func)]
allFuncsFromSubst substList =
  -- trace "blurp" $
  -- traceAll (map (\(f1, f2) -> "f(s): " ++ showFunc showVarDefault f1 ++ ", f(x): " ++ showFunc showVarDefault f2) $
  --               funcsByOrder substVars) $
  funcsByOrder substVars
  where
    -- traceAll [] result                = result
    -- traceAll (msg : msgRemain) result = trace msg $ traceAll msgRemain result

    substVars = fst . unzip $ substList
    substMap = Map.fromList substList

    termsOfOrder _ termVars 0 = [termVars]
    termsOfOrder [] _ _ = []
    termsOfOrder unusedVars _ n | length unusedVars < n = []
    termsOfOrder (v : unusedVarsRemain) termVars n =
      termsOfOrder unusedVarsRemain (Set.insert v termVars) (n - 1)
        ++ termsOfOrder unusedVarsRemain termVars n


    funcsByOrder :: [Var] -> [(Func, Func)]
    funcsByOrder termVars =
      map (\(ts, tf) -> (Func ts, tf)) (allSelections allTermFuncsOrdered)
      where
        allTermsOrdered :: [Term]
        allTermsOrdered = map Term (concat [termsOfOrder termVars Set.empty n | n <- [1 .. length termVars]])

        allTermFuncsOrdered :: [(Term, Func)]
        allTermFuncsOrdered = map (\t -> (t, substitutedTermFunc t)) allTermsOrdered

        -- substitute all the vars in the term using substMap, which rewrites f_j vars to x_j vars
        substitutedTermFunc :: Term -> Func
        substitutedTermFunc t = productOfFuncs (map (\v -> Map.findWithDefault (monomial v) v substMap) (varList t))

        allSelections :: [(Term, Func)] -> [(Set Term, Func)]
        allSelections termFuncs = nextSelectionsGen [(Set.empty, zeroFunc)] termFuncs
          where
            nextSelectionsGen lastGen [] = lastGen
            nextSelectionsGen lastGen ((t, f) : remain) = nextSelectionsGen thisGen remain
              where thisGen = lastGen ++ map (\(lt, lf) -> (Set.insert t lt, plus lf f)) lastGen


