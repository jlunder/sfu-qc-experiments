{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-incomplete-uni-patterns #-}
module Algebra.Multilinear (
  Var(..), Term(..), Func(..), Assigns(..),
  fromVarList, fromTermList, monomial, varList, varSet, termList, termSet,
  zeroFunc, oneFunc,
  plus, sumOfFuncs, times, productOfFuncs,
  substitute, assigns, eval,
  assignment, vars,
  allTerms, allFuncs) where

import Prelude   hiding (all, and, any, not, or, (&&), (||))

import Data.Map  (Map)
import Data.Map  qualified as Map
import Data.Set  (Set)
import Data.Set  qualified as Set
import Ersatz    (Boolean (..))


newtype Var = Var Int
  deriving (Eq, Ord)
newtype Term = Term (Set Var)
  deriving (Eq, Ord)
newtype Func = Func (Set Term)
  deriving (Eq, Ord)
newtype Assigns b = Assigns (Map Var b)
  deriving (Eq, Ord)

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
sumOfFuncs [] = zeroFunc
sumOfFuncs (func : remain) = foldl' plus func remain

times :: Func -> Func -> Func
times f g = foldl' (\p pt -> plus p (Func (Set.singleton pt))) zeroFunc prodTerms
  where
    prodTerms = [Term (Set.union ft gt) | ft <- map varSet (termList f), gt <- map varSet (termList g)]

productOfFuncs :: [Func] -> Func
productOfFuncs [] = oneFunc
productOfFuncs (func : remain) = foldl' plus func remain

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

