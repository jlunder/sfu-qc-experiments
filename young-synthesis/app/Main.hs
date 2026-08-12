{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use unwords" #-}
module Main (main) where

import Data.Foldable (Foldable (foldl'), for_)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.QuickCheck qualified as QC

newtype Var = Var Int deriving (Ord, Eq)
newtype Term = Term (Set Var) deriving (Ord, Eq)
newtype Func = Func (Set Term) deriving (Ord, Eq)
newtype Assigns = Assigns (Set Var) deriving (Ord, Eq, Show)

instance Show Var where
  show (Var i) = "x" ++ show i

instance Show Term where
  show (Term vars)
    | Set.null vars = "1"
    | otherwise = intercalate " " (map show (Set.toList vars))

instance Show Func where
  show (Func terms)
    | Set.null terms = "0"
    | otherwise = intercalate " + " (map show (Set.toList terms))

[x0, x1, x2, x3, x4, x5, x6, x7, x8, x9] = [Var i | i <- [0 .. 9]]

fromVarList :: [Var] -> Term
fromVarList = Term . Set.fromList

fromTermList :: [Term] -> Func
fromTermList = Func . Set.fromList

assigns :: [(Var, Bool)] -> Assigns
assigns = Assigns . Set.fromList . (map fst) . (filter snd)

eval :: Func -> Assigns -> Bool
eval (Func terms) (Assigns trueVars) = foldl' sumTerm False (Set.toList terms)
  where
    sumTerm val (Term vars) = (val /= (all (`Set.member` trueVars) (Set.toList vars)))

assignedTrue :: Assigns -> Set Var
assignedTrue (Assigns trueSet) = trueSet

assignment :: Var -> Assigns -> Bool
assignment var (Assigns trueVars) = Set.member var trueVars

showEval :: Func -> Assigns -> [Var] -> String
showEval f varAssigns vars =
  (intercalate " " [if assignment v varAssigns then "1" else "0" | v <- reverse vars])
    ++ " = "
    ++ (if eval f varAssigns then "1" else "0")

truthTable :: [Var] -> [[(Var, Bool)]]
truthTable [] = [[]]
truthTable (v : remain) = map ((v, False) :) remainTable ++ map ((v, True) :) remainTable
  where
    remainTable = truthTable remain

assignsTable :: [Var] -> [Assigns]
assignsTable = map assigns . truthTable

allTerms :: [Var] -> [Term]
allTerms = map fromVarList . allTermVars . reverse
  where
    allTermVars [v] = [[], [v]]
    allTermVars (v : vars) = (allTermVars vars) ++ map (v :) (allTermVars vars)

allFuncs :: [Term] -> [Func]
allFuncs = map fromTermList . allFuncTerms . reverse
  where
    allFuncTerms [t] = [[], [t]]
    allFuncTerms (t : terms) = (allFuncTerms terms) ++ map (t :) (allFuncTerms terms)

findFunc :: Map Assigns Bool -> Func
findFunc evalMap = head (filter fMatches (allFuncs (allTerms evalUsedVars)))
  where
    fMatches f = all (\(a, r) -> (eval f a) == r) (Map.toList evalMap)
    evalUsedVars = Set.toList (foldl' Set.union Set.empty (map assignedTrue (Map.keys evalMap)))



main :: IO ()
main = do
  let f = (fromTermList . map fromVarList) [[x1, x2], [x2, x3], [x1, x3]]
  print (truthTable [x1, x2, x3])
  putStrLn ("f = " ++ show f)
  putStrLn ""
  for_
    ((allFuncs . allTerms) [x1, x2])
    print
  putStrLn ""
  let allAssigns3 = assignsTable [x1, x2, x3]
  for_
    allAssigns3
    (\varAssigns -> putStrLn ("f " ++ showEval f varAssigns [x1, x2, x3]))
  putStrLn ""
  let fEvalMap = Map.fromList [(a, eval f a) | a <- allAssigns3]
  print fEvalMap
  putStrLn ""
  print (findFunc fEvalMap)
