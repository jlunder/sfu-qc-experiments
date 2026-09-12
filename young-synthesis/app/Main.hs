{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-unused-top-binds -Wno-missing-signatures -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use unwords" #-}
module Main (main) where

import Data.Array.IArray          (Array, (!))
import Data.Array.IArray          qualified as Array
import Data.Array.Unboxed         (UArray)
import Data.Foldable              (Foldable (..), forM_, for_)
import Data.List                  (intercalate)
import Data.Map                   (Map)
import Data.Map                   qualified as Map
import Data.Maybe                 (listToMaybe, mapMaybe)
import Data.Set                   (Set)
import Data.Set                   qualified as Set

import Debug.Trace                (trace)

import Algebra.Multilinear
import SubgroupDecomp.Enumeration qualified as SGDecomp

-- truthTable :: [Var] -> [[(Var, Bool)]]
-- truthTable [] = [[]]
-- truthTable (v : remain) = map ((v, False) :) remainTable ++ map ((v, True) :) remainTable
--   where
--     remainTable = truthTable remain

-- assignsTable :: [Var] -> [Assigns Bool]
-- assignsTable = map assigns . truthTable

instance Show Var where
  show v@(Var i) = maybe ("x" ++ show i) id (varNames Map.!? v)

instance Show Term where
  show (Term vs)
    | Set.null vs = "1"
    | otherwise = intercalate " " (map show (Set.toList vs))

instance Show Func where
  show (Func terms)
    | Set.null terms = "0"
    | otherwise = intercalate " + " (map show (Set.toList terms))

varNames :: Map Var String
varNames =  Map.fromList [(x1, "x1"), (x2, "x2"), (x3, "x3"), (x4, "x4"),
                          (s1, "s1"), (s2, "s2"), (s3, "s3"), (s4, "s4"),
                          (y1, "y1"), (y2, "y2"), (y3, "y3"), (y4, "y4")]

x1, x2, x3, x4, s1, s2, s3, s4, y1, y2, y3, y4 :: Var
[x1, x2, x3, x4,
 s1, s2, s3, s4,
 y1, y2, y3, y4] = [Var i | i <- [1 .. 12]]

showEval :: Func -> Assigns Bool -> [Var] -> String
showEval f vals vs =
  (intercalate " " [if assignment vals v then "1" else "0" | v <- reverse vs])
    ++ " = "
    ++ (if eval f vals then "1" else "0")

-- findFunc :: Map (Assigns Bool) Bool -> Maybe Func
-- findFunc evalMap = listToMaybe (filter fMatches (allFuncs (allTerms evalUsedVars)))
--   where
--     fMatches f = all (\(a, r) -> (eval f a) == r) (Map.toList evalMap)
--     evalUsedVars = Set.toList (foldl' Set.union Set.empty (map assignedTrue (Map.keys evalMap)))
--     assignedTrue (Assigns vals) = Set.fromList (map fst (filter snd (Map.toList vals)))


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
  let res = SGDecomp.decompose [(x1, y1, s1, y1_x), (x2, y2, s2, y2_x), (x3, y3, s3, y3_x)]
  case res of
    Nothing -> putStrLn("Failed to decompose")
    Just vghs -> forM_ vghs (\(xv, fv, sv, g, h) -> putStrLn (show fv ++ " = " ++ show xv ++ " (+) " ++ show g ++ " (= " ++ show sv ++ "; +) " ++ show h))


