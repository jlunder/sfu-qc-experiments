module Xag.Optimize(dowork) where

--import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import SAT.MiniSat
--import Xag.Graph

dowork :: Maybe (Map.Map Int Bool)
dowork = solve (Var (2 :: Int) :<->: Yes)

