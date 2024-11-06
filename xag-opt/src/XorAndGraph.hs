{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module XorAndGraph (XagNode (..), Xag (..), valid) where

import GHC.Float (sqrtFloat)
import GHC.Generics (Generic)
import Test.QuickCheck

{-
The Xag is a list of XagNode; each node has a nodeId, uniquely identifying it.
Nodes are ordered by increasing nodeId, and the graph should be acyclic, with
each XagNode referring only to prior XagNodes via its inputs. References to
nodeIds not in the list are free variables.

valid checks these properties.

Note that although the Const node can take value True or False, only True is
useful in analysis -- False inputs to Xor can be trivially pruned, and False
inputs to And mean the And can be trivially converted to a Const False.

The Arbitrary makes a valid, arbitrary Xag with (size permitting) one True
Const node, a gap for free variables, and then a number of Xor and And nodes
to fill the list to the arbitrary size. Inputs are weighted to prefer earlier
nodes, so as to make shallower graphs.
-}

data XagNode
  = Const {nodeId :: !Int, value :: !Bool}
  | Xor {nodeId :: !Int, inputs :: ![Int]}
  | And {nodeId :: !Int, inputs :: ![Int]}
  deriving (Eq, Generic, Ord, Show)

newtype Xag = Xag [XagNode] deriving (Eq, Generic, Ord, Show)

valid :: Xag -> Bool
valid (Xag ns) = validNodes 0 ns
  where
    validNodes _ [] = True
    validNodes nextId (node : nodes) = validNode nextId node && validNodes (nodeId node + 1) nodes
    validNode nextId (Xor n inNs) = (n >= nextId) && validList n inNs
    validNode nextId (And n inNs) = (n >= nextId) && validList n inNs
    validNode nextId (Const n _) = n >= nextId
    -- validNode nextId (Var n) = n >= nextId
    validList _ [] = False
    validList n inNs = all (>= 0) inNs && all (< n) inNs

instance Arbitrary Xag where
  arbitrary :: Gen Xag
  arbitrary = do
    nNodes <- getSize
    let constNodes = [Const 0 True | nNodes > 0]
    let nVarNodes = length constNodes + (floor . sqrtFloat . fromIntegral) nNodes
    let nXaNodes = nNodes + (nVarNodes - length constNodes)
    -- varNodes <- mapM genVarNode [length constNodes .. nVarNodes - 1]
    xaNodes <- mapM genXANode [nVarNodes .. nXaNodes - 1]
    return $ Xag (constNodes {- ++ varNodes -} ++ xaNodes)
    where
      -- genVarNode = return . Var
      genXANode n = oneof [genBinary n Xor, genBinary n And]

      genBinary n xa = do
        inputsCount <- choose (2 :: Int, 5)
        newInputs <- genInputs inputsCount [0 .. n - 1]
        return $ xa n newInputs

      genInputs 0 _ = return []
      genInputs n [] = mapM (const $ choose (0, 1)) [0 .. n - 1]
      genInputs n list = oneof [genConstInputs n list, genListInputs n list]

      genConstInputs n list = do
        k <- choose (0, 1)
        moreInputs <- genInputs (n - 1) list
        return (k : moreInputs)

      genListInputs _ [] = undefined
      genListInputs n list = do
        (input, listRemain) <- genTakeOneFromList list
        moreInputs <- genInputs (n - 1) listRemain
        return (input : moreInputs)

      genTakeOneFromList [] = undefined
      genTakeOneFromList list = do
        index <- genIndex (length list)
        let (hd, taken, tl) = case splitAt index list of
              (h, x : t) -> (h, x, t)
              _ -> undefined -- can't happen, but _you_ tell that to GHC
        return (taken, hd ++ tl)

      genIndex 0 = undefined
      genIndex n = do
        unbiased <- choose (0, n * n - 1)
        return $ unbiased `div` n

-- eval :: Xag a -> (b -> b -> b) -> (b -> b -> b) -> (a -> b) -> b
-- eval (Xor x y) xorF andF litF = xorF (eval x xorF andF litF) (eval y xorF andF litF)
-- eval (And x y) xorF andF litF = andF (eval x xorF andF litF) (eval y xorF andF litF)
-- eval (Lit v) _ _ litF = litF v

-- andCost :: (Num b) => Xag a -> b
-- andCost g = eval g (+) (\x y -> x + y + 1) (const 0)

-- (Xor x y)
-- (And x y)1
-- (Lit v)
