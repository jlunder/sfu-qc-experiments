{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module XorAndGraph (XagNode (..), Xag (..), valid) where

import GHC.Generics (Generic)
import Test.QuickCheck

data XagNode
  = Xor !Int ![Int]
  | And !Int ![Int]
  | Const !Int !Bool
  | Var !Int !Int
  deriving (Eq, Generic, Ord, Show)

newtype Xag = Xag [XagNode] deriving (Eq, Generic, Ord, Show)

valid :: [XagNode] -> Bool
valid = validNodes 0
  where
    validNodes _ [] = True
    validNodes count (node : nodes) = validNode count node && validNodes (count + 1) nodes
    validNode count (Xor n inNs) = (n == count) && validList n inNs
    validNode count (And n inNs) = (n == count) && validList n inNs
    validNode count (Const n _) = n == count
    validNode count (Var n _) = n == count
    validList _ [] = False
    validList n inNs = all (>= 0) inNs && all (< n) inNs

instance Arbitrary Xag where
  arbitrary :: Gen Xag
  arbitrary = do
    s <- getSize
    nodes <- mapM genNode [2 .. s + 2]
    return $ Xag ([Const 0 False, Const 1 True] ++ nodes)
    where
      genNode n = oneof [genBinary n Xor, genBinary n And]

      genBinary n xa = do
        inputsCount <- choose (2 :: Int, 5)
        inputs <- genInputs inputsCount [0 .. n - 1]
        return $ xa n inputs

      genInputs 0 _ = return []
      genInputs n [] = mapM (const $ choose (0, 1)) [0 .. n - 1]
      genInputs n list = oneof [genConstInputs n 0 list, genConstInputs n 1 list, genListInputs n list]

      genConstInputs n k list = do
        moreInputs <- genInputs (n - 1) list
        return (k : moreInputs)

      genListInputs n list = do
        (input, listRemain) <- genTakeOneFromList list
        moreInputs <- genInputs (n - 1) listRemain
        return (input : moreInputs)

      genTakeOneFromList list = do
        index <- genIndex (length list - 1)
        let (hd, taken, tl) = case splitAt index list of
              (h, x : t) -> (h, x, t)
              _ -> undefined -- can't happen, but _you_ tell that to GHC
        return (taken, hd ++ tl)

      genIndex n = do
        unbiased <- choose (0, n * n * n - 1)
        return $ unbiased `div` (n * n)

-- eval :: Xag a -> (b -> b -> b) -> (b -> b -> b) -> (a -> b) -> b
-- eval (Xor x y) xorF andF litF = xorF (eval x xorF andF litF) (eval y xorF andF litF)
-- eval (And x y) xorF andF litF = andF (eval x xorF andF litF) (eval y xorF andF litF)
-- eval (Lit v) _ _ litF = litF v

-- andCost :: (Num b) => Xag a -> b
-- andCost g = eval g (+) (\x y -> x + y + 1) (const 0)

-- (Xor x y)
-- (And x y)1
-- (Lit v)