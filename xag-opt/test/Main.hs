module Main (main) where

import Test.QuickCheck
import XorAndGraph

{-
  xag2 <- generate (resize 0 (arbitrary :: Gen Xag))
  print xag2
  xag3 <- generate (resize 1 (arbitrary :: Gen Xag))
  print xag3
  xag4 <- generate (resize 2 (arbitrary :: Gen Xag))
  print xag4
  xag5 <- generate (resize 3 (arbitrary :: Gen Xag))
  print xag5
  xag6 <- generate (resize 4 (arbitrary :: Gen Xag))
  print xag6
  xag7 <- generate (resize 5 (arbitrary :: Gen Xag))
  print xag7
-}

prop_valid :: Xag -> Bool
prop_valid = valid


main :: IO ()
main = do
  -- xag <- generate (arbitrary :: Gen Xag)
  -- print xag
  -- print $ valid xag
  quickCheck prop_valid
