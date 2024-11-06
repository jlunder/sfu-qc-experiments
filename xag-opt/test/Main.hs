module Main (main) where

import Test.QuickCheck
import XorAndGraph

main :: IO ()
main = do
  xag <- generate (arbitrary :: Gen Xag)
  print xag
