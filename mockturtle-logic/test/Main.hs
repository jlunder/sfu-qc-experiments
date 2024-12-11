module Main (main) where

import Foreign
import Logic.MockTurtle.LowLevel


main :: IO ()
main = do
  obj <- mkExample
  withForeignPtr obj $ \objPtr -> do
    iterator <- mkExampleIterator objPtr
    withForeignPtr iterator $ \iteratorPtr -> do
      values <- collectValues iteratorPtr
      -- Now you can process the values like you would
      -- for any other Haskell value.
      print $ map (+ 1) values
