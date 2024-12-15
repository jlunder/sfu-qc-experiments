module Main (main) where

import Logic.MockTurtle
import Logic.MockTurtle.XAG qualified as XAG

reducible1 :: XAG.Graph
reducible1 =
  XAG.Graph
    [ XAG.Xor 3 0 2,
      XAG.Not 4 0,
      XAG.And 5 4 1,
      XAG.Not 6 3,
      XAG.Not 7 5,
      XAG.And 8 6 7,
      XAG.Xor 9 5 2,
      XAG.And 10 8 9
    ]
    [0, 1, 2]
    [10]

main :: IO ()
main = do
  print reducible1
  putStrLn "Optimizing graph"
  let opt = optimize reducible1
  print opt
  return ()
