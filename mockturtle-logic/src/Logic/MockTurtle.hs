module Logic.MockTurtle (XAG.Graph (..), optimize) where

import Foreign (Ptr, withForeignPtr)
import Logic.MockTurtle.LowLevel
import Logic.MockTurtle.XAG qualified as XAG

optimize :: XAG.Graph -> IO XAG.Graph
optimize g = do
  mtxag <- allocForeignXAGWrap
  withForeignPtr mtxag (buildMTXAG g)
  return g

buildMTXAG :: XAG.Graph -> Ptr XAGWrap -> IO ()
buildMTXAG g mtxag = do
  builder <- allocForeignXAGBuilderWrap mtxag
  withForeignPtr builder doBuild
  where
    doBuild :: Ptr XAGBuilderWrap -> IO ()
    doBuild b = do
      mapM_ (buildInput b) (XAG.inputIDs g)
      mapM_ (buildNode b) (XAG.nodes g)
      mapM_ (buildOutput b) (XAG.outputIDs g)

    buildOutput :: Ptr XAGBuilderWrap -> Int -> IO ()
    buildOutput b nID = xagBuilderWrapCreatePO b (fromIntegral nID)

    buildNode :: Ptr XAGBuilderWrap -> XAG.Node -> IO ()
    buildNode b (XAG.Const nID v) =
      xagBuilderWrapCreateConst b (fromIntegral nID) v
    buildNode b (XAG.Not nID xID) =
      xagBuilderWrapCreateNot b (fromIntegral nID) xID
    buildNode b (XAG.Xor nID xID yID) =
      xagBuilderWrapCreateXor b (fromIntegral nID) xID yID
    buildNode b (XAG.And nID xID yID) =
      xagBuilderWrapCreateAnd b (fromIntegral nID) xID yID

    buildInput :: Ptr XAGBuilderWrap -> Int -> IO ()
    buildInput b nID = xagBuilderWrapCreatePI b (fromIntegral nID)
