module Logic.MockTurtle (XAG.Graph (..), optimize) where

import Foreign (Ptr, withForeignPtr)
import GHC.IO (unsafePerformIO)
import Logic.MockTurtle.LowLevel
import Logic.MockTurtle.XAG qualified as XAG

-- Don't worry about inlining or CSE in this use of unsafePerformIO -- there
-- really should be no visible side-effects in this use, so strange behaviour,
-- repetition or elimination, in how the call to optimize is made is actually
-- okay, maybe even desirable.

-- It might be possible for some/all of these FFI calls not to depend on the IO
-- monad, but using the IO monad and containing it within a call to
-- unsafePerformIO does ensure the expected ordering.

-- About that: the place ordering is likely to go very sideways is if the build
-- and read steps aren't properly ordered with respect to each other. Because
-- Ptr XAGWrap doesn't change throughout the build process, Haskell has no
-- visibility into the state changes and could just decide to start reading as
-- soon as the thing is allocated, without ever even doing the build. It's the
-- same Ptr, right? That's where the IO usage saves our butts.

-- As long as we are done the whole process before we get back from
-- unsafePerformIO, though, we should be fine.

optimize :: XAG.Graph -> XAG.Graph
optimize g =
  unsafePerformIO
    ( do
        mtxagFP <- allocForeignXAGWrap
        withForeignPtr
          mtxagFP
          ( \mtxag -> do
              buildMTXAG g mtxag
              readMTXAG mtxag
          )
    )

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

readMTXAG :: Ptr XAGWrap -> IO XAG.Graph
readMTXAG mtxag = do
  return $ XAG.Graph nodes inIDs outIDs
  where nodes = []
        inIDs = []
        outIDs = []
