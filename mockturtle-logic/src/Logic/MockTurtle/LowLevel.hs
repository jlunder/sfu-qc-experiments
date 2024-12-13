-- This FFI pattern cribbed from https://luctielen.com/posts/calling_cpp_from_haskell/, credit to Luc Tielen

module Logic.MockTurtle.LowLevel where

import Control.Exception (mask_)
import Foreign

data XAGWrap

data XAGBuilderWrap

foreign import ccall unsafe "xag_alloc"
  allocXAGWrap :: IO (Ptr XAGWrap)

foreign import ccall unsafe "&xag_free"
  freeXAGWrap :: FunPtr (Ptr XAGWrap -> IO ())

allocForeignXAGWrap :: IO (ForeignPtr XAGWrap)
allocForeignXAGWrap = mask_ $ newForeignPtr freeXAGWrap =<< allocXAGWrap

foreign import ccall unsafe "xag_builder_alloc"
  allocXAGBuilderWrap :: Ptr XAGWrap -> IO (Ptr XAGBuilderWrap)

foreign import ccall unsafe "&xag_builder_free" freeXAGBuilderWrap :: FunPtr (Ptr XAGBuilderWrap -> IO ())

allocForeignXAGBuilderWrap :: Ptr XAGWrap -> IO (ForeignPtr XAGBuilderWrap)
allocForeignXAGBuilderWrap xagP = mask_ $ newForeignPtr freeXAGBuilderWrap =<< allocXAGBuilderWrap xagP

foreign import ccall unsafe "xag_builder_create_pi"
  xagBuilderWrapCreatePI :: Ptr XAGBuilderWrap -> Int -> IO ()

foreign import ccall unsafe "xag_builder_create_const"
  xagBuilderWrapCreateConst :: Ptr XAGBuilderWrap -> Int -> Bool -> IO ()

foreign import ccall unsafe "xag_builder_create_not"
  xagBuilderWrapCreateNot :: Ptr XAGBuilderWrap -> Int -> Int -> IO ()

foreign import ccall unsafe "xag_builder_create_xor"
  xagBuilderWrapCreateXor :: Ptr XAGBuilderWrap -> Int -> Int -> Int -> IO ()

foreign import ccall unsafe "xag_builder_create_and"
  xagBuilderWrapCreateAnd :: Ptr XAGBuilderWrap -> Int -> Int -> Int -> IO ()

foreign import ccall unsafe "xag_builder_create_po"
  xagBuilderWrapCreatePO :: Ptr XAGBuilderWrap -> Int -> IO ()
