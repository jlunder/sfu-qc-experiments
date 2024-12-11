-- This FFI pattern cribbed from https://luctielen.com/posts/calling_cpp_from_haskell/, credit to Luc Tielen

module Logic.MockTurtle.LowLevel where

import Control.Exception (mask_)
import Foreign
import Foreign.C.Types

data Example

data ExampleIterator

foreign import ccall unsafe "example_create"
  exampleCreate ::
    IO (Ptr Example)

foreign import ccall unsafe "&example_destroy"
  exampleDestroy ::
    FunPtr (Ptr Example -> IO ())

mkExample :: IO (ForeignPtr Example)
mkExample =
  mask_ $ newForeignPtr exampleDestroy =<< exampleCreate

foreign import ccall unsafe "ffi_iterator_create"
  ffiIteratorCreate ::
    Ptr Example -> IO (Ptr ExampleIterator)

foreign import ccall unsafe "&ffi_iterator_destroy"
  ffiIteratorDestroy ::
    FunPtr (Ptr ExampleIterator -> IO ())

foreign import ccall unsafe "ffi_iterator_has_next"
  ffiIteratorHasNext ::
    Ptr ExampleIterator -> IO CBool

foreign import ccall unsafe "ffi_iterator_next"
  ffiIteratorNext ::
    Ptr ExampleIterator -> IO CInt

mkExampleIterator :: Ptr Example -> IO (ForeignPtr ExampleIterator)
mkExampleIterator ptr =
  mask_ $ newForeignPtr ffiIteratorDestroy =<< ffiIteratorCreate ptr

-- Helper function for looping over the data and collecting the results on
-- the Haskell side.
collectValues :: Ptr ExampleIterator -> IO [CInt]
collectValues = go []
  where
    go acc iterator = do
      CBool hasNext <- ffiIteratorHasNext iterator
      if hasNext == 1
        then do
          value <- ffiIteratorNext iterator
          go (value : acc) iterator
        else pure acc
