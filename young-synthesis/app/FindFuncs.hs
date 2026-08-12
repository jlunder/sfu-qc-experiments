module FindFuncs (findFuncs) where

import Prelude hiding (all, and, any, not, or, (&&), (||))

import Control.Monad (forM_, replicateM, when)
import Control.Monad.Reader (ReaderT (..), asks)

import Data.Array (Array, (!))
import Data.Array qualified as Array
import Data.Word
import Ersatz
import GHC.Generics

findFuncs = undefined

newtype Cell = Cell Bit4
  deriving (Show, Generic)

instance Boolean Cell
instance Variable Cell
instance Equatable Cell

instance Codec Cell where
  type Decoded Cell = Word8
  decode s (Cell b) = decode s b
  encode n
    | 1 <= n && n <= 9 = Cell (encode n)
    | otherwise = error ("Cell encode: invalid value " ++ show n)

type Index = (Word8, Word8)

type Grid = Array Index Cell

data Env = Env
  { -- | The puzzle.
    envCellArray :: Grid,
    -- | The possible values for any cell.
    envValues :: [Cell]
  }
  deriving (Show)

problem ::
  (Applicative m, MonadSAT s m) =>
  Array Index Word8 ->
  m Grid
problem initValues = do
  cellArray <-
    Array.listArray range
      <$> replicateM (Array.rangeSize range) exists

  runReaderT problem' $ Env cellArray (map encode [1 .. 9])

  -- Assert all initial values.
  forM_ (Array.assocs initValues) $ \(idx, val) ->
    when (1 <= val && val <= 9) $
      assert $
        (cellArray ! idx) === encode val

  return cellArray

problem' :: (MonadSAT s m) => ReaderT Env m ()
problem' = do
  legalValues
  mapM_ allDifferent (subsquares ++ horizontal ++ vertical)

-- | Assert that each cell must have one of the legal values.
legalValues :: (MonadSAT s m) => ReaderT Env m ()
legalValues = mapM_ legalValue . Array.elems =<< asks envCellArray
  where
    legalValue cell = do
      values <- asks envValues
      assert $ any (cell ===) values

-- | Assert that each cell in a group must have a different value.
allDifferent :: (MonadSAT s m) => [(Word8, Word8)] -> ReaderT Env m ()
allDifferent indices = do
  cellArray <- asks envCellArray
  let pairs =
        [ (cellArray ! a, cellArray ! b)
          | a <- indices,
            b <- indices,
            a /= b
        ]
  forM_ pairs $ \(cellA, cellB) -> assert (cellA /== cellB)

-- | The valid index range for the grid.
range :: (Index, Index)
range = ((0, 0), (8, 8))

subsquares, horizontal, vertical :: [[Index]]

-- | The index group for each subsquare.
subsquares = do
  sqY <- [0 .. 2]
  sqX <- [0 .. 2]
  let top = 3 * sqY
      left = 3 * sqX
  return [(y, x) | y <- [top .. top + 2], x <- [left .. left + 2]]

-- | The index group for each line.
horizontal = do
  line <- [0 .. 8]
  return [(line, x) | x <- [0 .. 8]]

-- | The index group for each column.
vertical = do
  column <- [0 .. 8]
  return [(y, column) | y <- [0 .. 8]]
