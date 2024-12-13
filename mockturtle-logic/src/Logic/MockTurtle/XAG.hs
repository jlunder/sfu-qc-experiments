module Logic.MockTurtle.XAG where

import GHC.Generics (Generic)

data Graph = Graph {nodes :: [Node], inputIDs :: [Int], outputIDs :: [Int]}
  deriving (Eq, Generic, Ord, Read, Show)

data Node
  = Const {nodeID :: !Int, value :: !Bool}
  | Not {nodeID :: !Int, xIn :: !Int}
  | Xor {nodeID :: !Int, xIn :: !Int, yIn :: !Int}
  | And {nodeID :: !Int, xIn :: !Int, yIn :: !Int}
  deriving (Eq, Generic, Read, Show)

instance Ord Node where
  compare :: Node -> Node -> Ordering
  compare x y
    | nidOrd == EQ = compareType x y
    | otherwise = nidOrd
    where
      nidOrd = compare (nodeID x) (nodeID y)
      -- Same-type: drill down
      compareType (Const _ xVal) (Const _ yVal) = compare xVal yVal
      compareType (Not _ xXIn) (Not _ yXIn) = compare xXIn yXIn
      compareType (Xor _ xXIn xYIn) (Xor _ yXIn yYIn) = compare xXIn yXIn <> compare xYIn yYIn
      compareType (And _ xXIn xYIn) (And _ yXIn yYIn) = compare xXIn yXIn <> compare xYIn yYIn
      -- Different-type: early out
      compareType (Const {}) _ = LT
      compareType (Not {}) _ = LT
      compareType (Xor {}) _ = LT
      compareType (And {}) _ = undefined
