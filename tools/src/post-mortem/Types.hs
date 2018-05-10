module Types
    ( TxHash
    , BlockHash
    , Timestamp
    , Slot
<<<<<<< HEAD
    , NodeId
=======
    , NodeIndex
>>>>>>> CHW-82-84, orphan branch
    ) where

import           Data.Time.Units (Microsecond)
import           Universum

type TxHash = Text
type BlockHash = Text
type Timestamp = Microsecond
type Slot = (Word64, Word16)
<<<<<<< HEAD
type NodeId = Text
=======
type NodeIndex = Int
>>>>>>> CHW-82-84, orphan branch
