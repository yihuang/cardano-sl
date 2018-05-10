module Statistics.MemPool
    ( memPoolF
    ) where

import           Control.Foldl (Fold (..))

import           JSONLog (IndexedJLTimedEvent (..))
<<<<<<< HEAD
import           Pos.Util.JsonLog.Events (JLEvent (..), JLMemPool (..))
import           Prelude (id)
import           Types

memPoolF :: Fold IndexedJLTimedEvent [(NodeId, Timestamp, JLMemPool)]
memPoolF = Fold step [] id
  where
    step :: [(NodeId, Timestamp, JLMemPool)]
         -> IndexedJLTimedEvent
         -> [(NodeId, Timestamp, JLMemPool)]
=======
import           Pos.Util.JsonLog (JLEvent (..), JLMemPool (..))
import           Prelude (id)
import           Types

memPoolF :: Fold IndexedJLTimedEvent [(NodeIndex, Timestamp, JLMemPool)]
memPoolF = Fold step [] id
  where
    step :: [(NodeIndex, Timestamp, JLMemPool)]
         -> IndexedJLTimedEvent
         -> [(NodeIndex, Timestamp, JLMemPool)]
>>>>>>> CHW-82-84, orphan branch
    step xs IndexedJLTimedEvent{..} = case ijlEvent of
        JLMemPoolEvent mp -> (ijlNode, ijlTimestamp, mp) : xs
        _                 -> xs

{-
<<<<<<< HEAD
memPoolF :: Fold IndexedJLTimedEvent [(NodeId, Timestamp, Int)]
memPoolF = Fold step [] id
  where
    step :: [(NodeId, Timestamp, Int)] -> IndexedJLTimedEvent -> [(NodeId, Timestamp, Int)]
=======
memPoolF :: Fold IndexedJLTimedEvent [(NodeIndex, Timestamp, Int)]
memPoolF = Fold step [] id
  where
    step :: [(NodeIndex, Timestamp, Int)] -> IndexedJLTimedEvent -> [(NodeIndex, Timestamp, Int)]
>>>>>>> CHW-82-84, orphan branch
    step xs IndexedJLTimedEvent{..} = case ijlEvent of
        JLMemPoolEvent (JLMemPool {..}) -> (ijlNode, ijlTimestamp, jlmSizeAfter) : xs
        _                               -> xs
        -}
