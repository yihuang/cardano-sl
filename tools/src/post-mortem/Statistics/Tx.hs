module Statistics.Tx
    ( txReceivedF
    , txFirstReceivedF
    ) where

import           Control.Foldl (Fold (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           JSONLog
<<<<<<< HEAD
import           Pos.Util.JsonLog.Events (JLEvent (..), JLTxR (..))
=======
import           Pos.Util.JsonLog (JLEvent (..), JLTxR (..))
>>>>>>> CHW-82-84, orphan branch
import           Prelude (head, id)
import           Types
import           Universum hiding (head)

<<<<<<< HEAD
txReceivedF :: Fold IndexedJLTimedEvent (Map TxHash [(Timestamp, NodeId)])
txReceivedF = M.map reverse <$> Fold step M.empty id
  where
    step :: Map TxHash [(Timestamp, NodeId)] -> IndexedJLTimedEvent -> Map TxHash [(Timestamp, NodeId)]
=======
txReceivedF :: Fold IndexedJLTimedEvent (Map TxHash [(Timestamp, NodeIndex)])
txReceivedF = M.map reverse <$> Fold step M.empty id
  where
    step :: Map TxHash [(Timestamp, NodeIndex)] -> IndexedJLTimedEvent -> Map TxHash [(Timestamp, NodeIndex)]
>>>>>>> CHW-82-84, orphan branch
    step m IndexedJLTimedEvent{..} = case ijlEvent of
        JLTxReceived JLTxR{..} -> M.insertWith (++) jlrTxId [(ijlTimestamp, ijlNode)] m
        _                      -> m

txFirstReceivedF :: Fold IndexedJLTimedEvent (Map TxHash Timestamp)
txFirstReceivedF = M.map (fst . head) <$> txReceivedF
