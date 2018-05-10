module Statistics.Focus
    ( Focus (..)
    , focusF
    ) where

import           Control.Foldl (Fold (..))
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import           JSONLog
<<<<<<< HEAD
import           Pos.Util.JsonLog.Events (JLBlock (..), JLEvent (..), JLTxR (..))
=======
import           Pos.Util.JsonLog (JLBlock (..), JLEvent (..), JLTxR (..))
>>>>>>> CHW-82-84, orphan branch
import           Prelude (id)
import           Types
import           Universum

data Focus =
      Received !(Maybe Text)
    | InCreatedBlock !BlockHash
    | InAdoptedBlock !BlockHash
    deriving Show

<<<<<<< HEAD
focusF :: TxHash -> Fold IndexedJLTimedEvent [(Timestamp, NodeId, Focus)]
focusF tx = f <$> allF <*> blocksF
  where
    f :: [(Timestamp, NodeId, Focus)] -> Set BlockHash -> [(Timestamp, NodeId, Focus)]
    f xs s = filter g xs
      where
        g :: (Timestamp, NodeId, Focus) -> Bool
=======
focusF :: TxHash -> Fold IndexedJLTimedEvent [(Timestamp, NodeIndex, Focus)]
focusF tx = f <$> allF <*> blocksF
  where
    f :: [(Timestamp, NodeIndex, Focus)] -> Set BlockHash -> [(Timestamp, NodeIndex, Focus)]
    f xs s = filter g xs
      where
        g :: (Timestamp, NodeIndex, Focus) -> Bool
>>>>>>> CHW-82-84, orphan branch
        g (_, _, x) = case x of
            (Received _)       -> True
            (InCreatedBlock h) -> S.member h s
            (InAdoptedBlock h) -> S.member h s

<<<<<<< HEAD
    allF :: Fold IndexedJLTimedEvent [(Timestamp, NodeId, Focus)]
    allF = reverse <$> Fold step [] id
      where
        step :: [(Timestamp, NodeId, Focus)] -> IndexedJLTimedEvent -> [(Timestamp, NodeId, Focus)]
=======
    allF :: Fold IndexedJLTimedEvent [(Timestamp, NodeIndex, Focus)]
    allF = reverse <$> Fold step [] id
      where
        step :: [(Timestamp, NodeIndex, Focus)] -> IndexedJLTimedEvent -> [(Timestamp, NodeIndex, Focus)]
>>>>>>> CHW-82-84, orphan branch
        step xs IndexedJLTimedEvent{..} = case ijlEvent of
            (JLCreatedBlock JLBlock{..}) -> (ijlTimestamp, ijlNode, InCreatedBlock jlHash) : xs
            (JLAdoptedBlock h)           -> (ijlTimestamp, ijlNode, InAdoptedBlock h)      : xs
            (JLTxReceived JLTxR{..})     -> if jlrTxId == tx
                                                then (ijlTimestamp, ijlNode, Received jlrError) : xs
                                                else xs
            _                            -> xs

    blocksF :: Fold IndexedJLTimedEvent (Set BlockHash)
    blocksF = Fold step S.empty id
      where
        step :: Set BlockHash -> IndexedJLTimedEvent -> Set BlockHash
        step s IndexedJLTimedEvent{..} = case ijlEvent of
<<<<<<< HEAD
            (JLCreatedBlock JLBlock{..}) -> if tx `elem` [T.take 16 tx' | tx' <- jlTxs]
=======
            (JLCreatedBlock JLBlock{..}) -> if tx `elem` [T.take 8 tx' | tx' <- jlTxs]
>>>>>>> CHW-82-84, orphan branch
                                                then S.insert jlHash s
                                                else s
            _                            ->  s
