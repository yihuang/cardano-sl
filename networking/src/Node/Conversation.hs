{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Node.Conversation
    ( Converse (..)
<<<<<<< HEAD
    , Conversation (..)
    , ConversationActions (..)
    , converseWith
=======
    , converseWith
    , hoistConverse
    , Conversation (..)
    , hoistConversation
    , ConversationActions (..)
    , hoistConversationActions
>>>>>>> CHW-82-84, orphan branch
    ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Word (Word32)
import qualified Node.Internal as LL
import           Node.Message.Class

<<<<<<< HEAD
newtype Converse packingType peerData = Converse {
      runConverse :: forall t . LL.NodeId -> (peerData -> Conversation packingType t) -> IO t
    }

converseWith
    :: Converse packingType peerData
    -> LL.NodeId
    -> (peerData -> Conversation packingType t)
    -> IO t
converseWith = runConverse

-- | Use ConversationActions on some Serializable, Message send type, with a
--   Serializable receive type.
data Conversation packingType t where
    Conversation
        :: (Serializable packingType snd, Serializable packingType rcv, Message snd)
        => (ConversationActions snd rcv -> IO t)
        -> Conversation packingType t

data ConversationActions body rcv = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> IO ()
=======
newtype Converse packingType peerData m = Converse {
      runConverse :: forall t . LL.NodeId -> (peerData -> Conversation packingType m t) -> m t
    }

converseWith
    :: Converse packingType peerData m
    -> LL.NodeId
    -> (peerData -> Conversation packingType m t)
    -> m t
converseWith = runConverse

hoistConverse
    :: (forall a . m a -> n a)
    -> (forall a . n a -> m a)
    -> Converse packingType peerData m
    -> Converse packingType peerData n
hoistConverse nat rnat (Converse k) = Converse $ \nodeId l ->
    let l' = \peerData -> hoistConversation rnat nat (l peerData)
    in  nat (k nodeId l')

-- | Use ConversationActions on some Serializable, Message send type, with a
--   Serializable receive type.
data Conversation packingType m t where
    Conversation
        :: (Serializable packingType snd, Serializable packingType rcv, Message snd)
        => (ConversationActions snd rcv m -> m t)
        -> Conversation packingType m t

hoistConversation
    :: (forall a . m a -> n a)
    -> (forall a . n a -> m a)
    -> Conversation packingType m t
    -> Conversation packingType n t
hoistConversation nat rnat (Conversation k) = Conversation k'
  where
    k' cactions = nat (k (hoistConversationActions rnat cactions))

data ConversationActions body rcv m = ConversationActions {
       -- | Send a message within the context of this conversation
       send :: body -> m ()
>>>>>>> CHW-82-84, orphan branch

       -- | Receive a message within the context of this conversation.
       --   'Nothing' means end of input (peer ended conversation).
       --   The 'Word32' parameter is a limit on how many bytes will be read
       --   in by this use of 'recv'. If the limit is exceeded, the
       --   'LimitExceeded' exception is thrown.
<<<<<<< HEAD
     , recv :: Word32 -> IO (Maybe rcv)

       -- | Send raw bytes.
     , sendRaw :: ByteString -> IO ()
     }
=======
     , recv :: Word32 -> m (Maybe rcv)

       -- | Send raw bytes.
     , sendRaw :: ByteString -> m ()
     }

hoistConversationActions
    :: (forall a. n a -> m a)
    -> ConversationActions body rcv n
    -> ConversationActions body rcv m
hoistConversationActions nat ConversationActions {..} =
  ConversationActions send' recv' sendRaw'
      where
        send' = nat . send
        recv' = nat . recv
        sendRaw' = nat . sendRaw
>>>>>>> CHW-82-84, orphan branch
