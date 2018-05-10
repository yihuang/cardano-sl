{-# LANGUAGE Rank2Types #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Listener
       ( listenerConv
       ) where

import qualified Node as N
<<<<<<< HEAD
=======
import           System.Wlog (WithLogger)
>>>>>>> CHW-82-84, orphan branch
import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ
import           Pos.Binary.Class (Bi)
import           Pos.Binary.Infra ()
import           Pos.Communication.Protocol (ConversationActions, HandlerSpec (..),
                                             ListenerSpec (..), Message, NodeId, OutSpecs,
                                             VerInfo (..), checkProtocolMagic, checkingInSpecs,
                                             messageCode)
import           Pos.Network.Types (Bucket)
<<<<<<< HEAD
import           Pos.Util.Trace (Trace, Severity)
=======
>>>>>>> CHW-82-84, orphan branch

-- TODO automatically provide a 'recvLimited' here by using the
-- 'MessageLimited'?
listenerConv
<<<<<<< HEAD
    :: forall snd rcv pack .
=======
    :: forall snd rcv pack m .
>>>>>>> CHW-82-84, orphan branch
       ( Bi snd
       , Bi rcv
       , Message snd
       , Message rcv
<<<<<<< HEAD
       )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> (VerInfo -> NodeId -> ConversationActions snd rcv -> IO ())
    -> (ListenerSpec, OutSpecs)
listenerConv logTrace oq h = (lspec, mempty)
=======
       , WithLogger m
       , MonadIO m
       )
    => OQ.OutboundQ pack NodeId Bucket
    -> (VerInfo -> NodeId -> ConversationActions snd rcv m -> m ())
    -> (ListenerSpec m, OutSpecs)
listenerConv oq h = (lspec, mempty)
>>>>>>> CHW-82-84, orphan branch
  where
    spec = (rcvMsgCode, ConvHandler sndMsgCode)
    lspec =
      flip ListenerSpec spec $ \ourVerInfo ->
          N.Listener $ \peerVerInfo' nNodeId conv -> checkProtocolMagic ourVerInfo peerVerInfo' $ do
<<<<<<< HEAD
              OQ.clearFailureOf oq nNodeId
              checkingInSpecs logTrace ourVerInfo peerVerInfo' spec nNodeId $
=======
              liftIO $ OQ.clearFailureOf oq nNodeId
              checkingInSpecs ourVerInfo peerVerInfo' spec nNodeId $
>>>>>>> CHW-82-84, orphan branch
                  h ourVerInfo nNodeId conv

    sndProxy :: Proxy snd
    sndProxy = Proxy
    rcvProxy :: Proxy rcv
    rcvProxy = Proxy

    sndMsgCode = messageCode sndProxy
    rcvMsgCode = messageCode rcvProxy
