{-# LANGUAGE RankNTypes #-}

-- | Send and receive for delegation.

module Pos.Diffusion.Full.Delegation
       ( delegationListeners
       , delegationOutSpecs
       , sendPskHeavy
       ) where

import           Universum

import           Data.Functor.Contravariant (contramap)
import qualified Network.Broadcast.OutboundQueue as OQ

import           Pos.Binary ()
import           Pos.Communication.Limits (mlHeavyDlgIndex, mlProxySecretKey)
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (MsgType (..), NodeId, EnqueueMsg,
                                             MkListeners, OutSpecs)
import           Pos.Communication.Relay (DataParams (..), Relay (..),
                                          relayListeners, dataFlow,
                                          relayPropagateOut)
import           Pos.Core       (ProxySKHeavy)
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (Bucket)
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem, publicPrivateLogItem)

delegationListeners
    :: Trace IO LogItem
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
delegationListeners logTrace logic oq enqueue = relayListeners
    (contramap publicPrivateLogItem logTrace)
    oq
    enqueue
    (delegationRelays logic)

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: Logic IO
    -> [Relay]
delegationRelays logic = [ pskHeavyRelay logic ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
delegationOutSpecs
    :: Logic IO
    -> OutSpecs
delegationOutSpecs logic = relayPropagateOut (delegationRelays logic)

pskHeavyRelay
    :: Logic IO
    -> Relay
pskHeavyRelay logic = Data $ DataParams
    MsgTransaction
    (\_ _ -> postPskHeavy logic)
    -- The message size limit for ProxySKHeavy: a ProxySecretKey with an
    -- EpochIndex.
    (pure (mlProxySecretKey mlHeavyDlgIndex))

sendPskHeavy
    :: Trace IO LogItem
    -> EnqueueMsg
    -> ProxySKHeavy
    -> IO ()
sendPskHeavy logTrace enqueue = dataFlow
    (contramap publicPrivateLogItem logTrace)
    "pskHeavy"
    enqueue
    (MsgTransaction OQ.OriginSender)
