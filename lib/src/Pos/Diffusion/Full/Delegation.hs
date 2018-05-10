{-# LANGUAGE RankNTypes #-}

-- | Send and receive for delegation.

module Pos.Diffusion.Full.Delegation
       ( delegationListeners
       , delegationOutSpecs
       , sendPskHeavy
       ) where

import           Universum

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
<<<<<<< HEAD
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (Bucket)
import           Pos.Util.Trace (Trace, Severity)

delegationListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
delegationListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (delegationRelays logic)

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: Logic IO
    -> [Relay]
=======
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (Bucket)

delegationListeners
    :: ( DiffusionWorkMode m )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
delegationListeners logic oq enqueue = relayListeners oq enqueue (delegationRelays logic)

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: forall m .
       ( DiffusionWorkMode m )
    => Logic m
    -> [Relay m]
>>>>>>> CHW-82-84, orphan branch
delegationRelays logic = [ pskHeavyRelay logic ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
delegationOutSpecs
<<<<<<< HEAD
    :: Logic IO
=======
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
>>>>>>> CHW-82-84, orphan branch
    -> OutSpecs
delegationOutSpecs logic = relayPropagateOut (delegationRelays logic)

pskHeavyRelay
<<<<<<< HEAD
    :: Logic IO
    -> Relay
=======
    :: ( DiffusionWorkMode m )
    => Logic m
    -> Relay m
>>>>>>> CHW-82-84, orphan branch
pskHeavyRelay logic = Data $ DataParams
    MsgTransaction
    (\_ _ -> postPskHeavy logic)
    -- The message size limit for ProxySKHeavy: a ProxySecretKey with an
    -- EpochIndex.
    (pure (mlProxySecretKey mlHeavyDlgIndex))

sendPskHeavy
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> EnqueueMsg
    -> ProxySKHeavy
    -> IO ()
sendPskHeavy logTrace enqueue = dataFlow logTrace "pskHeavy" enqueue (MsgTransaction OQ.OriginSender)
=======
    :: forall m .
       ( DiffusionWorkMode m )
    => EnqueueMsg m
    -> ProxySKHeavy
    -> m ()
sendPskHeavy enqueue = dataFlow "pskHeavy" enqueue (MsgTransaction OQ.OriginSender)
>>>>>>> CHW-82-84, orphan branch
