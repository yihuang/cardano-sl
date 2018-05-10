{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Txp
       ( sendTx
       , txListeners
       , txOutSpecs
       ) where

import           Universum
import           Data.Tagged (Tagged)
import qualified Network.Broadcast.OutboundQueue as OQ

import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Txp ()
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (mlTxMsgContents)
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..), NodeId,
                                             MkListeners, OutSpecs)
<<<<<<< HEAD
import           Pos.Communication.Relay (invReqDataFlowTK, resOk,
=======
import           Pos.Communication.Relay (invReqDataFlowTK, resOk, MinRelayWorkMode,
>>>>>>> CHW-82-84, orphan branch
                                          InvReqDataParams (..), invReqMsgType, Relay (..),
                                          relayListeners, MempoolParams (..),
                                          relayPropagateOut)
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Crypto (hash)
<<<<<<< HEAD
=======
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Network.Types (Bucket)
import           Pos.Txp.Network.Types (TxMsgContents (..))
<<<<<<< HEAD
import           Pos.Util.Trace (Trace, Severity)

-- | Send Tx to given addresses.
-- Returns 'True' if any peer accepted and applied this transaction.
sendTx :: Trace IO (Severity, Text) -> EnqueueMsg -> TxAux -> IO Bool
sendTx logTrace enqueue txAux = do
    anySucceeded <$> invReqDataFlowTK
        logTrace
=======

-- | Send Tx to given addresses.
-- Returns 'True' if any peer accepted and applied this transaction.
sendTx
    :: ( MinRelayWorkMode m
       )
    => EnqueueMsg m
    -> TxAux
    -> m Bool
sendTx enqueue txAux = do
    anySucceeded <$> invReqDataFlowTK
>>>>>>> CHW-82-84, orphan branch
        "tx"
        enqueue
        (MsgTransaction OriginSender)
        (hash $ taTx txAux)
        (TxMsgContents txAux)
  where
    anySucceeded outcome =
        not $ null
        [ ()
        | Right (Just peerResponse) <- toList outcome
        , resOk peerResponse
        ]

txListeners
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
txListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (txRelays logic)
=======
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
txListeners logic oq enqueue = relayListeners oq enqueue (txRelays logic)
>>>>>>> CHW-82-84, orphan branch

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
<<<<<<< HEAD
txOutSpecs :: Logic IO -> OutSpecs
txOutSpecs logic = relayPropagateOut (txRelays logic)

txInvReqDataParams
    :: Logic IO
    -> InvReqDataParams (Tagged TxMsgContents TxId) TxMsgContents
=======
txOutSpecs
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OutSpecs
txOutSpecs logic = relayPropagateOut (txRelays logic)

txInvReqDataParams
    :: DiffusionWorkMode m
    => Logic m
    -> InvReqDataParams (Tagged TxMsgContents TxId) TxMsgContents m
>>>>>>> CHW-82-84, orphan branch
txInvReqDataParams logic =
    InvReqDataParams
       { invReqMsgType = MsgTransaction
       , contentsToKey = KV.toKey (postTx logic)
       , handleInv = \_ -> KV.handleInv (postTx logic)
       , handleReq = \_ -> KV.handleReq (postTx logic)
       , handleData = \_ -> KV.handleData (postTx logic)
       , irdpMkLimit = mlTxMsgContents <$> getAdoptedBVData logic
       }

<<<<<<< HEAD
txRelays :: Logic IO -> [Relay]
=======
txRelays
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> [Relay m]
>>>>>>> CHW-82-84, orphan branch
txRelays logic = pure $
    -- Previous implementation had KeyMempool, but mempool messages are never
    -- used so we drop it.
    InvReqData NoMempool (txInvReqDataParams logic)
