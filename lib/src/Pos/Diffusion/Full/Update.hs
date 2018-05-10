{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Update
       ( sendVote
       , sendUpdateProposal

       , updateListeners
       , updateOutSpecs
       ) where

import           Universum
<<<<<<< HEAD

import qualified Network.Broadcast.OutboundQueue as OQ
=======
import           Formatting (sformat, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           System.Wlog (logInfo)
>>>>>>> CHW-82-84, orphan branch

import           Pos.Core.Update (UpId, UpdateVote, UpdateProposal, mkVoteId)
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (mlUpdateVote, mlUpdateProposalAndVotes)
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..),
                                             NodeId, MkListeners, OutSpecs)
<<<<<<< HEAD
import           Pos.Communication.Relay (invReqDataFlowTK,
                                          Relay (..), relayListeners,
                                          InvReqDataParams (..), MempoolParams (..),
                                          relayPropagateOut)
=======
import           Pos.Communication.Relay (invReqDataFlowTK, MinRelayWorkMode,
                                          Relay (..), relayListeners,
                                          InvReqDataParams (..), MempoolParams (..),
                                          relayPropagateOut)
import           Pos.Crypto (hashHexF)
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Network.Types (Bucket)
import           Pos.Update ()
<<<<<<< HEAD
import           Pos.Util.Trace (Trace, Severity)

-- Send UpdateVote to given addresses.
sendVote
    :: Trace IO (Severity, Text)
    -> EnqueueMsg
    -> UpdateVote
    -> IO ()
sendVote logTrace enqueue vote =
    void $ invReqDataFlowTK
        logTrace
=======

-- Send UpdateVote to given addresses.
sendVote
    :: ( MinRelayWorkMode m
       )
    => EnqueueMsg m
    -> UpdateVote
    -> m ()
sendVote enqueue vote =
    void $ invReqDataFlowTK
>>>>>>> CHW-82-84, orphan branch
        "UpdateVote"
        enqueue
        (MsgMPC OriginSender)
        (mkVoteId vote)
        vote

-- Send UpdateProposal to given address.
sendUpdateProposal
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> EnqueueMsg
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> IO ()
sendUpdateProposal logTrace enqueue upid proposal votes = do
    void $ invReqDataFlowTK
        logTrace
=======
    :: ( MinRelayWorkMode m
       )
    => EnqueueMsg m
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal enqueue upid proposal votes = do
    logInfo $ sformat ("Announcing proposal with id "%hashHexF) upid
    void $ invReqDataFlowTK
>>>>>>> CHW-82-84, orphan branch
        "UpdateProposal"
        enqueue
        (MsgMPC OriginSender)
        upid
        (proposal, votes)

updateListeners
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
updateListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (usRelays logic)

-- | Relays for data related to update system
usRelays :: Logic IO -> [Relay]
=======
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
updateListeners logic oq enqueue = relayListeners oq enqueue (usRelays logic)

-- | Relays for data related to update system
usRelays
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> [Relay m]
>>>>>>> CHW-82-84, orphan branch
usRelays logic = [proposalRelay logic, voteRelay logic]

-- | 'OutSpecs' for the update system, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
<<<<<<< HEAD
updateOutSpecs :: Logic IO -> OutSpecs
=======
updateOutSpecs
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OutSpecs
>>>>>>> CHW-82-84, orphan branch
updateOutSpecs logic = relayPropagateOut (usRelays logic)

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

<<<<<<< HEAD
proposalRelay :: Logic IO -> Relay
=======
proposalRelay
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> Relay m
>>>>>>> CHW-82-84, orphan branch
proposalRelay logic =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = KV.toKey kv
           , handleInv = \_ -> KV.handleInv kv
           , handleReq = \_ -> KV.handleReq kv
           , handleData = \_ -> KV.handleData kv
           , irdpMkLimit = mlUpdateProposalAndVotes <$> getAdoptedBVData logic
           }
  where
    kv = postUpdate logic

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

<<<<<<< HEAD
voteRelay :: Logic IO -> Relay
=======
voteRelay
    :: ( DiffusionWorkMode m )
    => Logic m
    -> Relay m
>>>>>>> CHW-82-84, orphan branch
voteRelay logic =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = KV.toKey kv
           , handleInv = \_ -> KV.handleInv kv
           , handleReq = \_ -> KV.handleReq kv
           , handleData = \_ -> KV.handleData kv
           , irdpMkLimit = pure mlUpdateVote
           }
  where
    kv = postVote logic
