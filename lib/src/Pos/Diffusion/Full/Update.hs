{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Update
       ( sendVote
       , sendUpdateProposal

       , updateListeners
       , updateOutSpecs
       ) where

import           Universum

import           Data.Functor.Contravariant (contramap)
import qualified Network.Broadcast.OutboundQueue as OQ

import           Pos.Core.Update (UpId, UpdateVote, UpdateProposal, mkVoteId)
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (mlUpdateVote, mlUpdateProposalAndVotes)
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..),
                                             NodeId, MkListeners, OutSpecs)
import           Pos.Communication.Relay (invReqDataFlowTK,
                                          Relay (..), relayListeners,
                                          InvReqDataParams (..), MempoolParams (..),
                                          relayPropagateOut)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Network.Types (Bucket)
import           Pos.Update ()
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem, publicPrivateLogItem)

-- Send UpdateVote to given addresses.
sendVote
    :: Trace IO LogItem
    -> EnqueueMsg
    -> UpdateVote
    -> IO ()
sendVote logTrace enqueue vote =
    void $ invReqDataFlowTK
        (contramap publicPrivateLogItem logTrace)
        "UpdateVote"
        enqueue
        (MsgMPC OriginSender)
        (mkVoteId vote)
        vote

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: Trace IO LogItem
    -> EnqueueMsg
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> IO ()
sendUpdateProposal logTrace enqueue upid proposal votes = do
    void $ invReqDataFlowTK
        (contramap publicPrivateLogItem logTrace)
        "UpdateProposal"
        enqueue
        (MsgMPC OriginSender)
        upid
        (proposal, votes)

updateListeners
    :: Trace IO LogItem
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
updateListeners logTrace logic oq enqueue = relayListeners
    (contramap publicPrivateLogItem logTrace)
    oq
    enqueue
    (usRelays logic)

-- | Relays for data related to update system
usRelays :: Logic IO -> [Relay]
usRelays logic = [proposalRelay logic, voteRelay logic]

-- | 'OutSpecs' for the update system, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
updateOutSpecs :: Logic IO -> OutSpecs
updateOutSpecs logic = relayPropagateOut (usRelays logic)

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

proposalRelay :: Logic IO -> Relay
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

voteRelay :: Logic IO -> Relay
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
