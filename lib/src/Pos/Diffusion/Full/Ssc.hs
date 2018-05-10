{-# LANGUAGE RankNTypes #-}

module Pos.Diffusion.Full.Ssc
    ( sscListeners
    , sscOutSpecs
      -- TODO move the conversation starters here too (they're defined inline
      -- in Pos.Diffusion.Full).
    ) where

import           Universum

import           Data.Tagged (Tagged (..))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Message.Class (Message)

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Crypto ()
import           Pos.Binary.Infra ()
import           Pos.Binary.Ssc ()
-- Message instances for various types.
-- TODO should move these into the Diffusion module subtree.
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (Limit, mlMCOpening, mlMCVssCertificate,
                                           mlMCCommitment, mlMCShares)
import           Pos.Communication.Relay (DataMsg, InvOrData, InvReqDataParams (..),
                                          MempoolParams (NoMempool), Relay (..), ReqMsg, ReqOrRes,
                                          relayListeners, relayPropagateOut)
import           Pos.Communication.Types.Protocol (MsgType (..), NodeId, EnqueueMsg,
                                                   MkListeners, OutSpecs)
import           Pos.Core (StakeholderId)
<<<<<<< HEAD
=======
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Network.Types (Bucket)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..), SscMessageConstraints)
<<<<<<< HEAD
import           Pos.Util.Trace (Trace, Severity)

sscListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
sscListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (sscRelays logic)

sscRelays
    :: ( SscMessageConstraints )
    => Logic IO
    -> [Relay]
=======

sscListeners
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
sscListeners logic oq enqueue = relayListeners oq enqueue (sscRelays logic)

sscRelays
    :: ( DiffusionWorkMode m
       , SscMessageConstraints
       )
    => Logic m
    -> [Relay m]
>>>>>>> CHW-82-84, orphan branch
sscRelays logic =
    [ commitmentRelay logic (postSscCommitment logic)
    , openingRelay (postSscOpening logic)
    , sharesRelay logic (postSscShares logic)
    , vssCertRelay (postSscVssCert logic)
    ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
<<<<<<< HEAD
sscOutSpecs :: Logic IO -> OutSpecs
sscOutSpecs logic = relayPropagateOut (sscRelays logic)

commitmentRelay
    :: ( SscMessageConstraints )
    => Logic IO
    -> KV.KeyVal (Tagged MCCommitment StakeholderId) MCCommitment IO
    -> Relay
commitmentRelay logic kv = sscRelay kv (mlMCCommitment <$> getAdoptedBVData logic)

openingRelay
    :: ( SscMessageConstraints )
    => KV.KeyVal (Tagged MCOpening StakeholderId) MCOpening IO
    -> Relay
=======
sscOutSpecs
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OutSpecs
sscOutSpecs logic = relayPropagateOut (sscRelays logic)

commitmentRelay
    :: ( SscMessageConstraints
       , DiffusionWorkMode m
       )
    => Logic m
    -> KV.KeyVal (Tagged MCCommitment StakeholderId) MCCommitment m
    -> Relay m
commitmentRelay logic kv = sscRelay kv (mlMCCommitment <$> getAdoptedBVData logic)

openingRelay
    :: ( SscMessageConstraints
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged MCOpening StakeholderId) MCOpening m
    -> Relay m
>>>>>>> CHW-82-84, orphan branch
openingRelay kv = sscRelay kv (pure mlMCOpening)

sharesRelay
    :: ( SscMessageConstraints
<<<<<<< HEAD
       )
    => Logic IO
    -> KV.KeyVal (Tagged MCShares StakeholderId) MCShares IO
    -> Relay
sharesRelay logic kv = sscRelay kv (mlMCShares <$> getAdoptedBVData logic)

vssCertRelay
    :: ( SscMessageConstraints )
    => KV.KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate IO
    -> Relay
=======
       , DiffusionWorkMode m
       )
    => Logic m
    -> KV.KeyVal (Tagged MCShares StakeholderId) MCShares m
    -> Relay m
sharesRelay logic kv = sscRelay kv (mlMCShares <$> getAdoptedBVData logic)

vssCertRelay
    :: ( SscMessageConstraints
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate m
    -> Relay m
>>>>>>> CHW-82-84, orphan branch
vssCertRelay kv = sscRelay kv (pure mlMCVssCertificate)

sscRelay
    :: ( Buildable contents
       , Typeable contents
       , Bi (DataMsg contents)
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqOrRes (Tagged contents StakeholderId))
       , Message (ReqMsg (Tagged contents StakeholderId))
<<<<<<< HEAD
       )
    => KV.KeyVal (Tagged contents StakeholderId) contents IO
    -> IO (Limit contents)
    -> Relay
=======
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged contents StakeholderId) contents m
    -> m (Limit contents)
    -> Relay m
>>>>>>> CHW-82-84, orphan branch
sscRelay kv mkLimit =
    InvReqData NoMempool $
        InvReqDataParams
          { invReqMsgType = MsgMPC
          , contentsToKey = KV.toKey kv
          , handleInv = \_ -> KV.handleInv kv
          , handleReq = \_ -> KV.handleReq kv
          , handleData = \_ -> KV.handleData kv
          , irdpMkLimit = mkLimit
          }
