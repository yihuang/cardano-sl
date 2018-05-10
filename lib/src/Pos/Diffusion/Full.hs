{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Pos.Diffusion.Full
<<<<<<< HEAD
    ( FullDiffusionConfiguration (..)
    , diffusionLayerFull
    , diffusionLayerFullExposeInternals
    , FullDiffusionInternals (..)
    , RunFullDiffusionInternals (..)
    ) where

import           Universum
import           Nub (ordNub)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Async as Async
import           Data.Functor.Contravariant (contramap)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import           Data.Time.Units (Microsecond, Millisecond, Second)
import           Formatting (Format)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (MsgType (..), Origin (..))
import           Network.Transport (Transport)
import           Node (Node, NodeAction (..), simpleNodeEndPoint, NodeEnvironment (..), defaultNodeEnvironment, node)
import           Node.Conversation (Converse, converseWith, Conversation)
import           System.Random (newStdGen)
=======
    ( diffusionLayerFull
    ) where

import           Nub (ordNub)
import           Universum

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import           Control.Exception (Exception, throwIO)
import           Control.Monad.Fix (MonadFix)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import           Data.Time.Units (Microsecond, Millisecond, Second, convertUnit)
import           Formatting (Format)
import           Mockable (withAsync, link)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (MsgType (..), Origin (..))
import           Network.Transport.Abstract (Transport)
import           Node (Node, NodeAction (..), simpleNodeEndPoint, NodeEnvironment (..), defaultNodeEnvironment, node)
import           Node.Conversation (Converse, converseWith, Conversation)
import           System.Random (newStdGen)
import           System.Wlog (WithLogger, CanLog, usingLoggerName)
>>>>>>> CHW-82-84, orphan branch

import           Pos.Block.Network (MsgGetHeaders, MsgHeaders, MsgGetBlocks, MsgBlock)
import           Pos.Communication (NodeId, VerInfo (..), PeerData, PackingType,
                                    EnqueueMsg, makeEnqueueMsg, bipPacking, Listener,
                                    MkListeners (..), HandlerSpecs, InSpecs (..),
                                    OutSpecs (..), createOutSpecs, toOutSpecs, convH,
                                    InvOrDataTK, MsgSubscribe, MsgSubscribe1,
                                    makeSendActions, SendActions, Msg)
import           Pos.Communication.Relay.Logic (invReqDataFlowTK)
<<<<<<< HEAD
=======
import           Pos.Communication.Util (wrapListener)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Core (BlockVersionData (..), BlockVersion, HeaderHash, ProxySKHeavy,
                           StakeholderId, ProtocolConstants (..))
import           Pos.Core.Block (Block, BlockHeader, MainBlockHeader)
import           Pos.Core.Ssc (Opening, InnerSharesMap, SignedCommitment, VssCertificate)
import           Pos.Core.Txp (TxAux)
import           Pos.Core.Update (UpId, UpdateProposal, UpdateVote)
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.DHT.Real (KademliaDHTInstance (..), KademliaParams (..),
                               startDHTInstance, stopDHTInstance,
                               kademliaJoinNetworkNoThrow, kademliaJoinNetworkRetry)
import qualified Pos.Diffusion.Full.Block as Diffusion.Block
import qualified Pos.Diffusion.Full.Delegation as Diffusion.Delegation
import qualified Pos.Diffusion.Full.Ssc as Diffusion.Ssc
import qualified Pos.Diffusion.Full.Txp as Diffusion.Txp
<<<<<<< HEAD
=======
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
>>>>>>> CHW-82-84, orphan branch
import qualified Pos.Diffusion.Full.Update as Diffusion.Update
import           Pos.Diffusion.Subscription.Common (subscriptionListeners)
import           Pos.Diffusion.Subscription.Dht (dhtSubscriptionWorker)
import           Pos.Diffusion.Subscription.Dns (dnsSubscriptionWorker)
<<<<<<< HEAD
import           Pos.Diffusion.Transport.TCP (bracketTransportTCP)
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..), SubscriptionStatus)
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (NetworkConfig (..), Bucket (..), initQueue,
                                    topologySubscribers, SubscriptionWorker (..),
                                    NodeType,  topologySubscriptionWorker,
                                    topologyRunKademlia, topologyHealthStatus)
=======
import           Pos.Diffusion.Types (Diffusion (..) , DiffusionLayer (..), SubscriptionStatus)
                                      
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (NetworkConfig (..), Topology (..), Bucket (..), initQueue,
                                    topologySubscribers, SubscriptionWorker (..),
                                    topologySubscriptionWorker, topologyRunKademlia,
                                    topologyHealthStatus)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Reporting.Health.Types (HealthStatus (..))
import           Pos.Reporting.Ekg (EkgNodeMetrics (..), registerEkgNodeMetrics)
import           Pos.Ssc.Message (MCOpening (..), MCShares (..), MCCommitment (..), MCVssCertificate (..))
import           Pos.Util.Chrono (OldestFirst)
import           Pos.Util.OutboundQueue (EnqueuedConversation (..))
import           Pos.Util.Timer (Timer, newTimer)
<<<<<<< HEAD
import           Pos.Util.Trace (Trace, Severity (Error))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
{-# ANN module ("HLint: ignore Use whenJust" :: Text) #-}
{-# ANN module ("HLint: ignore Use record patterns" :: Text) #-}

data FullDiffusionConfiguration = FullDiffusionConfiguration
    { fdcProtocolMagic          :: !ProtocolMagic
    , fdcProtocolConstants      :: !ProtocolConstants
    , fdcRecoveryHeadersMessage :: !Word
    , fdcLastKnownBlockVersion  :: !BlockVersion
    , fdcConvEstablishTimeout   :: !Microsecond
    , fdcTrace                  :: !(Trace IO (Severity, Text))
    }

data RunFullDiffusionInternals = RunFullDiffusionInternals
    { runFullDiffusionInternals :: forall y . (FullDiffusionInternals -> IO y) -> IO y
    }

data FullDiffusionInternals = FullDiffusionInternals
    { fdiNode :: Node
    , fdiConverse :: Converse PackingType PeerData
    }

-- | Make a full diffusion layer, filling in many details using a
-- 'NetworkConfig' and its constituent 'Topology'.
-- An 'OutboundQ' is brought up for you, based on the 'NetworkConfig'.
-- A TCP transport is brought up as well, again using the 'NetworkConfig',
-- which includes information about the address. This is why we use CPS here:
-- the transport is bracketed.
-- The 'NetworkConfig's topology is also used to fill in various options
-- related to subscription, health status reporting, etc.
diffusionLayerFull
    :: FullDiffusionConfiguration
    -> NetworkConfig KademliaParams
    -> Maybe EkgNodeMetrics
    -> Logic IO
    -> (DiffusionLayer IO -> IO x)
    -> IO x
diffusionLayerFull fdconf networkConfig mEkgNodeMetrics logic k = do
    -- Make the outbound queue using network policies.
    oq :: OQ.OutboundQ EnqueuedConversation NodeId Bucket <-
        -- NB: <> it's not Text semigroup append, it's LoggerName append, which
        -- puts a "." in the middle.
        initQueue networkConfig ("diffusion" <> "outboundqueue") (enmStore <$> mEkgNodeMetrics)
    let topology = ncTopology networkConfig
        mSubscriptionWorker = topologySubscriptionWorker topology
        mSubscribers = topologySubscribers topology
        healthStatus = topologyHealthStatus topology oq
        mKademliaParams = topologyRunKademlia topology
        -- Transport needs a Trace IO Text. We re-use the 'Trace' given in
        -- the configuration at severity 'Error' (when transport has an
        -- exception trying to 'accept' a new connection).
        logTrace :: Trace IO Text
        logTrace = contramap ((,) Error) (fdcTrace fdconf)
    bracketTransportTCP logTrace (fdcConvEstablishTimeout fdconf) (ncTcpAddr networkConfig) $ \transport -> do
        (fullDiffusion, internals) <-
            diffusionLayerFullExposeInternals fdconf
                                              transport
                                              oq
                                              (ncDefaultPort networkConfig)
                                              mSubscriptionWorker
                                              mSubscribers
                                              mKademliaParams
                                              healthStatus
                                              mEkgNodeMetrics
                                              logic
        k $ DiffusionLayer
            { diffusion = fullDiffusion
            , runDiffusionLayer = \action -> runFullDiffusionInternals internals (const action)
            }

diffusionLayerFullExposeInternals
    :: FullDiffusionConfiguration
    -> Transport
    -> OQ.OutboundQ EnqueuedConversation NodeId Bucket
    -> Word16 -- ^ Port on which peers are assumed to listen.
    -> Maybe SubscriptionWorker
    -> Maybe (NodeType, OQ.MaxBucketSize)
    -> Maybe (KademliaParams, Bool)
       -- ^ KademliaParams and a default port for kademlia.
       -- Bool says whether the node must join before starting normal
       -- operation, as opposed to passively trying to join.
    -> IO HealthStatus
       -- ^ Amazon Route53 health check support (stopgap measure, see note
       --   in Pos.Diffusion.Types, above 'healthStatus' record field).
    -> Maybe EkgNodeMetrics
    -> Logic IO
    -> IO (Diffusion IO, RunFullDiffusionInternals)
diffusionLayerFullExposeInternals fdconf
                                  transport
                                  oq
                                  defaultPort
                                  mSubscriptionWorker
                                  mSubscribers
                                  mKademliaParams
                                  healthStatus -- named to be picked up by record wildcard
                                  mEkgNodeMetrics
                                  logic = do

    let protocolMagic = fdcProtocolMagic fdconf
        protocolConstants = fdcProtocolConstants fdconf
        lastKnownBlockVersion = fdcLastKnownBlockVersion fdconf
        recoveryHeadersMessage = fdcRecoveryHeadersMessage fdconf
        logTrace = fdcTrace fdconf

    -- Subscription status.
    subscriptionStatus <- newTVarIO MS.empty

    keepaliveTimer <- newTimer

    let -- VerInfo is a diffusion-layer-specific thing. It's only used for
        -- negotiating with peers.
        --
        -- Known bug: if the block version changes, the VerInfo will be
        -- out of date, as it's immutable.
        -- Solution: don't put it in the VerInfo. Other clients don't need
        -- to know the peer's latest adopted block version, they need only
        -- know what software version its running.
        ourVerInfo :: VerInfo
        ourVerInfo = VerInfo (getProtocolMagic protocolMagic)
                             lastKnownBlockVersion
                             ins
                             (outs <> workerOuts)

        ins :: HandlerSpecs
        InSpecs ins = inSpecs mkL

        -- The out specs come not just from listeners but also from workers.
        -- Workers in the existing implementation were bundled up in
        --   allWorkers :: ([WorkerSpec m], OutSpecs)
        -- and they performed logic layer tasks, so having out specs defined
        -- by them doesn't make sense.
        -- For the first iteration, we just dump those out specs here, since
        -- we know in the diffusion layer the set of all requests that might
        -- be made.
        --
        -- Find below a definition of each of the worker out specs,
        -- copied from Pos.Worker (allWorkers). Each one was manually
        -- inspected to determine the out specs.
        --
        -- FIXME this system must change. Perhaps replace it with a
        -- version number?
        outs :: HandlerSpecs
        OutSpecs outs = outSpecs mkL

        workerOuts :: HandlerSpecs
        OutSpecs workerOuts = mconcat
            [ -- First: the relay system out specs.
              Diffusion.Txp.txOutSpecs logic
            , Diffusion.Update.updateOutSpecs logic
            , Diffusion.Delegation.delegationOutSpecs logic
            , Diffusion.Ssc.sscOutSpecs logic
              -- Relay system for blocks is ad-hoc.
            , blockWorkerOutSpecs
              -- SSC has non-relay out specs, defined below.
            , sscWorkerOutSpecs
            , securityWorkerOutSpecs
            , slottingWorkerOutSpecs
            , subscriptionWorkerOutSpecs
            , dhtWorkerOutSpecs
            ]

        -- An onNewSlotWorker and a localWorker. Latter is mempty. Former
        -- actually does the ssc stuff.
        sscWorkerOutSpecs = mconcat
            [ createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCCommitment))
            , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCOpening))
            , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCShares))
            , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCVssCertificate))
            ]

        -- A single worker checkForReceivedBlocksWorker with
        -- requestTipOuts from Pos.Block.Network.
        securityWorkerOutSpecs = toOutSpecs
            [ convH (Proxy :: Proxy MsgGetHeaders)
                    (Proxy :: Proxy MsgHeaders)
            ]

        -- announceBlockHeaderOuts from blkCreatorWorker
        -- announceBlockHeaderOuts from blkMetricCheckerWorker
        -- along with the retrieval worker outs which also include
        -- announceBlockHeaderOuts.
        blockWorkerOutSpecs = mconcat
            [ announceBlockHeaderOuts
            , announceBlockHeaderOuts
            , announceBlockHeaderOuts <> toOutSpecs [ convH (Proxy :: Proxy MsgGetBlocks)
                                                            (Proxy :: Proxy MsgBlock)
                                                    ]
            ]

        announceBlockHeaderOuts = toOutSpecs [ convH (Proxy :: Proxy MsgHeaders)
                                                     (Proxy :: Proxy MsgGetHeaders)
                                             ]

        -- Plainly mempty from the definition of allWorkers.
        slottingWorkerOutSpecs = mempty

        -- Copied from existing implementation but
        -- FIXME it will be wrong when the patch to include a keepalive
        -- is merged. That shall be the first test of this inspec/outspec
        -- system I suppose.
        subscriptionWorkerOutSpecs = case mSubscriptionWorker of
            Just (SubscriptionWorkerBehindNAT _)     -> specs
            Just (SubscriptionWorkerKademlia  _ _ _) -> specs
            _                                        -> mempty
          where
            specs = toOutSpecs
                [ convH (Proxy @MsgSubscribe)  (Proxy @Void)
                , convH (Proxy @MsgSubscribe1) (Proxy @Void)
                ]

        -- It's a localOnNewSlotWorker, so mempty.
        dhtWorkerOutSpecs = mempty

        mkL :: MkListeners
        mkL = mconcat $
            [ Diffusion.Block.blockListeners logTrace logic protocolConstants recoveryHeadersMessage oq keepaliveTimer
            , Diffusion.Txp.txListeners logTrace logic oq enqueue
            , Diffusion.Update.updateListeners logTrace logic oq enqueue
            , Diffusion.Delegation.delegationListeners logTrace logic oq enqueue
            , Diffusion.Ssc.sscListeners logTrace logic oq enqueue
            ] ++ [
              subscriptionListeners logTrace oq subscriberNodeType
            | Just (subscriberNodeType, _) <- [mSubscribers]
            ]

        listeners :: VerInfo -> [Listener]
        listeners = mkListeners mkL ourVerInfo

        currentSlotDuration :: IO Millisecond
        currentSlotDuration = bvdSlotDuration <$> getAdoptedBVData logic

        -- Bracket kademlia and network-transport, create a node. This
        -- will be very involved. Should make it top-level I think.
        runDiffusionLayer :: forall y . (FullDiffusionInternals -> IO y) -> IO y
        runDiffusionLayer = runDiffusionLayerFull
            logTrace
            transport
            oq
            (fdcConvEstablishTimeout fdconf)
            ourVerInfo
            defaultPort
            mKademliaParams
            mSubscriptionWorker
            mEkgNodeMetrics
            keepaliveTimer
            currentSlotDuration
            subscriptionStatus
            listeners

        enqueue :: EnqueueMsg
        enqueue = makeEnqueueMsg logTrace ourVerInfo $ \msgType k -> do
            itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
            pure (M.fromList itList)

        getBlocks :: NodeId
                  -> HeaderHash
                  -> [HeaderHash]
                  -> IO (OldestFirst [] Block)
        getBlocks = Diffusion.Block.getBlocks logTrace logic recoveryHeadersMessage enqueue

        requestTip :: IO (Map NodeId (IO BlockHeader))
        requestTip = Diffusion.Block.requestTip logTrace logic enqueue recoveryHeadersMessage

        announceBlockHeader :: MainBlockHeader -> IO ()
        announceBlockHeader = void . Diffusion.Block.announceBlockHeader logTrace logic protocolConstants recoveryHeadersMessage enqueue

        sendTx :: TxAux -> IO Bool
        sendTx = Diffusion.Txp.sendTx logTrace enqueue

        sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> IO ()
        sendUpdateProposal = Diffusion.Update.sendUpdateProposal logTrace enqueue

        sendVote :: UpdateVote -> IO ()
        sendVote = Diffusion.Update.sendVote logTrace enqueue

        -- TODO put these into a Pos.Diffusion.Full.Ssc module.
        sendSscCert :: VssCertificate -> IO ()
        sendSscCert = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCVssCertificate

        sendSscOpening :: Opening -> IO ()
        sendSscOpening = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCOpening (ourStakeholderId logic)

        sendSscShares :: InnerSharesMap -> IO ()
        sendSscShares = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCShares (ourStakeholderId logic)

        sendSscCommitment :: SignedCommitment -> IO ()
        sendSscCommitment = void . invReqDataFlowTK logTrace "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCCommitment

        sendPskHeavy :: ProxySKHeavy -> IO ()
        sendPskHeavy = Diffusion.Delegation.sendPskHeavy logTrace enqueue

        formatPeers :: forall r . (forall a . Format r a -> a) -> IO (Maybe r)
        formatPeers formatter = Just <$> OQ.dumpState oq formatter

        diffusion :: Diffusion IO
        diffusion = Diffusion {..}

        runInternals = RunFullDiffusionInternals
            { runFullDiffusionInternals = runDiffusionLayer
            }

    return (diffusion, runInternals)
=======

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | The full diffusion layer.
--
-- NB: we could do the whole logic/diffusion layer interface using typeclasses
-- against a monad, but we'd end up with a bunch of reader constraints over
-- the values that would otherwise appear in the Logic and Diffusion records.
-- That's to say, we'd have to do the same work anyway, but then even more
-- work to juggle the instances.
diffusionLayerFull
    :: forall d m x .
       ( DiffusionWorkMode d
       , MonadFix d
       , MonadIO m
       , MonadMask m
       , WithLogger m
       )
    => (forall y . d y -> IO y)
    -> NetworkConfig KademliaParams
    -> BlockVersion -- For making the VerInfo.
    -> ProtocolMagic
    -> ProtocolConstants -- Certain protocol constants affect networking (accidental/historical; will fix soon).
    -> Word
    -> Transport d
    -> Maybe (EkgNodeMetrics d)
    -> ((Logic d -> m (DiffusionLayer d)) -> m x)
    -> m x
diffusionLayerFull runIO networkConfig lastKnownBlockVersion protocolMagic protocolConstants recoveryHeadersMessage transport mEkgNodeMetrics expectLogic =
    bracket acquire release $ \_ -> expectLogic $ \logic -> do

        -- Make the outbound queue using network policies.
        -- NB: the <> is under 'LoggerName', which is representationally equal
        -- to 'Text' but its semigroup instance adds a '.' in-between.
        -- The result: the outbound queue will log under the
        -- ["diffusion", "outboundqueue"] hierarchy via log-warper.
        oq :: OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket <-
            initQueue networkConfig ("diffusion" <> "outboundqueue") (enmStore <$> mEkgNodeMetrics)

        -- Subscription status.
        subscriptionStatus <- newTVarIO MS.empty

        -- Timer is in microseconds.
        keepaliveTimer :: Timer <- newTimer $ convertUnit (20 :: Second)

        let -- VerInfo is a diffusion-layer-specific thing. It's only used for
            -- negotiating with peers.
            --
            -- Known bug: if the block version changes, the VerInfo will be
            -- out of date, as it's immutable.
            -- Solution: don't put it in the VerInfo. Other clients don't need
            -- to know the peer's latest adopted block version, they need only
            -- know what software version its running.
            ourVerInfo :: VerInfo
            ourVerInfo = VerInfo (getProtocolMagic protocolMagic)
                                 lastKnownBlockVersion
                                 ins
                                 (outs <> workerOuts)

            ins :: HandlerSpecs
            InSpecs ins = inSpecs mkL

            -- The out specs come not just from listeners but also from workers.
            -- Workers in the existing implementation were bundled up in
            --   allWorkers :: ([WorkerSpec m], OutSpecs)
            -- and they performed logic layer tasks, so having out specs defined
            -- by them doesn't make sense.
            -- For the first iteration, we just dump those out specs here, since
            -- we know in the diffusion layer the set of all requests that might
            -- be made.
            --
            -- Find below a definition of each of the worker out specs,
            -- copied from Pos.Worker (allWorkers). Each one was manually
            -- inspected to determine the out specs.
            --
            -- FIXME this system must change. Perhaps replace it with a
            -- version number?
            outs :: HandlerSpecs
            OutSpecs outs = outSpecs mkL

            workerOuts :: HandlerSpecs
            OutSpecs workerOuts = mconcat
                [ -- First: the relay system out specs.
                  Diffusion.Txp.txOutSpecs logic
                , Diffusion.Update.updateOutSpecs logic
                , Diffusion.Delegation.delegationOutSpecs logic
                , Diffusion.Ssc.sscOutSpecs logic
                  -- Relay system for blocks is ad-hoc.
                , blockWorkerOutSpecs
                  -- SSC has non-relay out specs, defined below.
                , sscWorkerOutSpecs
                , securityWorkerOutSpecs
                , slottingWorkerOutSpecs
                , subscriptionWorkerOutSpecs
                , dhtWorkerOutSpecs
                ]

            -- An onNewSlotWorker and a localWorker. Latter is mempty. Former
            -- actually does the ssc stuff.
            sscWorkerOutSpecs = mconcat
                [ createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCCommitment))
                , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCOpening))
                , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCShares))
                , createOutSpecs (Proxy @(InvOrDataTK StakeholderId MCVssCertificate))
                ]

            -- A single worker checkForReceivedBlocksWorker with
            -- requestTipOuts from Pos.Block.Network.
            securityWorkerOutSpecs = toOutSpecs
                [ convH (Proxy :: Proxy MsgGetHeaders)
                        (Proxy :: Proxy MsgHeaders)
                ]

            -- announceBlockHeaderOuts from blkCreatorWorker
            -- announceBlockHeaderOuts from blkMetricCheckerWorker
            -- along with the retrieval worker outs which also include
            -- announceBlockHeaderOuts.
            blockWorkerOutSpecs = mconcat
                [ announceBlockHeaderOuts
                , announceBlockHeaderOuts
                , announceBlockHeaderOuts <> toOutSpecs [ convH (Proxy :: Proxy MsgGetBlocks)
                                                                (Proxy :: Proxy MsgBlock)
                                                        ]
                ]

            announceBlockHeaderOuts = toOutSpecs [ convH (Proxy :: Proxy MsgHeaders)
                                                         (Proxy :: Proxy MsgGetHeaders)
                                                 ]

            -- Plainly mempty from the definition of allWorkers.
            slottingWorkerOutSpecs = mempty

            -- Copied from existing implementation but
            -- FIXME it will be wrong when the patch to include a keepalive
            -- is merged. That shall be the first test of this inspec/outspec
            -- system I suppose.
            subscriptionWorkerOutSpecs = case topologySubscriptionWorker (ncTopology networkConfig) of
                Just (SubscriptionWorkerBehindNAT _)       -> specs
                Just (SubscriptionWorkerKademlia __ _ _ _) -> specs
                _                                          -> mempty
              where
                specs = toOutSpecs
                    [ convH (Proxy @MsgSubscribe)  (Proxy @Void)
                    , convH (Proxy @MsgSubscribe1) (Proxy @Void)
                    ]

            -- It's a localOnNewSlotWorker, so mempty.
            dhtWorkerOutSpecs = mempty

            mkL :: MkListeners d
            mkL = mconcat $
                [ lmodifier "block"       $ Diffusion.Block.blockListeners logic protocolConstants recoveryHeadersMessage oq keepaliveTimer
                , lmodifier "tx"          $ Diffusion.Txp.txListeners logic oq enqueue
                , lmodifier "update"      $ Diffusion.Update.updateListeners logic oq enqueue
                , lmodifier "delegation"  $ Diffusion.Delegation.delegationListeners logic oq enqueue
                , lmodifier "ssc"         $ Diffusion.Ssc.sscListeners logic oq enqueue
                ] ++ [
                  lmodifier "subscription" $ subscriptionListeners oq subscriberNodeType
                | Just (subscriberNodeType, _) <- [topologySubscribers (ncTopology networkConfig)]
                ]

            lmodifier lname mkLs = mkLs { mkListeners = mkListeners' }
              where
                mkListeners' v p =
                    let ls = mkListeners mkLs v p
                        f = wrapListener ("server" <> lname)
                    in  map f ls

            listeners :: VerInfo -> [Listener d]
            listeners = mkListeners mkL ourVerInfo

            currentSlotDuration :: d Millisecond
            currentSlotDuration = bvdSlotDuration <$> getAdoptedBVData logic

            convEstablishTimeout :: Microsecond
            convEstablishTimeout = convertUnit (15 :: Second)

            -- Bracket kademlia and network-transport, create a node. This
            -- will be very involved. Should make it top-level I think.
            runDiffusionLayer :: forall y . d y -> d y
            runDiffusionLayer = runDiffusionLayerFull
                runIO
                networkConfig
                transport
                convEstablishTimeout
                ourVerInfo
                mEkgNodeMetrics
                oq
                keepaliveTimer
                currentSlotDuration
                subscriptionStatus
                listeners

            enqueue :: EnqueueMsg d
            enqueue = makeEnqueueMsg ourVerInfo $ \msgType k -> liftIO $ do
                itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
                let itMap = M.fromList itList
                    -- FIXME this is duplicated.
                    -- Define once, perhaps in cardano-sl-infra near the
                    -- definition of EnqueueMsg.
                    waitOnIt :: STM.TVar (OQ.PacketStatus a) -> d a
                    waitOnIt tvar = liftIO $ do
                        it <- STM.atomically $ do
                                  status <- STM.readTVar tvar
                                  case status of
                                      OQ.PacketEnqueued        -> STM.retry
                                      OQ.PacketAborted         -> return Nothing
                                      OQ.PacketDequeued thread -> return (Just thread)
                        case it of
                            Nothing -> throwIO Aborted
                            Just thread -> Async.wait thread
                return (waitOnIt <$> itMap)

            getBlocks :: NodeId
                      -> BlockHeader
                      -> [HeaderHash]
                      -> d (OldestFirst [] Block)
            getBlocks = Diffusion.Block.getBlocks logic recoveryHeadersMessage enqueue

            requestTip :: (BlockHeader -> NodeId -> d t) -> d (Map NodeId (d t))
            requestTip = Diffusion.Block.requestTip logic enqueue

            announceBlockHeader :: MainBlockHeader -> d ()
            announceBlockHeader = void . Diffusion.Block.announceBlockHeader logic protocolConstants recoveryHeadersMessage enqueue

            sendTx :: TxAux -> d Bool
            sendTx = Diffusion.Txp.sendTx enqueue

            sendUpdateProposal :: UpId -> UpdateProposal -> [UpdateVote] -> d ()
            sendUpdateProposal = Diffusion.Update.sendUpdateProposal enqueue

            sendVote :: UpdateVote -> d ()
            sendVote = Diffusion.Update.sendVote enqueue

            -- FIXME
            -- SSC stuff has a 'waitUntilSend' motif before it. Must remember to
            -- investigate that and port it if necessary...
            -- No, it really should be the logic layer which decides when to send
            -- things.
            --
            -- TODO put these into a Pos.Diffusion.Full.Ssc module.
            sendSscCert :: VssCertificate -> d ()
            sendSscCert = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCVssCertificate

            sendSscOpening :: Opening -> d ()
            sendSscOpening = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCOpening (ourStakeholderId logic)

            sendSscShares :: InnerSharesMap -> d ()
            sendSscShares = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCShares (ourStakeholderId logic)

            sendSscCommitment :: SignedCommitment -> d ()
            sendSscCommitment = void . invReqDataFlowTK "ssc" enqueue (MsgMPC OriginSender) (ourStakeholderId logic) . MCCommitment

            sendPskHeavy :: ProxySKHeavy -> d ()
            sendPskHeavy = Diffusion.Delegation.sendPskHeavy enqueue

            -- Amazon Route53 health check support (stopgap measure, see note
            -- in Pos.Diffusion.Types, above 'healthStatus' record field).
            healthStatus :: d HealthStatus
            healthStatus = topologyHealthStatus (ncTopology networkConfig) oq

            formatPeers :: forall r . (forall a . Format r a -> a) -> d (Maybe r)
            formatPeers formatter = liftIO $ (Just <$> OQ.dumpState oq formatter)

            diffusion :: Diffusion d
            diffusion = Diffusion {..}

        return DiffusionLayer {..}

  where
    -- TBD will we need any resources here?
    acquire = pure ()
    release = \_ -> pure ()
>>>>>>> CHW-82-84, orphan branch

-- | Create kademlia, network-transport, and run the outbound queue's
-- dequeue thread.
runDiffusionLayerFull
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Transport
    -> OQ.OutboundQ EnqueuedConversation NodeId Bucket
    -> Microsecond -- ^ Conversation establish timeout
    -> VerInfo
    -> Word16 -- ^ Default port to use for resolved hosts (from dns)
    -> Maybe (KademliaParams, Bool)
    -> Maybe SubscriptionWorker
    -> Maybe EkgNodeMetrics
    -> Timer -- ^ Keepalive timer.
    -> IO Millisecond -- ^ Slot duration; may change over time.
    -> TVar (MS.Map NodeId SubscriptionStatus) -- ^ Subscription status.
    -> (VerInfo -> [Listener])
    -> (FullDiffusionInternals -> IO x)
    -> IO x
runDiffusionLayerFull logTrace
                      transport
                      oq
                      convEstablishTimeout
                      ourVerInfo
                      defaultPort
                      mKademliaParams
                      mSubscriptionWorker
                      mEkgNodeMetrics
                      keepaliveTimer
                      slotDuration
                      subscriptionStatus
                      listeners
                      k =
    maybeBracketKademliaInstance logTrace mKademliaParams defaultPort $ \mKademlia ->
        timeWarpNode logTrace transport convEstablishTimeout ourVerInfo listeners $ \nd converse ->
            -- TODO use 'concurrently' or similar combinator.
            Async.withAsync (liftIO $ OQ.dequeueThread oq (sendMsgFromConverse converse)) $ \dthread -> do
                Async.link dthread
=======
    :: forall d x .
       ( DiffusionWorkMode d, MonadFix d )
    => (forall y . d y -> IO y)
    -> NetworkConfig KademliaParams
    -> Transport d
    -> Microsecond -- ^ Conversation establish timeout
    -> VerInfo
    -> Maybe (EkgNodeMetrics d)
    -> OQ.OutboundQ (EnqueuedConversation d) NodeId Bucket
    -> Timer -- ^ Keepalive timer.
    -> d Millisecond -- ^ Slot duration; may change over time.
    -> TVar (MS.Map NodeId SubscriptionStatus) -- ^ Subscription status.
    -> (VerInfo -> [Listener d])
    -> d x
    -> d x
runDiffusionLayerFull runIO networkConfig transport convEstablishTimeout ourVerInfo mEkgNodeMetrics oq keepaliveTimer slotDuration subscriptionStatus listeners action =
    bracketKademlia networkConfig $ \networkConfig' ->
        timeWarpNode transport convEstablishTimeout ourVerInfo listeners $ \nd converse ->
            withAsync (liftIO $ OQ.dequeueThread oq (sendMsgFromConverse runIO converse)) $ \dthread -> do
                link dthread
>>>>>>> CHW-82-84, orphan branch
                case mEkgNodeMetrics of
                    Just ekgNodeMetrics -> registerEkgNodeMetrics ekgNodeMetrics nd
                    Nothing -> pure ()
                -- Subscription worker bypasses the outbound queue and uses
                -- send actions directly.
<<<<<<< HEAD
                let sendActions :: SendActions
                    sendActions = makeSendActions logTrace ourVerInfo oqEnqueue converse
                Async.withAsync (subscriptionThread (fst <$> mKademlia) sendActions) $ \sthread -> do
                    Async.link sthread
                    maybe (pure ()) (joinKademlia logTrace) mKademlia
                    k $ FullDiffusionInternals
                        { fdiNode = nd
                        , fdiConverse = converse
                        }
  where
    oqEnqueue :: Msg
              -> (NodeId -> VerInfo -> Conversation PackingType t)
              -> IO (Map NodeId (STM.TVar (OQ.PacketStatus t)))
    oqEnqueue msgType l = do
        itList <- OQ.enqueue oq msgType (EnqueuedConversation (msgType, l))
        return (M.fromList itList)
    subscriptionThread mKademliaInst sactions = case mSubscriptionWorker of
        Just (SubscriptionWorkerBehindNAT dnsDomains) ->
            dnsSubscriptionWorker logTrace oq defaultPort dnsDomains keepaliveTimer slotDuration subscriptionStatus sactions
        Just (SubscriptionWorkerKademlia nodeType valency fallbacks) -> case mKademliaInst of
            -- Caller wanted a DHT subscription worker, but not a Kademlia
            -- instance. Shouldn't be allowed, but oh well FIXME later.
            Nothing -> pure ()
            Just kInst -> dhtSubscriptionWorker logTrace oq kInst nodeType valency fallbacks sactions
        Nothing -> pure ()

sendMsgFromConverse
    :: Converse PackingType PeerData
    -> OQ.SendMsg EnqueuedConversation NodeId
sendMsgFromConverse converse (EnqueuedConversation (_, k)) nodeId =
    converseWith converse nodeId (k nodeId)

-- | Bring up a time-warp node. It will come down when the continuation ends.
timeWarpNode
    :: Trace IO (Severity, Text)
    -> Transport
    -> Microsecond -- Timeout.
    -> VerInfo
    -> (VerInfo -> [Listener])
    -> (Node -> Converse PackingType PeerData -> IO t)
    -> IO t
timeWarpNode logTrace transport convEstablishTimeout ourVerInfo listeners k = do
    stdGen <- newStdGen
    node logTrace mkTransport mkReceiveDelay mkConnectDelay stdGen bipPacking ourVerInfo nodeEnv $ \theNode ->
=======
                let sendActions :: SendActions d
                    sendActions = makeSendActions ourVerInfo oqEnqueue converse
                withAsync (subscriptionThread networkConfig' sendActions) $ \sthread -> do
                    link sthread
                    joinKademlia networkConfig'
                    action
  where
    oqEnqueue :: Msg -> (NodeId -> VerInfo -> Conversation PackingType d t) -> d (Map NodeId (d t))
    oqEnqueue msgType k = do
        itList <- liftIO $ OQ.enqueue oq msgType (EnqueuedConversation (msgType, k))
        let itMap = M.fromList itList
            -- Wait on the TVar until it's either aborted or dequeued.
            -- If it's aborted, throw an exception (TBD consider giving
            -- Nothing instead?) and if it's dequeued, wait on the thread.
            -- FIXME we'll want to refine this a bit. Callers should be able
            -- to get a hold of the Async instead.
            waitOnIt :: STM.TVar (OQ.PacketStatus a) -> d a
            waitOnIt tvar = liftIO $ do
                it <- STM.atomically $ do
                          status <- STM.readTVar tvar
                          case status of
                              OQ.PacketEnqueued        -> STM.retry
                              OQ.PacketAborted         -> return Nothing
                              OQ.PacketDequeued thread -> return (Just thread)
                case it of
                    Nothing -> throwIO Aborted
                    Just thread -> Async.wait thread
        return (waitOnIt <$> itMap)
    subscriptionThread nc sactions = case topologySubscriptionWorker (ncTopology nc) of
        Just (SubscriptionWorkerBehindNAT dnsDomains) ->
            dnsSubscriptionWorker oq networkConfig dnsDomains keepaliveTimer slotDuration subscriptionStatus sactions
        Just (SubscriptionWorkerKademlia kinst nodeType valency fallbacks) ->
            dhtSubscriptionWorker oq kinst nodeType valency fallbacks sactions
        Nothing -> pure ()

data Aborted = Aborted
  deriving (Show)

instance Exception Aborted

sendMsgFromConverse
    :: (forall x . d x -> IO x)
    -> Converse PackingType PeerData d
    -> OQ.SendMsg (EnqueuedConversation d) NodeId
sendMsgFromConverse runIO converse (EnqueuedConversation (_, k)) nodeId =
    runIO $ converseWith converse nodeId (k nodeId)

-- | Bring up a time-warp node. It will come down when the continuation ends.
timeWarpNode
    :: forall d t .
       ( DiffusionWorkMode d, MonadFix d )
    => Transport d
    -> Microsecond -- Timeout.
    -> VerInfo
    -> (VerInfo -> [Listener d])
    -> (Node d -> Converse PackingType PeerData d -> d t)
    -> d t
timeWarpNode transport convEstablishTimeout ourVerInfo listeners k = do
    stdGen <- liftIO newStdGen
    node mkTransport mkReceiveDelay mkConnectDelay stdGen bipPacking ourVerInfo nodeEnv $ \theNode ->
>>>>>>> CHW-82-84, orphan branch
        NodeAction listeners $ k theNode
  where
    mkTransport = simpleNodeEndPoint transport
    mkReceiveDelay = const (pure Nothing)
    mkConnectDelay = const (pure Nothing)
    nodeEnv = defaultNodeEnvironment { nodeAckTimeout = convEstablishTimeout }

----------------------------------------------------------------------------
-- Kademlia
----------------------------------------------------------------------------

<<<<<<< HEAD
createKademliaInstance
    :: Trace IO (Severity, Text)
    -> KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> IO KademliaDHTInstance
createKademliaInstance logTrace kp defaultPort =
    startDHTInstance logTrace instConfig defaultBindAddress
=======
createKademliaInstance ::
       (MonadIO m, MonadCatch m, CanLog m)
    => KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> m KademliaDHTInstance
createKademliaInstance kp defaultPort =
    usingLoggerName "kademlia" (startDHTInstance instConfig defaultBindAddress)
>>>>>>> CHW-82-84, orphan branch
  where
    instConfig = kp {kpPeers = ordNub $ kpPeers kp}
    defaultBindAddress = ("0.0.0.0", defaultPort)

-- | RAII for 'KademliaDHTInstance'.
bracketKademliaInstance
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> (KademliaParams, Bool)
    -> Word16
    -> ((KademliaDHTInstance, Bool) -> IO a)
    -> IO a
bracketKademliaInstance logTrace (kp, mustJoin) defaultPort action =
    bracket (createKademliaInstance logTrace kp defaultPort) stopDHTInstance $ \kinst ->
        action (kinst, mustJoin)

maybeBracketKademliaInstance
    :: Trace IO (Severity, Text)
    -> Maybe (KademliaParams, Bool)
    -> Word16
    -> (Maybe (KademliaDHTInstance, Bool) -> IO a)
    -> IO a
maybeBracketKademliaInstance _ Nothing _ k = k Nothing
maybeBracketKademliaInstance logTrace (Just kp) defaultPort k =
    bracketKademliaInstance logTrace kp defaultPort (k . Just)

-- | Join the Kademlia network.
joinKademlia :: Trace IO (Severity, Text) -> (KademliaDHTInstance, Bool) -> IO ()
joinKademlia logTrace (kInst, mustJoin) = case mustJoin of
    True  -> kademliaJoinNetworkRetry logTrace kInst (kdiInitialPeers kInst) retryInterval
    False -> kademliaJoinNetworkNoThrow logTrace kInst (kdiInitialPeers kInst)
=======
    :: (MonadIO m, MonadMask m, CanLog m)
    => KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> (KademliaDHTInstance -> m a)
    -> m a
bracketKademliaInstance kp defaultPort action =
    bracket (createKademliaInstance kp defaultPort) stopDHTInstance action

-- | The 'NodeParams' contain enough information to determine whether a Kademlia
-- instance should be brought up. Use this to safely acquire/release one.
bracketKademlia
    :: (MonadIO m, MonadMask m, CanLog m)
    => NetworkConfig KademliaParams
    -> (NetworkConfig KademliaDHTInstance -> m a)
    -> m a
bracketKademlia nc@NetworkConfig {..} action = case ncTopology of
    -- cases that need Kademlia
    TopologyP2P{topologyKademlia = kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyP2P{topologyKademlia = kinst, ..}
    TopologyTraditional{topologyKademlia = kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyTraditional{topologyKademlia = kinst, ..}
    TopologyRelay{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyRelay{topologyOptKademlia = Just kinst, ..}
    TopologyCore{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyCore{topologyOptKademlia = Just kinst, ..}

    -- cases that don't
    TopologyRelay{topologyOptKademlia = Nothing, ..} ->
        k $ TopologyRelay{topologyOptKademlia = Nothing, ..}
    TopologyCore{topologyOptKademlia = Nothing, ..} ->
        k $ TopologyCore{topologyOptKademlia = Nothing, ..}
    TopologyBehindNAT{..} ->
        k $ TopologyBehindNAT{..}
    TopologyAuxx{..} ->
        k $ TopologyAuxx{..}
  where
    k topology = action (nc { ncTopology = topology })

-- | Synchronously join the Kademlia network.
joinKademlia
    :: ( DiffusionWorkMode m )
    => NetworkConfig KademliaDHTInstance
    -> m ()
joinKademlia networkConfig = case topologyRunKademlia (ncTopology networkConfig) of
    -- See 'topologyRunKademlia' documentation: the second component is 'True'
    -- iff it's essential that at least one of the initial peers is contacted.
    -- Otherwise, it's OK to not find any initial peers and the program can
    -- continue.
    Just (kInst, True)  -> kademliaJoinNetworkRetry kInst (kdiInitialPeers kInst) retryInterval
    Just (kInst, False) -> kademliaJoinNetworkNoThrow kInst (kdiInitialPeers kInst)
    Nothing             -> return ()
>>>>>>> CHW-82-84, orphan branch
  where
    retryInterval :: Second
    retryInterval = 5
