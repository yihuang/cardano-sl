{-# LANGUAGE RankNTypes #-}

-- | Server which deals with blocks processing.

module Pos.Block.Network.Retrieval
       ( retrievalWorker
       ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM (putTMVar, swapTMVar, tryReadTBQueue, tryReadTMVar,
                                         tryTakeTMVar)
import           Control.Exception.Safe (handleAny)
import           Control.Lens (to)
import           Control.Monad.STM (retry)
import           Data.Aeson (Value)
import qualified Data.List.NonEmpty as NE
import           Formatting (build, int, sformat, (%))

import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Logic (ClassifyHeaderRes (..), classifyNewHeader, getHeadersOlderExp)
import           Pos.Block.Network.Logic (BlockNetLogicException (..), handleBlocks, triggerRecovery)
import           Pos.Block.RetrievalQueue (BlockRetrievalQueueTag, BlockRetrievalTask (..))
import           Pos.Block.Types (RecoveryHeaderTag)
import           Pos.Communication.Protocol (NodeId)
import           Pos.Core (Block, HasHeaderHash (..),  HeaderHash, difficultyL, isMoreDifficult)
import           Pos.Core.Block (BlockHeader)
import           Pos.Crypto (shortHashF)
import qualified Pos.DB.BlockIndex as DB
import           Pos.Diffusion.Types (Diffusion)
import qualified Pos.Diffusion.Types as Diffusion (Diffusion (getBlocks))
import           Pos.Reporting (HasMisbehaviorMetrics, reportOrLogE, reportOrLogW)
import           Pos.Util.Chrono (NE, OldestFirst (..), _OldestFirst)
import           Pos.Util.Util (HasLens (..))
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem, logDebug, logError,
                                              logInfo, logWarning)
-- TODO no explicit wlog dependencies.
import           Pos.Util.Trace.Wlog (LogNamed, named)

-- I really don't like join
{-# ANN retrievalWorker ("HLint: ignore Use join" :: Text) #-}

-- | Worker that queries blocks. It has two jobs:
--
-- * If there are headers in 'BlockRetrievalQueue', this worker retrieves
--   blocks according to that queue.
--
-- * If recovery is in progress, this worker keeps recovery going by asking
--   headers (and then switching to block retrieval on next loop iteration).
--
-- If both happen at the same time, 'BlockRetrievalQueue' takes precedence.
--
retrievalWorker
    :: forall ctx m.
       ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => Trace m (LogNamed LogItem)
    -> Trace m Value -- Json log.
    -> Diffusion m
    -> m ()
retrievalWorker logTrace jsonLogTrace diffusion = do
    logInfo (named logTrace) "Starting retrievalWorker loop"
    mainLoop
  where
    mainLoop = do
        queue        <- view (lensOf @BlockRetrievalQueueTag)
        recHeaderVar <- view (lensOf @RecoveryHeaderTag)
        logDebug (named logTrace) "Waiting on the block queue or recovery header var"
        -- Reading the queue is a priority, because it sets the recovery
        -- variable in case the header is classified as alternative. So if the
        -- queue contains lots of headers after a long delay, we'll first
        -- iterate over them and set recovery variable to the latest one, and
        -- only then we'll do recovery.
        thingToDoNext <- atomically $ do
            mbQueuedHeadersChunk <- tryReadTBQueue queue
            mbRecHeader <- tryReadTMVar recHeaderVar
            case (mbQueuedHeadersChunk, mbRecHeader) of
                (Nothing, Nothing) -> retry
                -- Dispatch the task
                (Just (nodeId, task), _) ->
                    pure (handleBlockRetrieval nodeId task)
                -- No tasks & the recovery header is set => do the recovery
                (_, Just (nodeId, rHeader))  ->
                    pure (handleRecoveryWithHandler nodeId rHeader)

        -- Exception handlers are installed locally, on the 'thingToDoNext',
        -- to ensure that network troubles, for instance, do not kill the
        -- worker.
        () <- thingToDoNext
        mainLoop

    -----------------

    -- That's the first queue branch (task dispatching).
    handleBlockRetrieval nodeId BlockRetrievalTask{..} =
        handleAny (handleRetrievalE nodeId brtHeader) $ do
            logDebug (named logTrace) $ sformat
                ("Block retrieval queue task received, nodeId="%build%
                 ", header="%build%", continues="%build)
                nodeId
                (headerHash brtHeader)
                brtContinues
            (if brtContinues then handleContinues else handleAlternative)
                nodeId
                brtHeader

    -- When we have a continuation of the chain, just try to get and apply it.
    handleContinues nodeId header = do
        let hHash = headerHash header
        logDebug (named logTrace) $ "handleContinues: " <> pretty hHash
        classifyNewHeader header >>= \case
            CHContinues ->
                void $ getProcessBlocks logTrace jsonLogTrace diffusion nodeId (headerHash header) [hHash]
            res -> logDebug (named logTrace) $
                "processContHeader: expected header to " <>
                "be continuation, but it's " <> show res

    -- When we have an alternative header, we should check whether it's actually
    -- recovery mode (server side should send us headers as a proof) and then
    -- enter recovery mode.
    handleAlternative nodeId header = do
        logDebug (named logTrace) $ "handleAlternative: " <> pretty (headerHash header)
        classifyNewHeader header >>= \case
            CHInvalid _ ->
                logError (named logTrace) $ "handleAlternative: invalid header got into retrievalWorker queue"
            CHUseless _ ->
                logDebug (named logTrace) $
                    sformat ("handleAlternative: header "%build%" became useless, ignoring it")
                            header
            _ -> do
                logDebug (named logTrace) "handleAlternative: considering header for recovery mode"
                -- CSL-1514
                updateRecoveryHeader (named logTrace) nodeId header

    -- Squelch the exception and continue. Used with 'handleAny' from
    -- safe-exceptions so it will let async exceptions pass.
    handleRetrievalE nodeId cHeader e = do
        reportOrLogW (named logTrace) (sformat
            ("handleRetrievalE: error handling nodeId="%build%", header="%build%": ")
            nodeId (headerHash cHeader)) e

    -----------------

    handleRecoveryWithHandler nodeId rHeader =
        handleAny (handleRecoveryE nodeId rHeader) $
        handleRecovery nodeId rHeader

    -- We immediately drop recovery mode/header and request tips
    -- again.
    handleRecoveryE nodeId rHeader e = do
        -- REPORT:ERROR 'reportOrLogW' in block retrieval worker/recovery.
        reportOrLogW (named logTrace) (sformat
            ("handleRecoveryE: error handling nodeId="%build%", header="%build%": ")
            nodeId (headerHash rHeader)) e
        dropRecoveryHeaderAndRepeat (named logTrace) diffusion nodeId

    -- Recovery handling. We assume that header in the recovery variable is
    -- appropriate and just query headers/blocks.
    handleRecovery :: NodeId -> BlockHeader -> m ()
    handleRecovery nodeId rHeader = do
        logDebug (named logTrace) $
            "Block retrieval queue is empty and we're in recovery mode,\
            \ so we will fetch more blocks"
        whenM (fmap isJust $ DB.getHeader $ headerHash rHeader) $
            -- How did we even got into recovery then?
            throwM $ DialogUnexpected $ "handleRecovery: recovery header is " <>
                                        "already present in db"
        logDebug (named logTrace) "handleRecovery: fetching blocks"
        checkpoints <- toList <$> getHeadersOlderExp Nothing
        void $ getProcessBlocks logTrace jsonLogTrace diffusion nodeId (headerHash rHeader) checkpoints

----------------------------------------------------------------------------
-- Entering and exiting recovery mode
----------------------------------------------------------------------------

-- | Result of attempt to update recovery header.
data UpdateRecoveryResult ssc
    = RecoveryStarted NodeId BlockHeader
      -- ^ Recovery header was absent, so we've set it.
    | RecoveryShifted NodeId BlockHeader NodeId BlockHeader
      -- ^ Header was present, but we've replaced it with another
      -- (more difficult) one.
    | RecoveryContinued NodeId BlockHeader
      -- ^ Header is good, but is irrelevant, so recovery variable is
      -- unchanged.

-- | Be careful to run this in the same thread that ends recovery mode
-- (or synchronise those threads with an MVar), otherwise a race
-- condition can occur where we are caught in the recovery mode
-- indefinitely.
updateRecoveryHeader
    :: BlockWorkMode ctx m
    => Trace m LogItem
    -> NodeId
    -> BlockHeader
    -> m ()
updateRecoveryHeader logTrace nodeId hdr = do
    recHeaderVar <- view (lensOf @RecoveryHeaderTag)
    logDebug logTrace "Updating recovery header..."
    updated <- atomically $ do
        mbRecHeader <- tryReadTMVar recHeaderVar
        case mbRecHeader of
            Nothing -> do
                putTMVar recHeaderVar (nodeId, hdr)
                return $ RecoveryStarted nodeId hdr
            Just (oldNodeId, oldHdr) -> do
                let needUpdate = hdr `isMoreDifficult` oldHdr
                if needUpdate
                    then swapTMVar recHeaderVar (nodeId, hdr) $>
                         RecoveryShifted oldNodeId oldHdr nodeId hdr
                    else return $ RecoveryContinued oldNodeId oldHdr
    logDebug logTrace $ case updated of
        RecoveryStarted rNodeId rHeader -> sformat
            ("Recovery started with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)
        RecoveryShifted rNodeId' rHeader' rNodeId rHeader -> sformat
            ("Recovery shifted from nodeId="%build%" and tip="%build%
             " to nodeId="%build%" and tip="%build)
            rNodeId' (headerHash rHeader')
            rNodeId  (headerHash rHeader)
        RecoveryContinued rNodeId rHeader -> sformat
            ("Recovery continued with nodeId="%build%" and tip="%build)
            rNodeId
            (headerHash rHeader)

-- | The returned 'Bool' signifies whether given peer was kicked and recovery
-- was stopped.
--
-- NB. The reason @nodeId@ is passed is that we want to avoid a race
-- condition. If you work with peer P and header H, after failure you want to
-- drop communication with P; however, if at the same time a new block
-- arrives and another thread replaces peer and header to (P2, H2), you want
-- to continue working with P2 and ignore the exception that happened with P.
-- So, @nodeId@ is used to check that the peer wasn't replaced mid-execution.
dropRecoveryHeader
    :: BlockWorkMode ctx m
    => Trace m LogItem
    -> NodeId
    -> m Bool
dropRecoveryHeader logTrace nodeId = do
    recHeaderVar <- view (lensOf @RecoveryHeaderTag)
    (kicked,realPeer) <- atomically $ do
        let processKick (peer,_) = do
                let p = peer == nodeId
                when p $ void $ tryTakeTMVar recHeaderVar
                pure (p, Just peer)
        maybe (pure (True,Nothing)) processKick =<< tryReadTMVar recHeaderVar
    when kicked $ logWarning logTrace $
        sformat ("Recovery mode communication dropped with peer "%build) nodeId
    unless kicked $
        logDebug logTrace $ "Recovery mode wasn't disabled: " <>
                   maybe "noth" show realPeer <> " vs " <> show nodeId
    pure kicked

-- | Drops the recovery header and, if it was successful, queries the tips.
dropRecoveryHeaderAndRepeat
    :: BlockWorkMode ctx m
    => Trace m LogItem
    -> Diffusion m
    -> NodeId
    -> m ()
dropRecoveryHeaderAndRepeat logTrace diffusion nodeId = do
    kicked <- dropRecoveryHeader logTrace nodeId
    when kicked $ attemptRestartRecovery
  where
    attemptRestartRecovery = do
        logDebug logTrace "Attempting to restart recovery"
        -- FIXME why wait, and why this long?
        liftIO $ threadDelay 2000000
        handleAny handleRecoveryTriggerE $ triggerRecovery logTrace diffusion
        logDebug logTrace "Attempting to restart recovery over"
    handleRecoveryTriggerE =
        -- REPORT:ERROR 'reportOrLogE' somewhere in block retrieval.
        reportOrLogE logTrace $
            "Exception happened while trying to trigger " <>
            "recovery inside dropRecoveryHeaderAndRepeat: "

-- Returns only if blocks were successfully downloaded and
-- processed. Throws exception if something goes wrong.
getProcessBlocks
    :: forall ctx m.
       ( BlockWorkMode ctx m
       , HasMisbehaviorMetrics ctx
       )
    => Trace m (LogNamed LogItem)
    -> Trace m Value -- Json log
    -> Diffusion m
    -> NodeId
    -> HeaderHash
    -> [HeaderHash]
    -> m ()
getProcessBlocks logTrace jsonLogTrace diffusion nodeId desired checkpoints = do
    result <- Diffusion.getBlocks diffusion nodeId desired checkpoints
    case OldestFirst <$> nonEmpty (getOldestFirst result) of
      Nothing -> do
          let msg = sformat ("getProcessBlocks: diffusion returned []"%
                             " on request to fetch "%shortHashF%" from peer "%build)
                            desired nodeId
          throwM $ DialogUnexpected msg
      Just (blocks :: OldestFirst NE Block) -> do
          recHeaderVar <- view (lensOf @RecoveryHeaderTag)
          logDebug (named logTrace) $ sformat
              ("Retrieved "%int%" blocks")
              (blocks ^. _OldestFirst . to NE.length)
          handleBlocks logTrace jsonLogTrace blocks diffusion
          -- If we've downloaded any block with bigger
          -- difficulty than ncRecoveryHeader, we're
          -- gracefully exiting recovery mode.
          let isMoreDifficultThan b x = b ^. difficultyL >= x ^. difficultyL
          exitedRecovery <- atomically $ tryReadTMVar recHeaderVar >>= \case
              -- We're not in recovery mode? That must be ok.
              Nothing -> pure False
              -- If we're in recovery mode we should exit it if
              -- any block is more difficult than one in
              -- recHeader.
              Just (_, rHeader) ->
                  if any (`isMoreDifficultThan` rHeader) blocks
                  then isJust <$> tryTakeTMVar recHeaderVar
                  else pure False
          when exitedRecovery $
              logInfo (named logTrace) "Recovery mode exited gracefully on receiving block we needed"
