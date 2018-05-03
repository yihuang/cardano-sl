{-# LANGUAGE CPP            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}

-- | Resources used by node and ways to deal with them.

module Pos.Launcher.Resource
       (
         -- * Full resources
         NodeResources (..)

       , allocateNodeResources
       , releaseNodeResources
       , bracketNodeResources

         -- * Smaller resources
       , loggerBracket
       ) where

import           Universum

import           Control.Concurrent.STM (newEmptyTMVarIO, newTBQueueIO)
import           Data.Default (Default)
import           Data.Functor.Contravariant (contramap)
import qualified Data.Time as Time
import           Formatting (sformat, shown, (%))
import           System.IO (BufferMode (..), Handle, hClose, hSetBuffering)
import qualified System.Metrics as Metrics
import           System.Wlog (LoggerConfig (..), consoleActionB, defaultHandleAction,
                              maybeLogsDirB, productionB, removeAllHandlers,
                              setupLogging, showTidB)

import           Pos.Binary ()
import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Block.Slog (mkSlogContext)
import           Pos.Client.CLI.Util (readLoggerConfig)
import           Pos.Configuration
import           Pos.Context (ConnectedPeers (..), NodeContext (..), StartTime (..))
import           Pos.Core (HasConfiguration, Timestamp, gdStartTime, genesisData)
import           Pos.DB (MonadDBRead, NodeDBs)
import           Pos.DB.Rocks (closeNodeDBs, openNodeDBs)
import           Pos.Delegation (DelegationVar, HasDlgConfiguration, mkDelegationVar)
import           Pos.DHT.Real (KademliaParams (..))
import qualified Pos.GState as GS
import           Pos.Launcher.Param (WlogParams (..), NodeParams (..))
import           Pos.Lrc.Context (LrcContext (..), mkLrcSyncData)
import           Pos.Network.Types (NetworkConfig (..))
import           Pos.Reporting (initializeMisbehaviorMetrics)
import           Pos.Shutdown.Types (ShutdownContext (..))
import           Pos.Slotting (SimpleSlottingStateVar, mkSimpleSlottingStateVar)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Ssc (SscParams, SscState, createSscContext, mkSscState)
import           Pos.StateLock (newStateLock)
import           Pos.Txp (GenericTxpLocalData (..), TxpGlobalSettings, mkTxpLocalData,
                          recordTxpMetrics)

import           Pos.Launcher.Mode (InitMode, InitModeContext (..), runInitMode)
import           Pos.Update.Context (mkUpdateContext)
import qualified Pos.Update.DB as GState
-- FIXME stop using newInitFuture. Surely it's not needed.
import           Pos.Util (newInitFuture)
import           Pos.Util.Trace (Trace, natTrace)
import           Pos.Util.Trace.Unstructured (LogItem, Severity (..), bracketWithLogging,
                                              logDebug, logInfo, publicPrivateLogItem)
import           Pos.Util.Trace.Wlog (LogNamed, appendName, named)

#ifdef linux_HOST_OS
import qualified System.Systemd.Daemon as Systemd
import           Pos.Util.Trace.Unstructured (logWarning)
#endif

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------------
-- Data type
----------------------------------------------------------------------------

-- | This data type contains all resources used by node.
data NodeResources ext = NodeResources
    { nrContext    :: !NodeContext
    , nrDBs        :: !NodeDBs
    , nrSscState   :: !SscState
    , nrTxpState   :: !(GenericTxpLocalData ext)
    , nrDlgState   :: !DelegationVar
    , nrJLogHandle :: !(Maybe Handle)
    -- ^ Handle for JSON logging (optional).
    , nrEkgStore   :: !Metrics.Store
    }

----------------------------------------------------------------------------
-- Allocation/release/bracket
----------------------------------------------------------------------------

-- | Allocate all resources used by node. They must be released eventually.
allocateNodeResources
    :: forall ext .
       ( Default ext
       , HasConfiguration
       , HasNodeConfiguration
       , HasDlgConfiguration
       , HasBlockConfiguration
       )
    => Trace IO (LogNamed LogItem)
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> WlogParams
    -> InitMode ()
    -> IO (NodeResources ext)
allocateNodeResources namedLogTrace np@NodeParams {..} sscnp txpSettings wlogParams initDB = do
    let logTrace = named namedLogTrace
    logInfo logTrace "Allocating node resources..."
    npDbPath <- case npDbPathM of
        Nothing -> do
            let dbPath = "node-db" :: FilePath
            logInfo logTrace $ sformat ("DB path not specified, defaulting to "%
                               shown) dbPath
            return dbPath
        Just dbPath -> return dbPath
    db <- openNodeDBs npRebuildDb npDbPath
    (futureLrcContext, putLrcContext) <- newInitFuture "lrcContext"
    (futureSlottingVar, putSlottingVar) <- newInitFuture "slottingVar"
    (futureSlottingContext, putSlottingContext) <- newInitFuture "slottingContext"
    let putSlotting sv sc = do
            putSlottingVar sv
            putSlottingContext sc
        initModeContext = InitModeContext
            db
            futureSlottingVar
            futureSlottingContext
            futureLrcContext
    logDebug logTrace "Opened DB, created some futures, going to run InitMode"
    runInitMode initModeContext $ do
        initDB
        liftIO $ logDebug logTrace "Initialized DB"

        nrEkgStore <- liftIO $ Metrics.newStore
        liftIO $ logDebug logTrace "Created EKG store"

        txpVar <- mkTxpLocalData -- doesn't use slotting or LRC
        let ancd =
                AllocateNodeContextData
                { ancdNodeParams = np
                , ancdSscParams = sscnp
                , ancdPutSlotting = putSlotting
                , ancdNetworkCfg = npNetworkConfig
                , ancdEkgStore = nrEkgStore
                , ancdTxpMemState = txpVar
                }
        ctx@NodeContext {..} <- allocateNodeContext
            (natTrace liftIO namedLogTrace)
            ancd
            txpSettings
            wlogParams
            nrEkgStore
        putLrcContext ncLrcContext
        liftIO $ logDebug logTrace "Filled LRC Context future"
        dlgVar <- mkDelegationVar
        liftIO $ logDebug logTrace "Created DLG var"
        sscState <- mkSscState (natTrace liftIO logTrace)
        liftIO $ logDebug logTrace "Created SSC var"
        nrJLogHandle <-
            case npJLFile of
                Nothing -> pure Nothing
                Just fp -> do
                    h <- openFile fp WriteMode
                    liftIO $ hSetBuffering h NoBuffering
                    return $ Just h

        liftIO $ logDebug logTrace "Finished allocating node resources!"
        return NodeResources
            { nrContext = ctx
            , nrDBs = db
            , nrSscState = sscState
            , nrTxpState = txpVar
            , nrDlgState = dlgVar
            , ..
            }

-- | Release all resources used by node. They must be released eventually.
releaseNodeResources ::
       NodeResources ext -> IO ()
releaseNodeResources NodeResources {..} = do
    whenJust nrJLogHandle (liftIO . hClose)
    closeNodeDBs nrDBs
    releaseNodeContext nrContext

-- | Run computation which requires 'NodeResources' ensuring that
-- resources will be released eventually.
bracketNodeResources :: forall ext a.
      ( Default ext
      , HasConfiguration
      , HasNodeConfiguration
      , HasDlgConfiguration
      , HasBlockConfiguration
      )
    => Trace IO (LogNamed LogItem)
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> WlogParams
    -> InitMode ()
    -> (HasConfiguration => NodeResources ext -> IO a)
    -> IO a
bracketNodeResources logTrace np sp txp wp initDB action = do
    let msg = "`NodeResources'"
        logTraceText = contramap (publicPrivateLogItem . (,) Error) (named logTrace)
    bracketWithLogging logTraceText msg
            (allocateNodeResources logTrace np sp txp wp initDB)
            releaseNodeResources $ \nodeRes ->do
        -- Notify systemd we are fully operative
        -- FIXME this is not the place to notify.
        -- The network transport is not up yet.
        notifyReady (named logTrace)
        action nodeRes

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

getRealLoggerConfig :: WlogParams -> IO LoggerConfig
getRealLoggerConfig WlogParams {..} = do
    let cfgBuilder = productionB
                  <> showTidB
                  <> maybeLogsDirB wpHandlerPrefix
    cfg <- readLoggerConfig wpConfigPath
    pure $ overrideConsoleLog $ cfg <> cfgBuilder
  where
    overrideConsoleLog :: LoggerConfig -> LoggerConfig
    overrideConsoleLog = case wpConsoleLog of
        Nothing    -> identity
        Just True  -> (<>) (consoleActionB defaultHandleAction)
        Just False -> (<>) (consoleActionB (\_ _ -> pass))

setupLoggers :: WlogParams -> IO ()
setupLoggers params = setupLogging Nothing =<< getRealLoggerConfig params

-- | RAII for Logging.
loggerBracket :: WlogParams -> IO a -> IO a
loggerBracket wp = bracket_ (setupLoggers wp) removeAllHandlers

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data AllocateNodeContextData ext = AllocateNodeContextData
    { ancdNodeParams  :: !NodeParams
    , ancdSscParams   :: !SscParams
    , ancdPutSlotting :: (Timestamp, TVar SlottingData) -> SimpleSlottingStateVar -> InitMode ()
    , ancdNetworkCfg  :: NetworkConfig KademliaParams
    , ancdEkgStore    :: !Metrics.Store
    , ancdTxpMemState :: !(GenericTxpLocalData ext)
    }

allocateNodeContext
    :: forall ext .
      (HasConfiguration, HasNodeConfiguration, HasBlockConfiguration)
    => Trace InitMode (LogNamed LogItem)
    -> AllocateNodeContextData ext
    -> TxpGlobalSettings
    -> WlogParams
    -> Metrics.Store
    -> InitMode NodeContext
allocateNodeContext namedLogTrace ancd txpSettings wlogParams ekgStore = do
    let logTrace = named namedLogTrace
        AllocateNodeContextData { ancdNodeParams = np@NodeParams {..}
                                , ancdSscParams = sscnp
                                , ancdPutSlotting = putSlotting
                                , ancdNetworkCfg = networkConfig
                                , ancdEkgStore = store
                                , ancdTxpMemState = TxpLocalData {..}
                                } = ancd
    logInfo logTrace "Allocating node context..."
    ncLoggerConfig <- liftIO $ getRealLoggerConfig wlogParams
    logDebug logTrace "Got logger config"
    ncStateLock <- newStateLock =<< GS.getTip
    logDebug logTrace "Created a StateLock"
    rctx <- ask
    let txpMetricsTrace = natTrace (flip runReaderT rctx) (named (appendName "metrics" namedLogTrace))
    ncStateLockMetrics <- liftIO $ recordTxpMetrics txpMetricsTrace store txpMemPool
    logDebug logTrace "Created StateLock metrics"
    lcLrcSync <- mkLrcSyncData >>= newTVarIO
    logDebug logTrace "Created LRC sync"
    ncSlottingVar <- (gdStartTime genesisData,) <$> mkSlottingVar
    logDebug logTrace "Created slotting variable"
    ncSlottingContext <- mkSimpleSlottingStateVar
    logDebug logTrace "Created slotting context"
    putSlotting ncSlottingVar ncSlottingContext
    logDebug logTrace "Filled slotting future"
    ncUserSecret <- newTVarIO $ npUserSecret
    logDebug logTrace "Created UserSecret variable"
    ncBlockRetrievalQueue <- liftIO $ newTBQueueIO blockRetrievalQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    logDebug logTrace "Created block retrieval queue, recovery and progress headers"
    ncShutdownFlag <- newTVarIO False
    ncStartTime <- StartTime <$> liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    logDebug logTrace "Created last known header and shutdown flag variables"
    ncUpdateContext <- mkUpdateContext
    logDebug logTrace "Created context for update"
    ncSscContext <- createSscContext sscnp
    logDebug logTrace "Created context for ssc"
    ncSlogContext <- mkSlogContext store
    logDebug logTrace "Created context for slog"
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    logDebug logTrace "Created peersVar"
    mm <- initializeMisbehaviorMetrics ekgStore

    logDebug logTrace "Finished allocating node context!"
    let ctx =
            NodeContext
            { ncConnectedPeers = ConnectedPeers peersVar
            , ncLrcContext = LrcContext {..}
            , ncShutdownContext = ShutdownContext ncShutdownFlag
            , ncNodeParams = np
            , ncTxpGlobalSettings = txpSettings
            , ncNetworkConfig = networkConfig
            , ncMisbehaviorMetrics = Just mm
            , ..
            }
    return ctx

releaseNodeContext :: forall m . MonadIO m => NodeContext -> m ()
releaseNodeContext _ = return ()

-- Create new 'SlottingVar' using data from DB. Probably it would be
-- good to have it in 'infra', but it's complicated.
mkSlottingVar :: (MonadIO m, MonadDBRead m) => m (TVar SlottingData)
mkSlottingVar = newTVarIO =<< GState.getSlottingData

-- | Notify process manager tools like systemd the node is ready.
-- Available only on Linux for systems where `libsystemd-dev` is installed.
-- It defaults to a noop for all the other platforms.
notifyReady :: Trace IO LogItem -> IO ()
#ifdef linux_HOST_OS
notifyReady logTrace = do
    res <- liftIO Systemd.notifyReady
    case res of
        Just () -> return ()
        Nothing -> logWarning logTrace "notifyReady failed to notify systemd."
#else
notifyReady :: (WithLogger m) => m ()
notifyReady = logInfo "notifyReady: no systemd support enabled"
#endif
