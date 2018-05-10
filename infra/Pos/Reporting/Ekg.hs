-- | EKG monitoring.

<<<<<<< HEAD
=======
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

>>>>>>> CHW-82-84, orphan branch
module Pos.Reporting.Ekg
    ( withEkgServer
    , registerEkgMetrics

    , EkgNodeMetrics (..)
    , registerEkgNodeMetrics
    ) where

import           Universum

<<<<<<< HEAD
=======
import           Mockable (Mockable)
import qualified Mockable.Metrics as Mockable
>>>>>>> CHW-82-84, orphan branch
import           Node (Node)
import           Node.Util.Monitor (registerMetrics)

import qualified System.Metrics as Metrics
<<<<<<< HEAD
=======
import qualified System.Metrics.Distribution as Metrics
import qualified System.Metrics.Gauge as Metrics
import qualified System.Metrics.Counter as Metrics
>>>>>>> CHW-82-84, orphan branch
import qualified System.Remote.Monitoring.Wai as Monitoring

import           Pos.Util.Monitor (stopMonitor)
import           Pos.Statistics (EkgParams (..))
import           Pos.System.Metrics.Constants (cardanoNamespace)

<<<<<<< HEAD
-- | All you need in order to register EKG metrics on a time-warp node.
data EkgNodeMetrics = EkgNodeMetrics
    { enmStore :: Metrics.Store
=======
-- | All you need in order to register EKG metrics on a time-warp node over
-- 'm'.
data EkgNodeMetrics m = EkgNodeMetrics
    { enmStore :: Metrics.Store
    , enmElim  :: forall t . m t -> IO t
>>>>>>> CHW-82-84, orphan branch
    }

-- | Register various network-related EKG metrics (relevant to a Node).
registerEkgNodeMetrics
<<<<<<< HEAD
    :: EkgNodeMetrics
    -> Node
    -> IO ()
registerEkgNodeMetrics ekgNodeMetrics nd =
    registerMetrics (Just cardanoNamespace) nd (enmStore ekgNodeMetrics)

-- | Register RTS/GC ekg metrics.
registerEkgMetrics
    :: Metrics.Store
    -> IO ()
registerEkgMetrics ekgStore = Metrics.registerGcMetrics ekgStore

-- | Bracket an EKG web server, so you can look at the metrics in your browser.
withEkgServer
    :: EkgParams
    -> Metrics.Store
    -> IO t
    -> IO t
withEkgServer EkgParams {..} ekgStore act = bracket acquire release (const act)
  where
    acquire = Monitoring.forkServerWith ekgStore ekgHost ekgPort
=======
    :: ( MonadIO m
       , Mockable Mockable.Metrics m
       , Mockable.Distribution m ~ Metrics.Distribution
       , Mockable.Gauge m ~ Metrics.Gauge
       , Mockable.Counter m ~ Metrics.Counter
       )
    => EkgNodeMetrics m
    -> Node m
    -> m ()
registerEkgNodeMetrics ekgNodeMetrics nd =
    registerMetrics (Just cardanoNamespace) (enmElim ekgNodeMetrics) nd (enmStore ekgNodeMetrics)

-- | Register RTS/GC ekg metrics.
registerEkgMetrics
    :: ( MonadIO m
       )
    => Metrics.Store
    -> m ()
registerEkgMetrics ekgStore = liftIO $ Metrics.registerGcMetrics ekgStore

-- | Bracket an EKG web server, so you can look at the metrics in your browser.
withEkgServer
    :: ( MonadIO m
       , MonadMask m
       )
    => EkgParams
    -> Metrics.Store
    -> m t
    -> m t
withEkgServer EkgParams {..} ekgStore act = bracket acquire release (const act)
  where
    acquire = liftIO $ Monitoring.forkServerWith ekgStore ekgHost ekgPort
>>>>>>> CHW-82-84, orphan branch
    release = stopMonitor
