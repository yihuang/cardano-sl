{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
<<<<<<< HEAD
=======
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
>>>>>>> CHW-82-84, orphan branch

module Node.Util.Monitor (
      registerMetrics
    ) where

<<<<<<< HEAD
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Node
import qualified System.Metrics as Monitoring
import qualified System.Metrics.Gauge as Gauge
=======
import           Control.Monad.IO.Class
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Mockable.Class
import qualified Mockable.Metrics as Metrics
import           Node
import qualified System.Metrics as Monitoring
>>>>>>> CHW-82-84, orphan branch
import qualified System.Metrics.Distribution as Monitoring.Distribution

-- | Put time-warp related metrics into an EKG store.
--   You must indicate how to run the monad into IO, so that EKG can produce
--   the metrics (it works in IO).
registerMetrics
<<<<<<< HEAD
    :: Maybe T.Text
    -> Node
    -> Monitoring.Store
    -> IO ()
registerMetrics mbNamespace node store = do
    flip (Monitoring.registerGauge (withNamespace "handlers.initiated_remotely")) store $ do
        stats <- nodeStatistics node
        Gauge.read (stRunningHandlersRemote stats)
    flip (Monitoring.registerGauge (withNamespace "handlers.initiated_locally")) store $ do
        stats <- nodeStatistics node
        Gauge.read (stRunningHandlersLocal stats)
    flip (Monitoring.registerDistribution (withNamespace "handlers.finished_normally.elapsed_time_microseconds")) store $ do
        stats <- nodeStatistics node
        Monitoring.Distribution.read (stHandlersFinishedNormally stats)
    flip (Monitoring.registerDistribution (withNamespace "handlers.finished_exceptionally.elapsed_time_microseconds")) store $ do
        stats <- nodeStatistics node
        Monitoring.Distribution.read (stHandlersFinishedExceptionally stats)
=======
    :: ( Mockable Metrics.Metrics m
       , Metrics.Distribution m ~ Monitoring.Distribution.Distribution
       , MonadIO m
       )
    => Maybe T.Text
    -> (forall t . m t -> IO t)
    -> Node m
    -> Monitoring.Store
    -> m ()
registerMetrics mbNamespace lowerIO node store = do
    liftIO $ flip (Monitoring.registerGauge (withNamespace "handlers.initiated_remotely")) store $ lowerIO $ do
        stats <- nodeStatistics node
        Metrics.readGauge (stRunningHandlersRemote stats)
    liftIO $ flip (Monitoring.registerGauge (withNamespace "handlers.initiated_locally")) store $ lowerIO $ do
        stats <- nodeStatistics node
        Metrics.readGauge (stRunningHandlersLocal stats)
    liftIO $ flip (Monitoring.registerDistribution (withNamespace "handlers.finished_normally.elapsed_time_microseconds")) store $ lowerIO $ do
        stats <- nodeStatistics node
        liftIO $ Monitoring.Distribution.read (stHandlersFinishedNormally stats)
    liftIO $ flip (Monitoring.registerDistribution (withNamespace "handlers.finished_exceptionally.elapsed_time_microseconds")) store $ lowerIO $ do
        stats <- nodeStatistics node
        liftIO $ Monitoring.Distribution.read (stHandlersFinishedExceptionally stats)
>>>>>>> CHW-82-84, orphan branch
  where
      withNamespace :: T.Text -> T.Text
      withNamespace name = case mbNamespace of
          Nothing -> name
          Just ns -> ns <> "." <> name
