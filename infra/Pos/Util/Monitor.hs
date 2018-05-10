<<<<<<< HEAD
-- | Start and stop EKG monitoring server using metrics from a 'Node' from
-- cardano-sl-networking.
=======
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE GADTs            #-}
>>>>>>> CHW-82-84, orphan branch

module Pos.Util.Monitor
    ( startMonitor
    , stopMonitor
    ) where

<<<<<<< HEAD
import           Universum

import           Control.Concurrent (killThread)
=======
import           Control.Concurrent (killThread)
import           Control.Monad.IO.Class (MonadIO)
import           Mockable.Class (Mockable)
import qualified Mockable.Metrics as Metrics
>>>>>>> CHW-82-84, orphan branch
import           Node (Node)
import           Node.Util.Monitor (registerMetrics)
import           Pos.System.Metrics.Constants (cardanoNamespace)
import qualified System.Metrics as Monitoring
<<<<<<< HEAD
import qualified System.Remote.Monitoring.Wai as Monitoring

startMonitor
    :: Int
    -> Node
    -> IO Monitoring.Server
startMonitor port node = do
    store <- Monitoring.newStore
    registerMetrics (Just cardanoNamespace) node store
    Monitoring.registerGcMetrics store
    server <- Monitoring.forkServerWith store "127.0.0.1" port
=======
import qualified System.Metrics.Distribution as Monitoring.Distribution
import qualified System.Remote.Monitoring.Wai as Monitoring

import           Universum

startMonitor
    :: ( Mockable Metrics.Metrics m
       , Metrics.Distribution m ~ Monitoring.Distribution.Distribution
       , MonadIO m
       )
    => Int
    -> (forall t . m t -> IO t)
    -> Node m
    -> m Monitoring.Server
startMonitor port lowerIO node = do
    store <- liftIO Monitoring.newStore
    registerMetrics (Just cardanoNamespace) lowerIO node store
    liftIO $ Monitoring.registerGcMetrics store
    server <- liftIO $ Monitoring.forkServerWith store "127.0.0.1" port
>>>>>>> CHW-82-84, orphan branch
    liftIO . putStrLn $ "Forked EKG server on port " ++ show port
    return server

stopMonitor
<<<<<<< HEAD
    :: Monitoring.Server
    -> IO ()
stopMonitor server = killThread (Monitoring.serverThreadId server)
=======
    :: ( MonadIO m )
    => Monitoring.Server
    -> m ()
stopMonitor server = liftIO $ killThread (Monitoring.serverThreadId server)
>>>>>>> CHW-82-84, orphan branch
