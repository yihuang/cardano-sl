module Pos.Shutdown.Logic
       ( triggerShutdown
       , waitForShutdown
       ) where

import           Universum

import           Control.Concurrent.STM (check, readTVar, writeTVar)

import           Pos.Shutdown.Class (HasShutdownContext (..))
import           Pos.Shutdown.Types (ShutdownContext (..), shdnIsTriggered)
import           Pos.Util.Trace (Trace, traceWith)
import           Pos.Util.Trace.Unstructured (Severity (Info))

triggerShutdown
    :: (MonadIO m, MonadReader ctx m, HasShutdownContext ctx)
    => Trace m (Severity, Text)
    -> m ()
triggerShutdown logTrace = do
    traceWith logTrace (Info, "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE")
    view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar True

-- | Wait for the shutdown var to be true.
waitForShutdown :: ShutdownContext -> IO ()
waitForShutdown (ShutdownContext v) = atomically (readTVar v >>= check)
