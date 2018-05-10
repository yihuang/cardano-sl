{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Universum

import           Pos.Block.Worker (blkWorkers)
import           Pos.Communication (OutSpecs)
<<<<<<< HEAD
=======
import           Pos.Communication.Util (wrapActionSpec)
>>>>>>> CHW-82-84, orphan branch
-- Message instances.
import           Pos.Communication.Message ()
import           Pos.Context (NodeContext (..))
import           Pos.Delegation.Worker (dlgWorkers)
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Network.CLI (launchStaticConfigMonitoring)
import           Pos.Network.Types (NetworkConfig (..))
import           Pos.Slotting (logNewSlotWorker)
import           Pos.Ssc.Worker (sscWorkers)
import           Pos.Update.Worker (usWorkers)
import           Pos.Util (mconcatPair)
import           Pos.Worker.Types (WorkerSpec, localWorker)
import           Pos.WorkMode (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ext ctx m .
       WorkMode ctx m
    => NodeResources ext -> ([WorkerSpec m], OutSpecs)
allWorkers NodeResources {..} = mconcatPair
<<<<<<< HEAD
    [ -- Only workers of "onNewSlot" type
      -- I have no idea what this ↑ comment means (@gromak).
      sscWorkers
    , usWorkers
      -- Have custom loggers
    , blkWorkers
    , dlgWorkers
    , (properSlottingWorkers, mempty)
    , first one $ localWorker $ launchStaticConfigMonitoring topology
=======
    [
      -- Only workers of "onNewSlot" type
      -- I have no idea what this ↑ comment means (@gromak).

      wrap' "ssc"        $ sscWorkers
    , wrap' "us"         $ usWorkers

      -- Have custom loggers
    , wrap' "block"      $ blkWorkers
    , wrap' "delegation" $ dlgWorkers
    , wrap' "slotting"   $ (properSlottingWorkers, mempty)
    , wrap' "StaticConfigMonitoring" $
      first one $
      localWorker $
      launchStaticConfigMonitoring topology
>>>>>>> CHW-82-84, orphan branch
    ]
  where
    topology = ncTopology ncNetworkConfig
    NodeContext {..} = nrContext
    properSlottingWorkers = [fst (localWorker logNewSlotWorker)]
<<<<<<< HEAD
=======
    wrap' lname = first (map $ wrapActionSpec $ "worker" <> lname)
>>>>>>> CHW-82-84, orphan branch
