-- | Functions for sanity checking the GState DB.

module Pos.GState.SanityCheck
       ( sanityCheckDB
       ) where

import           Universum

import           UnliftIO (MonadUnliftIO)

import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.GState.Stakes (getRealTotalStake)
import           Pos.Txp.DB (sanityCheckStakes, sanityCheckUtxo)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem)

sanityCheckDB ::
       ( MonadMask m
       , MonadDBRead m
       , MonadUnliftIO m
       )
    => Trace m LogItem
    -> m ()
sanityCheckDB logTrace = inAssertMode (sanityCheckGStateDB logTrace)

-- | Check that GState DB is consistent.
sanityCheckGStateDB ::
       forall m.
       ( MonadDBRead m
       , MonadUnliftIO m
       )
    => Trace m LogItem
    -> m ()
sanityCheckGStateDB logTrace = do
    sanityCheckStakes logTrace
    sanityCheckUtxo logTrace =<< getRealTotalStake
