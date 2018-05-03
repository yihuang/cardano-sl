{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.State
       ( mkSscState
       , module Pos.Ssc.State.Global
       , module Pos.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM

import           Pos.DB (MonadDBRead)
import           Pos.Slotting.Class (MonadSlots)
import           Pos.Ssc.Types (SscState (..))
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem)

-- Reexports
import           Pos.Ssc.State.Global
import           Pos.Ssc.State.Local

mkSscState
    :: forall ctx m .
       ( MonadReader ctx m
       , MonadDBRead m
       , MonadSlots ctx m
       )
    => Trace m LogItem
    -> m SscState
mkSscState logTrace = do
    gState <- sscLoadGlobalState logTrace
    ld <- sscNewLocalData
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
