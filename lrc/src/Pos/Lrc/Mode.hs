{-# LANGUAGE DataKinds #-}

-- | Constraints for LRC; a restricted version of `WorkMode`.

module Pos.Lrc.Mode
       ( LrcMode
       ) where

import           Universum

import           UnliftIO (MonadUnliftIO)

import           Pos.Core.Mockable (Async, Concurrently, Delay, Mockables)
import           Pos.DB.Class (MonadDB, MonadGState)
import           Pos.Lrc.Context (HasLrcContext)

-- | Set of constraints used by LRC.
type LrcMode ctx m
     = ( MonadMask m
       , MonadGState m
       , MonadDB m
       , MonadIO m
       , MonadUnliftIO m
       , Mockables m [Async, Concurrently, Delay]
       , MonadReader ctx m
       , HasLrcContext ctx
       )
