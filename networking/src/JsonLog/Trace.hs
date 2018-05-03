{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : JsonLog.JsonLogT
Description : Trace for JSON logging
License:      MIT
Maintainer:   lars.bruenjes@iohk.io
Stability:    experimental
Portability:  GHC

This module provides the monad transformer @'JsonLogT'@
for adding JSON logging to a monad transformer stack.
-}

module JsonLog.Trace
    ( JsonLogConfig(..)
    , jsonLogIO
    , jsonLogTrace
    , newJsonLogConfig
    ) where

import           Control.Concurrent.MVar (MVar, withMVar)
import           Data.Aeson (ToJSON, encode)
import           Data.ByteString.Lazy (hPut)
import           Data.Functor.Contravariant (Op (..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.IO (Handle)
import           Universum

import           JsonLog.Event (JLTimedEvent, JLTimed (..), toEvent)

import           Pos.Util.Trace (Trace (..))

data JsonLogConfig
    = JsonLogDisabled
    | JsonLogConfig (MVar Handle) (JLTimedEvent -> IO Bool)

-- | Default JSON logger according to a configuration.
-- In case it's enabled and writes to some handle, it does no exception
-- handling.
jsonLogTrace
    :: ( ToJSON a )
    => JsonLogConfig
    -> Trace IO a
jsonLogTrace jlc = case jlc of
    JsonLogDisabled -> Trace $ Op $ const (pure ())
    JsonLogConfig v decide -> Trace $ Op $ \x -> do
        timeNow <- round . (* 1000000) <$> getPOSIXTime
        let event = toEvent (JLTimed timeNow x)
        shouldLog <- decide event
        when shouldLog $ (withMVar v $ flip hPut $ encode event)

-- | Runs a computation containing JSON log messages,
-- either discarding all messages or writing
-- some of them to a handle.
newJsonLogConfig
    :: Maybe (Handle, JLTimedEvent -> IO Bool) -- ^ If @'Nothing'@, JSON log messages are discarded, if @'Just' (h, f)@,
                                               -- log messages @e@ are written to handle @h@ if @f e@ returns @True@,
                                               -- and are otherwise discarded.
    -> IO JsonLogConfig
newJsonLogConfig Nothing            = pure JsonLogDisabled
newJsonLogConfig (Just (h, decide)) = do
    v <- newMVar h
    pure $ JsonLogConfig v decide

jsonLogIO :: ToJSON a => Maybe (Handle, JLTimedEvent -> IO Bool) -> IO (Trace IO a)
jsonLogIO = fmap jsonLogTrace . newJsonLogConfig
