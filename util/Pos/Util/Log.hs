{-# LANGUAGE OverloadedStrings #-}

module Pos.Util.Log
       ( logInfo
       , slogInfo
       --, bracketLogging
       ) where

import           Universum

import           Data.Aeson (ToJSON(..), encode)

import qualified Katip as K


{- same for other severities
    https://github.com/Soostone/katip/blob/master/katip/src/Katip/Core.hs#L120
-}
-------------------------------------------------------------------------------
-- | log a Text with severity = Info
logInfo :: (K.KatipContext m, HasCallStack) => Text -> m ()
logInfo msg = K.logItemM Nothing K.InfoS $ K.logStr msg

-------------------------------------------------------------------------------
-- | log a JSON structure with severity = Info
slogInfo :: (ToJSON a, K.KatipContext m, HasCallStack) => a -> m ()
slogInfo json = K.logItemM Nothing K.InfoS $ K.logStr $ encode json

{-
--bracketLogging :: K.KatipContext m => Text -> IO (m ())
bracketLogging name = do
    handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout K.DebugS K.V2
    let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv (K.Namespace [name]) "production"

    bracket mkLogEnv K.closeScribes $ \le -> do
      K.runKatipContextT le () "bracket"
-}

