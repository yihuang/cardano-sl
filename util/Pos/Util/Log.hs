{-# LANGUAGE OverloadedStrings #-}

module Pos.Util.Log
       ( logInfo
       , slogInfo
       ) where

import           Universum

import           Data.Aeson                  (ToJSON(..), encode)

import qualified Katip                       as K


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


