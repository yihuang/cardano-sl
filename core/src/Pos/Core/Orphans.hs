{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for core

module Pos.Core.Orphans
       (
       ) where

import           Data.Aeson.TH (deriveJSON)
import           Data.Time.Units (Microsecond, Millisecond, Second)
import qualified Serokell.Aeson.Options as S (defaultOptions)

deriveJSON S.defaultOptions ''Millisecond
deriveJSON S.defaultOptions ''Microsecond
deriveJSON S.defaultOptions ''Second
