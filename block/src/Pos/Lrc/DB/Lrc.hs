-- | Higher-level functionality of LRC DB.

module Pos.Lrc.DB.Lrc
       ( prepareLrcDB
       ) where

import           Universum

<<<<<<< HEAD
import           Pos.Core (HasProtocolMagic, HasGenesisBlockVersionData, HasGeneratedSecrets,
                  HasProtocolConstants, HasGenesisData)
=======
import           Pos.Core (HasConfiguration)
>>>>>>> CHW-82-84, orphan branch
import           Pos.DB.Class (MonadDB)
import           Pos.DB.Error (DBError (..))
import           Pos.Lrc.DB.Common (prepareLrcCommon)
import           Pos.Lrc.DB.Issuers (prepareLrcIssuers)
import           Pos.Lrc.DB.Leaders (prepareLrcLeaders)
import           Pos.Lrc.DB.Richmen (prepareLrcRichmen, tryGetUSRichmen)
import           Pos.Lrc.DB.Seed (prepareLrcSeed)

import           Pos.Util (maybeThrow)

-- | Put missing initial data into LRC DB.
prepareLrcDB
    :: ( MonadDB m
<<<<<<< HEAD
       , HasProtocolMagic
       , HasGenesisBlockVersionData
       , HasGeneratedSecrets
       , HasProtocolConstants
       , HasGenesisData
=======
       , HasConfiguration
>>>>>>> CHW-82-84, orphan branch
       )
    => m ()
prepareLrcDB = do
    prepareLrcLeaders
    prepareLrcRichmen
    let cantReadErr =
            DBMalformed "Can't read richmen US after richmen initialization"
    totalStake <- fst <$> (maybeThrow cantReadErr =<< tryGetUSRichmen 0)
    prepareLrcIssuers totalStake
    prepareLrcSeed
    prepareLrcCommon
