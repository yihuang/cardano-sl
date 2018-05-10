{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Methods that operate on 'SscGlobalState' and 'VssCertificatesMap'.

module Pos.Ssc.State.Global
       (
       -- * Certs
         getGlobalCerts
       , getStableCerts

       -- * Global state
       , sscLoadGlobalState
       , sscGetGlobalState
       ) where

import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc ()
<<<<<<< HEAD
import           Pos.Core (EpochIndex (..), SlotId (..), VssCertificatesMap (..), HasGenesisData, HasProtocolConstants)
=======
import           Pos.Core (EpochIndex (..), HasConfiguration, SlotId (..), VssCertificatesMap (..))
>>>>>>> CHW-82-84, orphan branch
import           Pos.DB (MonadDBRead)
import           Pos.Ssc.Configuration (HasSscConfiguration)
import qualified Pos.Ssc.DB as DB
import           Pos.Ssc.Functions (getStableCertsPure)
import           Pos.Ssc.Mem (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.Types (SscGlobalState (..), sgsVssCertificates)
import qualified Pos.Ssc.VssCertData as VCD

----------------------------------------------------------------------------
-- Certs
----------------------------------------------------------------------------

getGlobalCerts
    :: (MonadSscMem ctx m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view sgsVssCertificates

-- | Get stable VSS certificates for given epoch.
getStableCerts
<<<<<<< HEAD
    :: (HasSscConfiguration, MonadSscMem ctx m, MonadIO m, HasGenesisData, HasProtocolConstants)
=======
    :: (HasSscConfiguration, HasConfiguration, MonadSscMem ctx m, MonadIO m)
>>>>>>> CHW-82-84, orphan branch
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view sgsVssCertificates)

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

-- | Load global state from DB by recreating it from recent blocks.
<<<<<<< HEAD
sscLoadGlobalState :: (MonadDBRead m, WithLogger m) => m SscGlobalState
=======
sscLoadGlobalState :: (HasConfiguration, MonadDBRead m, WithLogger m) => m SscGlobalState
>>>>>>> CHW-82-84, orphan branch
sscLoadGlobalState = do
    logDebug "Loading SSC global state"
    gs <- DB.getSscGlobalState
    gs <$ logInfo (sformat ("Loaded SSC state: " %build) gs)

sscGetGlobalState
    :: (MonadSscMem ctx m, MonadIO m)
    => m SscGlobalState
sscGetGlobalState = sscRunGlobalQuery ask
