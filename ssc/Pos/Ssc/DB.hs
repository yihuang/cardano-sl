-- | DB operations for storing and dumping SscGlobalState.

module Pos.Ssc.DB
       ( getSscGlobalState
       , sscGlobalStateToBatch
       , initSscDB
       ) where

import           Universum

import           Data.Default (def)
import qualified Data.Text.Buildable
import qualified Database.RocksDB as Rocks
import           Formatting (bprint, build, (%))

<<<<<<< HEAD
import           Pos.Core (genesisVssCerts, HasGeneratedSecrets, HasProtocolMagic, HasGenesisData, HasCoreConfiguration)
=======
import           Pos.Binary.Ssc ()
import           Pos.Core (HasConfiguration, genesisVssCerts)
>>>>>>> CHW-82-84, orphan branch
import           Pos.DB (MonadDB, MonadDBRead, RocksBatchOp (..))
import           Pos.DB.Error (DBError (DBMalformed))
import           Pos.DB.Functions (dbSerializeValue)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi)
import           Pos.Ssc.Types (SscGlobalState (..))
import qualified Pos.Ssc.VssCertData as VCD
import           Pos.Util.Util (maybeThrow)
<<<<<<< HEAD
import           Pos.Binary.Ssc.Types ()
=======
>>>>>>> CHW-82-84, orphan branch

getSscGlobalState :: (MonadDBRead m) => m SscGlobalState
getSscGlobalState =
    maybeThrow (DBMalformed "SSC global state DB is not initialized") =<<
    gsGetBi sscKey

sscGlobalStateToBatch :: SscGlobalState -> SscOp
sscGlobalStateToBatch = PutGlobalState

<<<<<<< HEAD
initSscDB :: (MonadDB m, HasGenesisData) => m ()
=======
initSscDB :: (HasConfiguration, MonadDB m) => m ()
>>>>>>> CHW-82-84, orphan branch
initSscDB = gsPutBi sscKey (def {_sgsVssCertificates = vcd})
  where
    vcd = VCD.fromList . toList $ genesisVssCerts

----------------------------------------------------------------------------
-- Operation
----------------------------------------------------------------------------

data SscOp
    = PutGlobalState !SscGlobalState

instance Buildable SscOp where
    build (PutGlobalState gs) = bprint ("SscOp ("%build%")") gs

<<<<<<< HEAD
instance (HasGeneratedSecrets, HasProtocolMagic, HasCoreConfiguration) => RocksBatchOp SscOp where
=======
instance HasConfiguration => RocksBatchOp SscOp where
>>>>>>> CHW-82-84, orphan branch
    toBatchOp (PutGlobalState gs) = [Rocks.Put sscKey (dbSerializeValue gs)]

----------------------------------------------------------------------------
-- Key
----------------------------------------------------------------------------

sscKey :: ByteString
sscKey = "ssc/"
