-- | Higher-level DB functionality.

module Pos.DB.DB
       ( initNodeDBs
       , gsAdoptedBVDataDefault
       ) where

import           Universum

import           Pos.Core as Core (BlockVersionData, Config (..), headerHash,
                     pcEpochSlots)
import           Pos.Core.Block.Constructors (genesisBlock0)
import           Pos.DB.Block (prepareBlockDB)
import           Pos.DB.Class (MonadDB, MonadDBRead (..))
import           Pos.GState.GState (prepareGStateDB)
import           Pos.Lrc.DB (prepareLrcDB)
import           Pos.Lrc.Genesis (genesisLeaders)
import           Pos.Update.DB (getAdoptedBVData)

-- | Initialize DBs if necessary.
initNodeDBs
    :: forall ctx m
     . (MonadReader ctx m, MonadDB m)
    => Core.Config
    -> m ()
initNodeDBs (Config pm pc genesisHash) = do
    let initialTip = headerHash gb
    prepareBlockDB gb
    prepareGStateDB pc genesisHash initialTip
    prepareLrcDB epochSlots
  where
    epochSlots = pcEpochSlots pc
    gb = genesisBlock0 pm genesisHash (genesisLeaders epochSlots)

----------------------------------------------------------------------------
-- MonadGState instance
----------------------------------------------------------------------------

gsAdoptedBVDataDefault :: MonadDBRead m => m BlockVersionData
gsAdoptedBVDataDefault = getAdoptedBVData
