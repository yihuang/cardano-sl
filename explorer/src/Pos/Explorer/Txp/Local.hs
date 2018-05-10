{-# LANGUAGE TypeFamilies #-}

-- | Explorer's local Txp.

module Pos.Explorer.Txp.Local
       ( eTxProcessTransaction
       , eTxProcessTransactionNoLock
       , eTxNormalize
       ) where

<<<<<<< HEAD
import           JsonLog (CanJsonLog (..))
=======
>>>>>>> CHW-82-84, orphan branch
import           Universum

import qualified Data.HashMap.Strict as HM

<<<<<<< HEAD
import           Pos.Core (BlockVersionData, EpochIndex, HasConfiguration, Timestamp)
=======
import           Pos.Core (BlockVersionData, EpochIndex, Timestamp)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Slotting (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import           Pos.Txp.Logic.Local (txNormalizeAbstract, txProcessTransactionAbstract)
import           Pos.Txp.MemState (MempoolExt, TxpLocalWorkMode, getTxpExtra, withTxpLocalData)
import           Pos.Txp.Toil (ToilVerFailure (..), Utxo)
<<<<<<< HEAD
import           Pos.Util.JsonLog.Events (MemPoolModifyReason (..))
=======
>>>>>>> CHW-82-84, orphan branch
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (HasLens')

import           Pos.Explorer.Core (TxExtra (..))
import           Pos.Explorer.Txp.Common (buildExplorerExtraLookup)
import           Pos.Explorer.Txp.Toil (ELocalToilM, ExplorerExtraLookup (..),
                                        ExplorerExtraModifier, eNormalizeToil, eProcessTx,
                                        eemLocalTxsExtra)


type ETxpLocalWorkMode ctx m =
    ( TxpLocalWorkMode ctx m
    , MempoolExt m ~ ExplorerExtraModifier
    )

eTxProcessTransaction ::
       ( ETxpLocalWorkMode ctx m
       , HasLens' ctx StateLock
<<<<<<< HEAD
       , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
       , CanJsonLog m
       , HasConfiguration
=======
       , HasLens' ctx StateLockMetrics
>>>>>>> CHW-82-84, orphan branch
       )
    => (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransaction itw =
<<<<<<< HEAD
    withStateLock LowPriority ProcessTransaction $ \__tip -> eTxProcessTransactionNoLock itw

eTxProcessTransactionNoLock ::
       forall ctx m. (ETxpLocalWorkMode ctx m, HasConfiguration)
=======
    withStateLock LowPriority "eTxProcessTransaction" $ \__tip -> eTxProcessTransactionNoLock itw

eTxProcessTransactionNoLock ::
       forall ctx m. (ETxpLocalWorkMode ctx m)
>>>>>>> CHW-82-84, orphan branch
    => (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock itw = getCurrentSlot >>= \case
    Nothing   -> pure $ Left ToilSlotUnknown
    Just slot -> do
        -- First get the current @SlotId@ so we can calculate the time.
        -- Then get when that @SlotId@ started and use that as a time for @Tx@.
        mTxTimestamp <- getSlotStart slot
        txProcessTransactionAbstract buildContext (processTx' mTxTimestamp) itw
  where
    buildContext :: Utxo -> TxAux -> m ExplorerExtraLookup
    buildContext utxo = buildExplorerExtraLookup utxo . one

    processTx' ::
           Maybe Timestamp
        -> BlockVersionData
        -> EpochIndex
        -> (TxId, TxAux)
        -> ExceptT ToilVerFailure ELocalToilM ()
    processTx' mTxTimestamp bvd epoch tx =
        eProcessTx bvd epoch tx (TxExtra Nothing mTxTimestamp)

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
<<<<<<< HEAD
       forall ctx m. (ETxpLocalWorkMode ctx m, HasConfiguration)
=======
       forall ctx m. (ETxpLocalWorkMode ctx m)
>>>>>>> CHW-82-84, orphan branch
    => m ()
eTxNormalize = do
    extras <- MM.insertionsMap . view eemLocalTxsExtra <$> withTxpLocalData getTxpExtra
    txNormalizeAbstract buildExplorerExtraLookup (normalizeToil' extras)
  where
    normalizeToil' ::
           HashMap TxId TxExtra
        -> BlockVersionData
        -> EpochIndex
        -> HashMap TxId TxAux
        -> ELocalToilM ()
    normalizeToil' extras bvd epoch txs =
        let toNormalize = HM.toList $ HM.intersectionWith (,) txs extras
        in eNormalizeToil bvd epoch toNormalize
