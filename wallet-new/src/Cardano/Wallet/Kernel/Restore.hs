{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Kernel.Restore () where

import Universum

import Data.Acid (AcidState)
import Data.Acid.Advanced (update')
import qualified Data.Map as M
import Pos.Core (CoreConfiguration, GenesisData, GenesisHash, GeneratedSecrets,
                 BlockVersionData, ProtocolConstants, toaOut, txOutAddress,
                 TxOutAux, TxIn)
import Pos.Crypto (PassPhrase)
import Pos.DB.Class (MonadDBRead)
import Pos.DB.Rocks.Types (NodeDBs)
import Pos.Txp.DB.Utxo (filterUtxo)
import Pos.Txp.Toil.Types (GenesisUtxo, unGenesisUtxo)
import Pos.Wallet.Web.Tracking.Decrypt (decryptAddress, WalletDecrCredentials)
import Pos.Wallet.Web.Tracking.Sync (firstGenesisHeader, processSyncError)
import System.Wlog (WithLogger, logInfo, modifyLoggerName)

import Cardano.Wallet.Kernel.Compat (DBReadT, runDBReadT)
import Cardano.Wallet.Kernel.PrefilterTx (WalletKey, prefilter, toHdAddressId)
import Cardano.Wallet.Kernel.DB.AcidState (CreateHdAddress(..), DB)
import Cardano.Wallet.Kernel.DB.HdWallet (HdWallets, HdRootId)
import Cardano.Wallet.Kernel.DB.HdWallet.Create
  (initHdAddress, CreateHdAddressError)
import Cardano.Wallet.Kernel.DB.InDb (InDb(..))

restoreWallet
  :: (MonadCatch m, MonadIO m, WithLogger m)
  => CoreConfiguration
  -> Maybe GeneratedSecrets
  -> GenesisData
  -> GenesisHash
  -> GenesisUtxo
  -> BlockVersionData -- ^ From genesis block
  -> ProtocolConstants
  -> NodeDBs
  -> AcidState DB
  -> WalletKey
  -> m ()
restoreWallet cc ygs gd gh gu bvd pc ndbs db wkey = do
  let runDBReadT' = runDBReadT cc ygs gd gh bvd pc ndbs
  modifyLoggerName (const "syncWalletWorker") $ do
     logInfo "New Restoration request for a wallet..."
     genesisBlockHeaderE <- runDBReadT' firstGenesisHeader
     case genesisBlockHeaderE of
         Left syncError -> processSyncError syncError
         Right genesisBlock -> do
            -- TODO error handling
            _ <- liftIO (restoreGenesisAddresses gu db wkey)
            undefined

restoreGenesisAddresses
  :: GenesisUtxo -> AcidState DB -> WalletKey
  -> IO (Either CreateHdAddressError ())
restoreGenesisAddresses gu db wkey = do
    let xs = map (\(a, b) -> (a, txOutAddress (toaOut b)))
                 (M.toList (unGenesisUtxo gu))
    runExceptT $ for_ (prefilter wkey snd xs) $ \((_,a),aId) -> do
        ExceptT $ update' db (CreateHdAddress (initHdAddress aId (InDb a)))

restoreWalletBalance
  :: (forall a. MonadDBRead (DBReadT IO) => DBReadT IO a -> IO a)
  -> AcidState DB
  -> WalletKey
  -> IO ()
restoreWalletBalance runDBReadT' db (wId, wdc) = do
    utxo <- runDBReadT' (filterUtxo isWalletUtxo)
    for_ (M.elems utxo) $ \toa -> do
       let a = txOutAddress (toaOut toa)
       case decryptAddress wdc a of
          Just wam -> do
             let aId = toHdAddressId wId wam
             update' db (CreateHdAddress (initHdAddress aId (InDb a)))
    -- updateWalletBalancesAndUtxo db (utxoToModifier utxo)
    where
      isWalletUtxo :: (TxIn, TxOutAux) -> Bool
      isWalletUtxo (_, toa) =
        isJust (decryptAddress wdc (txOutAddress (toaOut toa)))
