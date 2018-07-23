{-# LANGUAGE RankNTypes #-}
module Cardano.Wallet.Kernel.Restore
 ( restoreWallet
 ) where

import Universum

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Acid (AcidState)
import Data.Acid.Advanced (update')
import qualified Data.Map as M
import System.Wlog (WithLogger, logInfo, modifyLoggerName)

import Pos.Core (CoreConfiguration, GenesisData, GenesisHash, GeneratedSecrets,
                 BlockVersionData, ProtocolConstants, toaOut, txOutAddress,
                 TxOutAux, TxIn)
import Pos.Crypto (PassPhrase)
import Pos.DB.Class (MonadDBRead)
import Pos.DB.Rocks.Types (NodeDBs)
import Pos.Txp.DB.Utxo (filterUtxo)
import Pos.Txp.Toil.Types (GenesisUtxo, unGenesisUtxo, Utxo, utxoToModifier)
import Pos.Wallet.Web.Tracking.Decrypt (decryptAddress, WalletDecrCredentials)
import Pos.Wallet.Web.Tracking.Sync (firstGenesisHeader, processSyncError)

import Cardano.Wallet.Kernel.Compat (DBReadT, withMonadDBRead)
import Cardano.Wallet.Kernel.PrefilterTx (WalletKey, prefilter, toHdAddressId)
import Cardano.Wallet.Kernel.DB.AcidState (CreateHdAddress(..), UpdateCurrentCheckpointUtxo(..), DB)
import Cardano.Wallet.Kernel.DB.HdWallet (HdWallets, HdRootId)
import Cardano.Wallet.Kernel.DB.HdWallet.Create
  (initHdAddress, CreateHdAddressError)
import Cardano.Wallet.Kernel.DB.InDb (InDb(..))

restoreWallet
  :: forall m
  .  (WithLogger m, MonadUnliftIO m, MonadCatch m)
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
  let withMDBR :: (forall n. (MonadUnliftIO n, MonadDBRead n) => n x) -> m x
      withMDBR = withMonadDBRead cc ygs gd gh bvd pc ndbs
  modifyLoggerName (const "syncWalletWorker") $ do
     logInfo "New Restoration request for a wallet..."
     genesisBlockHeaderE <- withMDBR firstGenesisHeader
     case genesisBlockHeaderE of
         Left syncError -> processSyncError syncError
         Right genesisBlock -> do
            -- TODO error handling
            _ <- liftIO (restoreGenesisAddresses gu db wkey)
            _ <- withMDBR (restoreWalletBalance db wkey)
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
  :: (MonadDBRead m, MonadUnliftIO m)
  => AcidState DB
  -> WalletKey
  -> m ()
restoreWalletBalance db (wId, wdc) = do
    utxo <- filterUtxo isWalletUtxo
    for_ (M.elems utxo) $ \toa -> do
       let a = txOutAddress (toaOut toa)
       case decryptAddress wdc a of
          Nothing -> error "TODO"
          Just wam -> do
             let aId = toHdAddressId wId wam
             liftIO (update' db (CreateHdAddress (initHdAddress aId (InDb a))))
    liftIO (update' db (UpdateCurrentCheckpointUtxo (utxoToModifier utxo)))
  where
    isWalletUtxo :: (TxIn, TxOutAux) -> Bool
    isWalletUtxo (_, toa) =
      isJust (decryptAddress wdc (txOutAddress (toaOut toa)))
