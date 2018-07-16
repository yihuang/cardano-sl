module Cardano.Wallet.Kernel.Restore () where

import Universum

import Pos.Core (CoreConfiguration, GenesisData, GenesisHash, GeneratedSecrets,
                 BlockVersionData, ProtocolConstants)
import Pos.Crypto (PassPhrase)
import Pos.DB.Rocks.Types (NodeDBs)
import Pos.Wallet.Web.Tracking.Sync (firstGenesisHeader, processSyncError)
import System.Wlog (WithLogger, logInfo, modifyLoggerName)

import Cardano.Wallet.Kernel.Compat (runDBReadT)
import Cardano.Wallet.Kernel.DB.HdWallet (HdWallets, HdRootId)

restoreWallet
  :: (MonadCatch m, MonadIO m, WithLogger m)
  => CoreConfiguration
  -> Maybe GeneratedSecrets
  -> GenesisData
  -> GenesisHash
  -> BlockVersionData -- ^ From genesis block
  -> ProtocolConstants
  -> NodeDBs
  -> HdWallets
  -> HdRootId
  -> PassPhrase
  -> m ()
restoreWallet cc ygs gd gh bvd pc ndbs hdw hdrId passp = do
  let runDBReadT' = runDBReadT cc ygs gd gh bvd pc ndbs
  modifyLoggerName (const "syncWalletWorker") $ do
     logInfo "New Restoration request for a wallet..."
     genesisBlockHeaderE <- runDBReadT' firstGenesisHeader
     case genesisBlockHeaderE of
         Left syncError -> processSyncError syncError
         Right genesisBlock -> do undefined
--
-- -- | Restores the genesis addresses for a wallet, given its 'WalletDecrCredentials'.
-- -- NOTE: This doesn't have any effect on the balance as if these addresses still have
-- -- coins on them, this will be captured by the call to 'restoreWalletBalance', but yet
-- -- we want to add them to the pool of known addresses for history-rebuilding purposes.
-- restoreGenesisAddresses :: (HasConfiguration, MonadIO m) => HdWallets -> WalletDecrCredentials -> m ()
-- restoreGenesisAddresses hdw  =
--     let ownGenesisData =
--             selectOwnAddresses credentials (txOutAddress . toaOut . snd) $
--             M.toList $ unGenesisUtxo genesisUtxo
--         ownGenesisAddrs = map snd ownGenesisData
--     in mapM_ (WS.addWAddress db) ownGenesisAddrs
--
