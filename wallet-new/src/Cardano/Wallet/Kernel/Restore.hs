module Cardano.Wallet.Kernel.Restore () where

import Universum

import Pos.Core (CoreConfiguration, GenesisData, GenesisHash, GeneratedSecrets,
                 BlockVersionData, ProtocolConstants)
import Pos.Crypto (PassPhrase)
import Pos.DB.Rocks.Types (NodeDBs)
import Pos.Wallet.Web.Tracking.Sync (firstGenesisHeader)

import Cardano.Wallet.Kernel.Compat (runDBReadT)
import Cardano.Wallet.Kernel.DB.HdWallet (HdWallets, HdRootId)

restoreWallet
  :: (MonadCatch m, MonadIO m)
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
  runDBReadT cc ygs gd gh bvd pc ndbs $ do
    genesisBlockHeaderE <- firstGenesisHeader
    undefined

