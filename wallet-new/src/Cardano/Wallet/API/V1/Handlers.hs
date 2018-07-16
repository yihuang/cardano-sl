module Cardano.Wallet.API.V1.Handlers (handlers) where

import           Servant
import           Universum

import           Cardano.Wallet.API.Response
import qualified Cardano.Wallet.API.V1 as V1
import qualified Cardano.Wallet.API.V1.Handlers.Addresses as Addresses
import qualified Cardano.Wallet.API.V1.Handlers.Transactions as Transactions
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
import           Cardano.Wallet.WalletLayer.Types
import qualified Data.IxSet.Typed as IxSet
import           Mockable

handlers :: ActiveWalletLayer Production -> Server V1.API
handlers w =  Addresses.handlers w
         :<|> wallets
         :<|> accounts
         :<|> Transactions.handlers w
         :<|> settings
         :<|> info
  where
    pw = walletPassiveLayer w

    wallets = todo
    accounts = todo
    settings = todo
    info = todo

    todo = error "TODO"
<<<<<<< 003b147119451411c6c255f2040ec1f33d0d9d9a
=======

    getTransactionsHistory mwalletId mAccIdx mAddr requestParams fops sops = do
      res <- liftIO $ runProduction $ respondWith requestParams fops sops (IxSet.fromList <$> xs)
--      print ("-------" :: String)7
--      print fops
      case res of
          Left err -> ThrowM err
          Right xs -> getTransactions pw mwalletId mAccIdx mAddr requestParams fops sops
>>>>>>> wip
