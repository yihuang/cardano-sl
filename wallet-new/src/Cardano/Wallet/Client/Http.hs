module Cardano.Wallet.Client.Http
    ( module Cardano.Wallet.Client.Http
      -- * Abstract Client export
    , module Cardano.Wallet.Client
    -- * Servant Client Export
    , module Servant.Client
    , module Network.HTTP.Client
    ) where

import           Universum

import           Control.Lens (_Left)
<<<<<<< HEAD
import           Data.Aeson (decode)
import           Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import           Servant ((:<|>) (..), (:>))
import           Servant.Client (BaseUrl (..), ClientEnv (..), Scheme (..), client, runClientM, ServantError(..))
=======
import           Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import           Servant ((:<|>) (..), (:>))
import           Servant.Client (BaseUrl (..), ClientEnv (..), Scheme (..), client, runClientM)
>>>>>>> CHW-82-84, orphan branch

import qualified Cardano.Wallet.API.V1 as V1
import           Cardano.Wallet.Client

-- | Given a 'BaseUrl' and an @http-client@ 'Manager', this returns
-- a 'WalletClient' that operates in 'IO'.
mkHttpClient
    :: BaseUrl
    -> Manager
    -> WalletClient IO
mkHttpClient baseUrl manager = WalletClient
    { getAddressIndexPaginated
        = \x -> run . getAddressIndexR x
    , postAddress
        = run . postAddressR
    , getAddress
        = run . getAddressR
<<<<<<< HEAD
=======
    , postExternalAddress
        = run . postExternalAddressR
>>>>>>> CHW-82-84, orphan branch
    -- wallets endpoints
    , postWallet
        = run . postWalletR
    , getWalletIndexFilterSorts
        = \mp mpp filters sorts -> run $
            getWalletIndexFilterSortsR mp mpp filters sorts
    , updateWalletPassword
        = \x -> run . updateWalletPasswordR x
    , deleteWallet
        = unNoContent . run . deleteWalletR
    , getWallet
        = run . getWalletR
    , updateWallet
        = \x -> run . updateWalletR x
<<<<<<< HEAD
=======
    , postExternalWallet
        = run . postExternalWalletR
    , postAddressPath
        = run . postAddressPathR
>>>>>>> CHW-82-84, orphan branch
    -- account endpoints
    , deleteAccount
        = \x -> unNoContent . run . deleteAccountR x
    , getAccount
        = \x -> run . getAccountR x
    , getAccountIndexPaged
        = \x y -> run . getAccountIndexPagedR x y
    , postAccount
        = \w -> run . postAccountR w
    , updateAccount
        = \x y -> run . updateAccountR x y
<<<<<<< HEAD
=======
    , postExternalAccount
        = \w -> run . postExternalAccountR w
>>>>>>> CHW-82-84, orphan branch
    -- transactions endpoints
    , postTransaction
        = run . postTransactionR
    , getTransactionIndexFilterSorts
        = \walletId mAccountIndex mAddress mPage mpp filters ->
             run . getTransactionIndexFilterSortsR walletId mAccountIndex mAddress mPage mpp filters
    , getTransactionFee
        = run . getTransactionFeeR
<<<<<<< HEAD
=======
    , postUnsignedTransaction
        = run . postUnsignedTransactionR
    , postSignedTransaction
        = run . postSignedTransactionR
>>>>>>> CHW-82-84, orphan branch
    -- settings
    , getNodeSettings
        = run getNodeSettingsR
    -- info
    , getNodeInfo
        = run getNodeInfoR
    }

  where
    unNoContent = map void
    clientEnv = ClientEnv manager baseUrl
<<<<<<< HEAD
    parseJsendError servantErr =
        case servantErr of
            FailureResponse resp ->
                case decode (responseBody resp) of
                    Just err -> ClientWalletError err
                    Nothing  -> ClientHttpError servantErr
            _ -> ClientHttpError servantErr
    run       = fmap (over _Left parseJsendError) . (`runClientM` clientEnv)
    getAddressIndexR
        :<|> postAddressR
        :<|> getAddressR
=======
    run       = fmap (over _Left ClientHttpError) . (`runClientM` clientEnv)
    getAddressIndexR
        :<|> postAddressR
        :<|> getAddressR
        :<|> postExternalAddressR
>>>>>>> CHW-82-84, orphan branch
        = addressesAPI

    postWalletR
        :<|> getWalletIndexFilterSortsR
        :<|> updateWalletPasswordR
        :<|> deleteWalletR
        :<|> getWalletR
        :<|> updateWalletR
<<<<<<< HEAD
=======
        :<|> postExternalWalletR
        :<|> postAddressPathR
>>>>>>> CHW-82-84, orphan branch
        = walletsAPI

    deleteAccountR
        :<|> getAccountR
        :<|> getAccountIndexPagedR
        :<|> postAccountR
        :<|> updateAccountR
<<<<<<< HEAD
=======
        :<|> postExternalAccountR
>>>>>>> CHW-82-84, orphan branch
        = accountsAPI

    postTransactionR
        :<|> getTransactionIndexFilterSortsR
        :<|> getTransactionFeeR
<<<<<<< HEAD
=======
        :<|> postUnsignedTransactionR
        :<|> postSignedTransactionR
>>>>>>> CHW-82-84, orphan branch
        = transactionsAPI

    addressesAPI
        :<|> walletsAPI
        :<|> accountsAPI
        :<|> transactionsAPI
        :<|> getNodeSettingsR
        :<|> getNodeInfoR
        = client (Proxy @("api" :> "v1" :> V1.API))
