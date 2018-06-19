{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.Faucet (
    faucetServer
  , faucetServerAPI
  , module Cardano.Faucet.Types
  , module Cardano.Faucet.Init
  ) where

import           Control.Lens
import           Data.Monoid ((<>))
import           Data.Text.Lens
import           Servant
import           System.Wlog (LoggerName (..), logError, logInfo, withSublogger)

import           Cardano.Faucet.Init
import           Cardano.Faucet.Types
import           Cardano.Wallet.API.Response (WalletResponse (..))
import           Cardano.Wallet.API.V1.Types (txAmount, unV1)
import qualified Cardano.WalletClient as Client

-- | Top level type of the faucet API
type API = "withdraw" :> ReqBody '[JSON] WithDrawlRequest :> Post '[JSON] WithDrawlResult
      -- :<|> "_deposit" :> ReqBody '[JSON] DepositRequest :> Post '[JSON] DepositResult

-- | Handler for the withdrawl of ADA from the faucet
withdraw :: (MonadFaucet c m) => WithDrawlRequest -> m WithDrawlResult
withdraw wd = withSublogger (LoggerName "withdraw") $ do
    resp <- Client.withdraw (wd ^. wAddress)
    case resp of
        Left err -> do
            logError ("Error withdrawing " <> (wd ^. to show . packed)
                                           <> " error: "
                                           <> (err ^. to show . packed))
            return $ WithdrawlError err
        Right wr -> do
            let txn = wrData wr
                amount = unV1 $ txAmount txn
            logInfo ((wd ^. to show . packed) <> " withdrew: "
                                              <> (amount ^. to show . packed))
            incWithDrawn amount
            return $ WithdrawlSuccess txn
-- | Function to _deposit funds back into the faucet /not implemented/
_deposit :: (MonadFaucet c m) => DepositRequest -> m DepositResult
_deposit dr = withSublogger (LoggerName "_deposit") $ do
    -- decrWithDrawn (dr ^. dAmount)
    logInfo ((dr ^. to show . packed) <> " deposited")
    return DepositResult

faucetServer :: ServerT API M
faucetServer = withdraw -- :<|> _deposit

faucetServerAPI :: Proxy API
faucetServerAPI = Proxy
