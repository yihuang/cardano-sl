{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.Faucet.Types.API (
   WithdrawlRequest(..), wAddress
 , WithdrawlResult(..)
 , DepositRequest(..), dWalletId, dAmount
 , DepositResult(..)
  ) where

import           Control.Lens hiding ((.=))
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Data.Text (Text)
-- import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Wallet.API.V1.Types (Transaction, V1 (..))
import           Cardano.Wallet.Client (ClientError (..))
import           Pos.Core (Address (..), Coin (..))


--------------------------------------------------------------------------------
-- | A request to withdraw ADA from the faucet wallet
data WithdrawlRequest = WithdrawlRequest {
    _wAddress :: !(V1 Address)
  } deriving (Show, Typeable, Generic)

makeLenses ''WithdrawlRequest

instance FromJSON WithdrawlRequest where
  parseJSON = withObject "WithdrawlRequest" $ \v -> WithdrawlRequest
    <$> v .: "address"

instance ToJSON WithdrawlRequest where
    toJSON (WithdrawlRequest w) =
        object [ "address" .= w ]

-- | The result of processing a 'WithdrawlRequest'
data WithdrawlResult =
    WithdrawlError ClientError   -- ^ Error with http client error
  | WithdrawlSuccess Transaction -- ^ Success with transaction details
  deriving (Show, Typeable, Generic)

instance ToJSON WithdrawlResult where
    toJSON (WithdrawlSuccess txn) =
        object ["success" .= txn]
    toJSON (WithdrawlError err) =
        object ["error" .= show err]


--------------------------------------------------------------------------------
-- | A request to deposit ADA back into the wallet __not currently used__
data DepositRequest = DepositRequest {
    _dWalletId :: Text
  , _dAmount   :: Coin
  } deriving (Show, Typeable, Generic)

makeLenses ''DepositRequest

instance FromJSON DepositRequest where
  parseJSON = withObject "DepositRequest" $ \v -> DepositRequest
    <$> v .: "wallet"
    <*> (Coin <$> v .: "amount")

-- | The result of processing a 'DepositRequest' __not currently used__
data DepositResult = DepositResult
  deriving (Show, Typeable, Generic)

instance ToJSON DepositResult
