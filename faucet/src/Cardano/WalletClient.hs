{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.WalletClient (
    withdraw
  ) where
    -- ( export
    -- , export
    -- ) where

import           Cardano.Faucet.Types
import           Cardano.Wallet.API.V1.Types (Payment (..), PaymentDistribution (..), V1 (..))
import           Cardano.Wallet.Client (Resp, Transaction, WalletClient (..), liftClient)
import           Control.Lens
import           Crypto.Hash (Blake2b_256, Digest)
import qualified Crypto.Hash as CryptoHash
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty (..))
import           Pos.Core (Address (..), Coin (..))
import           Pos.Crypto.Signing (PassPhrase)



withdraw :: (MonadFaucet c m) => V1 Address -> V1 Coin -> Resp m Transaction
withdraw addr coin = do
    paymentSource <- view (feFaucetConfig . fcFaucetPaymentSource)
    client <- liftClient <$> view feWalletClient
    let paymentDist = (PaymentDistribution addr coin :| [])
        payment = Payment paymentSource paymentDist Nothing sp
    postTransaction client payment
  where
    sp :: Maybe (V1 PassPhrase)
    sp = Just $ V1 $ hashPwd "XXX" -- TODO: get from config

hashPwd :: ByteString -> PassPhrase
hashPwd  bs =
    let blake = CryptoHash.hash bs :: Digest (Blake2b_256)
    in BA.convert blake
