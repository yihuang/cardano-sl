{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.WalletClient (
    withdraw
  , createWallet
  ) where
    -- ( export
    -- , export
    -- ) where

import           Cardano.Faucet.Types
import           Cardano.Wallet.API.V1.Types (AssuranceLevel (NormalAssurance), NewWallet (..),
                                              Payment (..), PaymentDistribution (..), V1 (..),
                                              Wallet (..), WalletOperation (CreateWallet))
import           Cardano.Wallet.Client (Resp, Transaction, WalletClient (..), liftClient)
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Crypto.Hash (Blake2b_256, Digest)
import qualified Crypto.Hash as CryptoHash
import           Crypto.Random.Entropy (getEntropy)
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Strict.Lens (utf8)
import           Pos.Util.BackupPhrase (BackupPhrase (..))
import           Pos.Util.Mnemonics (toMnemonic)
import           System.Random

import           Pos.Core (Address (..), Coin (..))
import           Pos.Crypto.Signing (PassPhrase)


randomAmount :: (MonadFaucet c m) => m (V1 Coin)
randomAmount = do
    amt <- fromIntegral <$> view (feFaucetConfig . fcPaymentAmount . _Wrapped')
    var <- view (feFaucetConfig . fcPaymentVariation . _Wrapped')
    f <- liftIO $ randomRIO ((-1), 1)
    return $ V1 $ Coin $ round (amt + (var * f))

withdraw :: (MonadFaucet c m) => V1 Address -> Resp m Transaction
withdraw addr = do
    paymentSource <- view (fePaymentSource)
    spendingPassword <- view (feSpendingPassword . re utf8)
    coin <- randomAmount
    client <- liftClient <$> view feWalletClient
    let paymentDist = (PaymentDistribution addr coin :| [])
        sp = Just $ V1 $ hashPwd spendingPassword
        payment = Payment paymentSource paymentDist Nothing sp
    postTransaction client payment

createWallet :: (MonadFaucet c m) => Resp m (BackupPhrase, Wallet)
createWallet = do
    phrase <- liftIO generateBackupPhrase
    let w = NewWallet (V1 phrase) Nothing NormalAssurance "Faucet-Wallet" CreateWallet
    client <- liftClient <$> view feWalletClient
    fmap (fmap (phrase,)) <$> postWallet client w

hashPwd :: ByteString -> PassPhrase
hashPwd  bs =
    let blake = CryptoHash.hash bs :: Digest (Blake2b_256)
    in BA.convert blake

generateBackupPhrase :: IO BackupPhrase
generateBackupPhrase = do
    -- The size 16 should give us 12-words mnemonic after BIP-39 encoding.
    genMnemonic <- getEntropy 16
    let newMnemonic = either (error . show) id $ toMnemonic genMnemonic
    return $ mkBackupPhrase12 $ Text.words newMnemonic
  where
    mkBackupPhrase12 :: [Text] -> BackupPhrase
    mkBackupPhrase12 ls
        | length ls == 12 = BackupPhrase ls
        | otherwise = error "Invalid number of words in backup phrase! Expected 12 words."
