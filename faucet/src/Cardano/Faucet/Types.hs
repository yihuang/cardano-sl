{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.Faucet.Types (
   FaucetConfig(..), mkFaucetConfig
 , HasFaucetConfig(..)
 , FaucetEnv(..), initEnv
 , HasFaucetEnv(..)
 , incWithDrawn
 , decrWithDrawn
 , setWalletBalance
 , WithDrawlRequest(..), wAddress, wAmount
 , WithDrawlResult(..)
 , DepositRequest(..), dWalletId, dAmount
 , DepositResult(..)
 , M, runM
 , MonadFaucet
  ) where

import           Control.Lens hiding ((.=))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, object, withObject, (.:),
                             (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Text.Lens (packed)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientParams (..), credentialLoadX509FromMemory, defaultParamsClient,
                              onCertificateRequest, onServerCertificate, supportedCiphers)
import           Network.TLS.Extra.Cipher (ciphersuite_all)
import           Servant (ServantErr)
import           Servant.Client.Core (BaseUrl (..), Scheme (..))
import           System.Metrics (Store, createCounter, createGauge)
import           System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import           System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge
import           System.Remote.Monitoring.Statsd (StatsdOptions (..))
import           System.Wlog (CanLog, HasLoggerName, LoggerName (..), LoggerNameBox (..),
                              WithLogger, launchFromFile)

import           Cardano.Wallet.API.V1.Types (AccountIndex, PaymentSource (..), Transaction,
                                              V1, WalletId (..))
import           Cardano.Wallet.Client (ClientError (..), WalletClient)
import           Cardano.Wallet.Client.Http (mkHttpClient)
import           Pos.Core (Address (..), Coin (..))
--

--------------------------------------------------------------------------------
data WithDrawlRequest = WithDrawlRequest {
    _wAddress :: !(V1 Address)
  , _wAmount  :: !(V1 Coin)
  } deriving (Show, Typeable, Generic)

makeLenses ''WithDrawlRequest

instance FromJSON WithDrawlRequest where
  parseJSON = withObject "WithDrawlRequest" $ \v -> WithDrawlRequest
    <$> v .: "address"
    <*> v .: "amount"

instance ToJSON WithDrawlRequest where
    toJSON (WithDrawlRequest w a) =
        object ["address" .= w, "amount" .= a]

data WithDrawlResult =
    WithdrawlError ClientError
  | WithdrawlSuccess Transaction
  deriving (Show, Typeable, Generic)

instance ToJSON WithDrawlResult where
    toJSON (WithdrawlSuccess txn) =
        object ["success" .= txn]
    toJSON (WithdrawlError err) =
        object ["error" .= show err]


--------------------------------------------------------------------------------
data DepositRequest = DepositRequest {
    _dWalletId :: Text
  , _dAmount   :: Coin
  } deriving (Show, Typeable, Generic)

makeLenses ''DepositRequest

instance FromJSON DepositRequest where
  parseJSON = withObject "DepositRequest" $ \v -> DepositRequest
    <$> v .: "wallet"
    <*> (Coin <$> v .: "amount")

data DepositResult = DepositResult
  deriving (Show, Typeable, Generic)

instance ToJSON DepositResult

--------------------------------------------------------------------------------
newtype FaucetStatsdOpts = FaucetStatsdOpts StatsdOptions deriving (Generic)

makeWrapped ''FaucetStatsdOpts

instance FromJSON FaucetStatsdOpts where
    parseJSON = fmap FaucetStatsdOpts . (withObject "StatsdOptions" $ \v ->
        StatsdOptions
          <$> v .: "host"
          <*> v .: "port"
          <*> v .: "flush-interval"
          <*> pure False
          <*> pure "faucet"
          <*> pure "")

--------------------------------------------------------------------------------
data SourceWalletConfig = SourceWalletConfig {
    _srcWalletId         :: !WalletId
  , _srcAccountIndex     :: !AccountIndex
  , _srcSpendingPassword :: !Text
  }

srcSpendingPassword :: Lens' SourceWalletConfig Text
srcSpendingPassword f = \(SourceWalletConfig w a p) ->
    SourceWalletConfig w a <$> f p

instance FromJSON SourceWalletConfig where
    parseJSON = withObject "SourceWalletConfig" $ \v -> SourceWalletConfig
      <$> v .: "wallet-id"
      <*> v .: "account-index"
      <*> v .: "sending-password"

readSourceWalletConfig :: FilePath -> IO (Either String SourceWalletConfig)
readSourceWalletConfig = fmap eitherDecode . BSL.readFile

cfgToPaymentSource :: SourceWalletConfig -> PaymentSource
cfgToPaymentSource (SourceWalletConfig wId aIdx _) = PaymentSource wId aIdx

--------------------------------------------------------------------------------
data FaucetConfig = FaucetConfig {
    _fcWalletApiHost          :: !String
  , _fcWalletApiPort          :: !Int
  , _fcStatsdOpts             :: !FaucetStatsdOpts
  , _fcSourceWalletConfigFile :: !FilePath
  , _fcLoggerConfigFile       :: !FilePath
  , _fcPubCertFile            :: !FilePath
  , _fcPrivKeyFile            :: !FilePath
  }

makeClassy ''FaucetConfig

instance FromJSON FaucetConfig where
    parseJSON = withObject "FaucetConfig" $ \v ->
        FaucetConfig
          <$> v .: "wallet-host"
          <*> v .: "wallet-port"
          <*> v .: "statsd"
          <*> v .: "source-wallet-config"
          <*> v .: "logging-config"
          <*> v .: "public-certificate"
          <*> v .: "private-key"

mkFaucetConfig
    :: String
    -> Int
    -> FaucetStatsdOpts
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FaucetConfig
mkFaucetConfig = FaucetConfig


--------------------------------------------------------------------------------
data FaucetEnv = FaucetEnv {
    _feWithdrawn        :: !Counter
  , _feNumWithdrawn     :: !Counter
  , _feWalletBalance    :: !Gauge
  , _feStore            :: !Store
  , _fePaymentSource    :: !PaymentSource
  , _feSpendingPassword :: !Text
  , _feFaucetConfig     :: !FaucetConfig
  , _feWalletClient     :: !(WalletClient IO)
  }

makeClassy ''FaucetEnv

--------------------------------------------------------------------------------
initEnv :: FaucetConfig -> Store -> IO FaucetEnv
initEnv fc store = do
    withdrawn <- createCounter "total-withdrawn" store
    withdrawCount <- createCounter "num-withdrawals" store
    balance <- createGauge "wallet-balance" store
    manager <- createManager fc
    srcCfg <- either (error . ("SourceWalletConfig decode errror: " ++)) id
                 <$> readSourceWalletConfig (fc ^. fcSourceWalletConfigFile)
    let url = BaseUrl Https (fc ^. fcWalletApiHost) (fc ^. fcWalletApiPort) ""
    return $ FaucetEnv withdrawn withdrawCount balance
                       store
                       (cfgToPaymentSource srcCfg)
                       (srcCfg ^. srcSpendingPassword)
                       fc
                       (mkHttpClient url manager)

createManager :: FaucetConfig -> IO Manager
createManager fc = do
    pubCert <- BS.readFile (fc ^. fcPubCertFile)
    privKey <- BS.readFile (fc ^. fcPrivKeyFile)
    case credentialLoadX509FromMemory pubCert privKey of
        Left problem -> error $ "Unable to load credentials: " <> (problem ^. packed)
        Right credential ->
            let hooks = def {
                            onCertificateRequest = \_ -> return $ Just credential,
                            onServerCertificate  = \_ _ _ _ -> return []
                        }
                clientParams = (defaultParamsClient "localhost" "") {
                                   clientHooks = hooks,
                                   clientSupported = def {
                                       supportedCiphers = ciphersuite_all
                                   }
                               }
                tlsSettings = TLSSettings clientParams
            in
            newManager $ mkManagerSettings tlsSettings Nothing

incWithDrawn :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
incWithDrawn (Coin (fromIntegral -> c)) = do
  wd <- view feWithdrawn
  wc <- view feNumWithdrawn
  bal <- view feWalletBalance
  liftIO $ do
    Counter.add wd c
    Counter.inc wc
    Gauge.add bal c

decrWithDrawn :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
decrWithDrawn (Coin (fromIntegral -> c)) = do
  -- wd <- view feWithdrawn
  -- wc <- view feNumWithdrawn
  bal <- view feWalletBalance
  liftIO $ do
    -- Counter.subtract wd c
    -- Counter.inc wc
    Gauge.subtract bal c

setWalletBalance :: (MonadReader e m, HasFaucetEnv e, MonadIO m) => Coin -> m ()
setWalletBalance (Coin (fromIntegral -> c)) = do
  bal <- view feWalletBalance
  liftIO $ Gauge.set bal c

--------------------------------------------------------------------------------
newtype M a = M { unM :: ReaderT FaucetEnv (ExceptT ServantErr (LoggerNameBox IO)) a }
  deriving (Functor, Applicative, Monad, MonadReader FaucetEnv, CanLog, HasLoggerName, MonadIO)

runM :: FaucetEnv -> M a -> IO (Either ServantErr a)
runM c = launchFromFile (c ^. feFaucetConfig . fcLoggerConfigFile) (LoggerName "faucet")
       . runExceptT
       . flip runReaderT c
       . unM

type MonadFaucet c m = (MonadIO m, MonadReader c m, HasFaucetEnv c, WithLogger m, HasLoggerName m)
