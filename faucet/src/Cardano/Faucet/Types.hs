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
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}
module Cardano.Faucet.Types (
   FaucetConfig(..)
 , HasFaucetConfig(..)
 , FaucetEnv(..), initEnv
 , HasFaucetEnv(..)
 , incWithDrawn
 , decrWithDrawn
 , setWalletBalance
 , WithDrawlRequest(..), wAddress
 , WithDrawlResult(..)
 , DepositRequest(..), dWalletId, dAmount
 , DepositResult(..)
 , M, runM
 , MonadFaucet
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (Exception, throw)
import           Control.Lens hiding ((.=))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.Random.Entropy (getEntropy)
import           Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, object, withObject, (.:),
                             (.=))
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Default (def)
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
-- import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text
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
                              WithLogger, launchFromFile, logError, logInfo, withSublogger)

import           Cardano.Wallet.API.V1.Types (Account (..), AccountIndex,
                                              AssuranceLevel (NormalAssurance), NewWallet (..),
                                              PaymentSource (..), Transaction, V1 (..), Wallet (..),
                                              WalletAddress (..), WalletId (..),
                                              WalletOperation (CreateWallet), unV1)
import           Cardano.Wallet.Client (ClientError (..), WalletClient (..),
                                        WalletResponse (..), liftClient)
import           Cardano.Wallet.Client.Http (mkHttpClient)
import           Pos.Core (Address (..), Coin (..))
import           Pos.Util.BackupPhrase (BackupPhrase (..))
import           Pos.Util.Mnemonics (toMnemonic)


--------------------------------------------------------------------------------
data WithDrawlRequest = WithDrawlRequest {
    _wAddress :: !(V1 Address)
  } deriving (Show, Typeable, Generic)

makeLenses ''WithDrawlRequest

instance FromJSON WithDrawlRequest where
  parseJSON = withObject "WithDrawlRequest" $ \v -> WithDrawlRequest
    <$> v .: "address"

instance ToJSON WithDrawlRequest where
    toJSON (WithDrawlRequest w) =
        object [ "address" .= w ]

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

srcWalletId :: Lens' SourceWalletConfig WalletId
srcWalletId f = \(SourceWalletConfig w a p) ->
    f w <&> \w' -> SourceWalletConfig w' a p

srcSpendingPassword :: Lens' SourceWalletConfig Text
srcSpendingPassword f = \(SourceWalletConfig w a p) ->
    SourceWalletConfig w a <$> f p

instance FromJSON SourceWalletConfig where
    parseJSON = withObject "SourceWalletConfig" $ \v -> SourceWalletConfig
      <$> v .: "wallet-id"
      <*> v .: "account-index"
      <*> v .: "spending-password"

readSourceWalletConfig :: FilePath -> IO (Either String SourceWalletConfig)
readSourceWalletConfig = fmap eitherDecode . BSL.readFile

cfgToPaymentSource :: SourceWalletConfig -> PaymentSource
cfgToPaymentSource (SourceWalletConfig wId aIdx _) = PaymentSource wId aIdx

--------------------------------------------------------------------------------
newtype PaymentCenter = PaymentCenter Int deriving (Generic, Show, Eq, Ord)

makeWrapped ''PaymentCenter

instance FromJSON PaymentCenter

--------------------------------------------------------------------------------
newtype PaymentVariation = PaymentVariation Float deriving (Generic, Show, Eq, Ord)

makeWrapped ''PaymentVariation

instance FromJSON PaymentVariation

--------------------------------------------------------------------------------
data SourceWallet = Generate !FilePath
                  | Provided !FilePath

instance FromJSON SourceWallet where
    parseJSON = withObject "SourceWallet" $ \v ->
        (Generate <$> v .: "generate-to") <|> (Provided <$> v .: "read-from")

data InitializedWallet = InitializedWallet {
    _paymentSource  :: !PaymentSource
  , _walletBallance :: !Int64
  } deriving (Show, Generic)

makeLenses ''InitializedWallet
--------------------------------------------------------------------------------
data FaucetConfig = FaucetConfig {
    _fcWalletApiHost    :: !String
  , _fcWalletApiPort    :: !Int
  , _fcPort             :: !Int
  , _fcPaymentAmount    :: !PaymentCenter
  , _fcPaymentVariation :: !PaymentVariation
  , _fcStatsdOpts       :: !FaucetStatsdOpts
  , _fcSourceWallet     :: !SourceWallet
  , _fcLoggerConfigFile :: !FilePath
  , _fcPubCertFile      :: !FilePath
  , _fcPrivKeyFile      :: !FilePath
  }

makeClassy ''FaucetConfig

instance FromJSON FaucetConfig where
    parseJSON = withObject "FaucetConfig" $ \v ->
        FaucetConfig
          <$> v .: "wallet-host"
          <*> v .: "wallet-port"
          <*> v .: "port"
          <*> v .: "payment-amount"
          <*> v .: "payment-variation"
          <*> v .: "statsd"
          <*> v .: "source-wallet-config"
          <*> v .: "logging-config"
          <*> v .: "public-certificate"
          <*> v .: "private-key"

data CreatedWallet = CreatedWallet {
    _createdWalletId :: WalletId
  , _createdPhrase   :: BackupPhrase
  , _createdAcctIdx  :: AccountIndex
  , _createdAddress  :: Address
  } deriving (Show, Generic)

--------------------------------------------------------------------------------
instance ToJSON CreatedWallet where
    toJSON (CreatedWallet wId phrase acctIdx addr) =
        object [ "wallet-id" .= wId
               , "recovery-words" .= (Text.unwords $ bpToList phrase)
               , "account-index" .= acctIdx
               , "address" .= addr
               ]
--------------------------------------------------------------------------------
data InitFaucetError =
    SourceWalletParseError String
  | WalletCreationError ClientError
  | CouldntReadBalance ClientError
  | NoWalletAccounts WalletId
  | MultipleWalletAccounts WalletId
  | BadAddress WalletId AccountIndex
  deriving (Typeable, Show)

instance Exception InitFaucetError

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

--------------------------------------------------------------------------------
createWallet
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => WalletClient m
    -> m (Either InitFaucetError (BackupPhrase, WalletId, AccountIndex, Address))
createWallet client = withSublogger "create-wallet" $ do
    phrase <- liftIO generateBackupPhrase
    let w = NewWallet (V1 phrase) Nothing NormalAssurance "Faucet-Wallet" CreateWallet
    runExceptT $ do
        wId <- walId <$> (runClient WalletCreationError $ postWallet client w)
        logInfo $ "Created wallet with ID: " <> (Text.pack $ show wId)
        accounts <- runClient WalletCreationError $
                       getAccountIndexPaged
                         client
                         wId
                         Nothing
                         Nothing
        case accounts of
            [a] -> do
                let aIdx = accIndex a
                logInfo $ "Found a single account for wallet with ID: "
                          <> (Text.pack $ show wId)
                          <> " account index: " <> (Text.pack $ show aIdx)
                case accAddresses a of
                    [addr] -> do
                        logInfo $ "Found a single account for wallet with ID: "
                                  <> (Text.pack $ show wId)
                                  <> " account index: " <> (Text.pack $ show aIdx)
                        return (phrase, wId, aIdx, unV1 $ addrId addr)
                    _ -> do
                        logInfo $ "Didn't find an address for wallet with ID: "
                                  <> (Text.pack $ show wId)
                                  <> " account index: " <> (Text.pack $ show aIdx)
                        throwError $ BadAddress wId aIdx
            [] -> do
                logError $ "No accounts found for wallet with ID: " <> (Text.pack $ show wId)
                throwError $ NoWalletAccounts wId
            _ -> do
                logError $ "Multiple accounts found for wallet with ID: " <> (Text.pack $ show wId)
                throwError $ MultipleWalletAccounts wId
    where
        runClient err m = ExceptT $ (fmap (first err)) $ fmap (fmap wrData) m

--------------------------------------------------------------------------------
writeCreatedWalletInfo :: FilePath -> CreatedWallet -> IO ()
writeCreatedWalletInfo fp cw = do
    let theDir = takeDirectory fp
    createDirectoryIfMissing True theDir
    Text.writeFile fp $ encodeToLazyText cw

--------------------------------------------------------------------------------
readWalletBalance
    :: (HasLoggerName m, MonadIO m)
    => WalletClient m
    -> PaymentSource
    -> m (Either InitFaucetError Int64)
readWalletBalance client (psWalletId -> wId) = do
    r <- getWallet client wId
    return $ first CouldntReadBalance $ fmap (fromIntegral . getCoin . unV1 . walBalance . wrData) $ r

makeInitializedWallet
    :: (HasLoggerName m, CanLog m, MonadIO m)
    => FaucetConfig
    -> WalletClient m
    -> m (Either InitFaucetError InitializedWallet)
makeInitializedWallet fc client = withSublogger "makeInitializedWallet" $ do
    case (fc ^. fcSourceWallet) of
        Provided fp -> do
            srcCfg <- liftIO $ readSourceWalletConfig fp
            case cfgToPaymentSource <$> srcCfg of
                Left e -> return $ Left $ SourceWalletParseError e
                Right ps -> do
                    fmap (InitializedWallet ps) <$> readWalletBalance client ps
        Generate fp -> do
            resp <- createWallet client
            forM resp $ \(phrase, wallet,accIdx, addr) -> do
                    let iw = InitializedWallet (PaymentSource wallet accIdx) 0
                        createdWallet = CreatedWallet wallet phrase accIdx addr
                    liftIO $ writeCreatedWalletInfo fp createdWallet

                    return iw

initEnv :: (HasLoggerName m, CanLog m, MonadIO m) => FaucetConfig -> Store -> m FaucetEnv
initEnv fc store = withSublogger "init" $ do
    walletBallanceGauge <- liftIO $ createGauge "wallet-balance" store
    feConstruct <- liftIO $ FaucetEnv
      <$> createCounter "total-withdrawn" store
      <*> createCounter "num-withdrawals" store
      <*> pure walletBallanceGauge
    manager <- liftIO $ createManager fc
    let url = BaseUrl Https (fc ^. fcWalletApiHost) (fc ^. fcWalletApiPort) ""
        client = mkHttpClient url manager
    initialWallet <- makeInitializedWallet fc (liftClient client)
    case initialWallet of
        Left err -> throw err
        Right iw -> do
          liftIO $ Gauge.set walletBallanceGauge (iw ^. walletBallance)
          return $ feConstruct
                      store
                      (iw ^. paymentSource)
                      "TODO: THIS SHOULD BE THE PASSWORD"
                      fc
                      client

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
