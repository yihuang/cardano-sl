{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Default (def)
import           Data.Maybe (fromJust)
import           Data.Time.Units (Second)
import           System.Wlog (Severity (Debug))

import           GHC.TypeLits (symbolVal)
import           Pos.Block.Types (Blund, Undo (..))

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import qualified Cardano.Wallet.Kernel.Transactions as Kernel

import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.DB.TxMeta (TxMeta (..))
import qualified Cardano.Wallet.Kernel.DB.TxMeta as TxMeta
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.Kernel.Keystore (Keystore)
import           Cardano.Wallet.Kernel.Types (AccountId (..),
                     RawResolvedBlock (..), fromRawResolvedBlock)
import           Cardano.Wallet.WalletLayer.ExecutionTimeLimit
                     (limitExecutionTimeTo)
import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..),
                     CreateAddressError (..), EstimateFeesError (..),
                     GetTxError (..), NewPaymentError (..),
                     PassiveWalletLayer (..), WalletLayerError (..))

import           Cardano.Wallet.Kernel.CoinSelection.FromGeneric
                     (CoinSelectionOptions (..), ExpenseRegulation,
                     InputGrouping, newOptions)

import           Cardano.Wallet.API.Indices
import           Pos.Core (Address, Coin, decodeTextAddress)
import qualified Pos.Core as Core
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto (safeDeterministicKeyGen)
import           Pos.Util.Mnemonic (Mnemonic, mnemonicToSeed)

-- import           Cardano.Wallet.API.Request (RequestParams (..))
-- import           Cardano.Wallet.API.Request.Filter (FilterOperations (..))
-- import qualified Cardano.Wallet.API.Request.Parameters as Parameters
import qualified Cardano.Wallet.API.Request.Sort as S
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Actions as Actions

import qualified Data.Map.Strict as Map
import           Pos.Core as Core
import qualified Pos.Crypto as Core
import           Pos.Crypto.Signing


import           Cardano.Wallet.API.V1.Types (Payment (..),
                     PaymentDistribution (..), PaymentSource (..), V1 (..),
                     WalletId (..), unV1)

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadIO n, MonadIO m, MonadMask m)
    => (Severity -> Text -> IO ())
    -> Keystore
    -> (PassiveWalletLayer n -> Kernel.PassiveWallet -> m a) -> m a
bracketPassiveWallet logFunction keystore f =
    Kernel.bracketPassiveWallet logFunction keystore $ \w -> do

      -- Create the wallet worker and its communication endpoint `invoke`.
      bracket (liftIO $ Actions.forkWalletWorker $ Actions.WalletActionInterp
                 { Actions.applyBlocks  =  \blunds ->
                     Kernel.applyBlocks w $
                         OldestFirst (mapMaybe blundToResolvedBlock (toList (getOldestFirst blunds)))
                 , Actions.switchToFork = \_ _ -> logFunction Debug "<switchToFork>"
                 , Actions.emit         = logFunction Debug
                 }
              ) (\invoke -> liftIO (invoke Actions.Shutdown))
              $ \invoke -> do
                  -- TODO (temporary): build a sample wallet from a backup phrase
                  _ <- liftIO $ do
                    let (_, esk) = safeDeterministicKeyGen (mnemonicToSeed $ def @(Mnemonic 12)) emptyPassphrase
                    Kernel.createWalletHdRnd w walletName spendingPassword assuranceLevel esk Map.empty

                  f (passiveWalletLayer w invoke) w

  where
    -- TODO consider defaults
    walletName       = HD.WalletName "(new wallet)"
    spendingPassword = HD.NoSpendingPassword
    assuranceLevel   = HD.AssuranceLevelNormal

    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer :: Kernel.PassiveWallet
                       -> (Actions.WalletAction Blund -> IO ())
                       -> PassiveWalletLayer n
    passiveWalletLayer wallet invoke =
        PassiveWalletLayer
            { _pwlCreateWallet   = error "Not implemented!"
            , _pwlGetWalletIds   = error "Not implemented!"
            , _pwlGetWallet      = error "Not implemented!"
            , _pwlUpdateWallet   = error "Not implemented!"
            , _pwlDeleteWallet   = error "Not implemented!"

            , _pwlCreateAccount  = error "Not implemented!"
            , _pwlGetAccounts    = error "Not implemented!"
            , _pwlGetAccount     = error "Not implemented!"
            , _pwlUpdateAccount  = error "Not implemented!"
            , _pwlDeleteAccount  = error "Not implemented!"

            , _pwlCreateAddress  =
                \(V1.NewAddress mbSpendingPassword accIdx (V1.WalletId wId)) -> do
                    liftIO $ limitExecutionTimeTo (30 :: Second) CreateAddressTimeLimitReached $ do
                        case decodeTextAddress wId of
                             Left _ ->
                                 return $ Left (CreateAddressAddressDecodingFailed wId)
                             Right rootAddr -> do
                                let hdRootId = HD.HdRootId . InDb $ rootAddr
                                let hdAccountId = HD.HdAccountId hdRootId (HD.HdAccountIx accIdx)
                                let passPhrase = maybe mempty coerce mbSpendingPassword
                                res <- liftIO $ Kernel.createAddress passPhrase
                                                                     (AccountIdHdRnd hdAccountId)
                                                                     wallet
                                case res of
                                     Right newAddr -> return (Right newAddr)
                                     Left  err     -> return (Left $ CreateAddressError err)
            , _pwlGetAddresses   = error "Not implemented!"
            , _pwlGetTransactions =
                \ mbWalletId mbAccountIndex _ _ _ sop -> liftIO $ do
                    case toAccountFops mbWalletId mbAccountIndex of
                        Left x            -> return $ Left x
                        Right accountFops -> do
                            db <- Kernel.getWalletSnapshot wallet
                            let k = (2000 :: Word)         -- TODO: retrieve this constant from Core
                            let currentSlot = error "TODO" -- TODO: retrieve this constant from Core
                            case toSorting sop of
                                Left err -> return $ Left err
                                Right mSop -> do
                                    meta <- TxMeta.getTxMetas (wallet ^. Kernel.walletMeta)
                                                            (TxMeta.Offset 0)
                                                            (TxMeta.Limit (fromIntegral (maxBound :: Int)))
                                                            accountFops
                                                            TxMeta.NoFilterOp
                                                            TxMeta.NoFilterOp
                                                            mSop
                                    return $ Right $ map (metaToTx db k currentSlot) meta

            , _pwlApplyBlocks    = liftIO . invoke . Actions.ApplyBlocks
            , _pwlRollbackBlocks = liftIO . invoke . Actions.RollbackBlocks
            }

    -- The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
    -- by the invariants established in the 'Blund'.
    blundToResolvedBlock :: Blund -> Maybe ResolvedBlock
    blundToResolvedBlock (b,u)
        = rightToJust b <&> \mainBlock ->
            fromRawResolvedBlock
            $ UnsafeRawResolvedBlock mainBlock Nothing spentOutputs'
        where
            spentOutputs' = map (map fromJust) $ undoTx u
            rightToJust   = either (const Nothing) Just

-- | Initialize the active wallet.
-- The active wallet is allowed to send transactions, as it has the full
-- 'WalletDiffusion' layer in scope.
bracketActiveWallet
    :: forall m n a. (MonadIO m, MonadMask m, MonadIO n)
    => Core.ProtocolMagic
    -> PassiveWalletLayer n
    -> Kernel.PassiveWallet
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> Kernel.ActiveWallet -> m a) -> m a
bracketActiveWallet pm walletPassiveLayer passiveWallet walletDiffusion runActiveLayer =
    Kernel.bracketActiveWallet pm passiveWallet walletDiffusion $ \activeWallet -> do
        bracket
          (return (activeWalletLayer activeWallet))
          (\_ -> return ())
          (flip runActiveLayer activeWallet)
  where

    activeWalletLayer :: Kernel.ActiveWallet -> ActiveWalletLayer n
    activeWalletLayer activeWallet = ActiveWalletLayer {
          walletPassiveLayer = walletPassiveLayer

        -- | Generates a new transaction @and submit it as pending@.
        , pay = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) NewPaymentTimeLimitReached $ do
                  (opts, accountId, payees) <-
                       liftIO $ setupPayment grouping
                                             regulation
                                             payment
                  res <- liftIO $ Kernel.pay activeWallet
                                             spendingPassword
                                             opts
                                             accountId
                                             payees
                  case res of
                       Left e   -> return . Left . NewPaymentError $ e
                       Right tx -> return . Right $ tx

        -- | Estimates the fees for a payment.
        , estimateFees = \spendingPassword grouping regulation payment -> do
              liftIO $ limitExecutionTimeTo (60 :: Second) EstimateFeesTimeLimitReached $ do
                  (opts, accountId, payees) <-
                      liftIO $ setupPayment grouping
                                            regulation
                                            payment
                  fees <- liftIO $ Kernel.estimateFees activeWallet
                                                       spendingPassword
                                                       opts
                                                       accountId
                                                       payees
                  case fees of
                       Left e  -> return . Left  . EstimateFeesError $ e
                       Right f -> return . Right $ f
        }


-- | Internal function setup to facilitate the creation of the necessary
-- context to perform either a new payment or the estimation of the fees.
setupPayment :: InputGrouping
             -> ExpenseRegulation
             -> Payment
             -> IO ( CoinSelectionOptions
                   , HD.HdAccountId
                   , NonEmpty (Address, Coin)
                   )
setupPayment grouping regulation payment = do

    let (WalletId wId) = psWalletId . pmtSource $ payment

    hdRootId  <- case Core.decodeTextAddress wId of
                     Left e  -> throwM (InvalidAddressConversionFailed e)
                     Right a -> return (HD.HdRootId . InDb $ a)
    let opts = (newOptions Kernel.cardanoFee) {
               csoExpenseRegulation = regulation
             , csoInputGrouping     = grouping
             }
        accountIndex   = HD.HdAccountIx (psAccountIndex . pmtSource $ payment)
        accountId = HD.HdAccountId {
                    _hdAccountIdParent = hdRootId
                  , _hdAccountIdIx     = accountIndex
                  }
        payees    =  (\(PaymentDistribution a c) -> (unV1 a, unV1 c))
                 <$> (pmtDestinations payment)

    return (opts , accountId , payees)

-- | Type Casting for Account filtering from V1 to MetaData Types.
toAccountFops :: Maybe V1.WalletId -> Maybe V1.AccountIndex -> Either GetTxError TxMeta.AccountFops
toAccountFops mbWalletId mbAccountIndex =
    case (mbWalletId, mbAccountIndex) of
        (Nothing, Nothing) -> Right TxMeta.Everything
        (Nothing, Just _)  -> Left GetTxMissingWalletIdError
        -- AccountIndex doesn`t uniquely identify an Account, so we shouldn`t continue without a WalletId.
        (Just (V1.WalletId wId), _) ->
            case decodeTextAddress wId of
                Left _         -> Left $ GetTxAddressDecodingFailed wId
                Right rootAddr -> Right $ TxMeta.AccountFops rootAddr mbAccountIndex

metaToTx :: Kernel.DB -> Word -> Word -> TxMeta -> V1.Transaction
metaToTx db k current TxMeta{..} =
    V1.Transaction {
        txId = V1 _txMetaId,
        txConfirmations = confirmations,
        txAmount = V1 _txMetaAmount,
        txInputs = toPayDistr <$> _txMetaInputs,
        txOutputs = toPayDistr <$> _txMetaOutputs,
        txType = if _txMetaIsLocal then V1.LocalTransaction else V1.ForeignTransaction,
        txDirection = if _txMetaIsOutgoing then V1.OutgoingTransaction else V1.IncomingTransaction,
        txCreationTime = V1 _txMetaCreationAt,
        txStatus = status
    }

        where
            hdAccountId = HD.HdAccountId (HD.HdRootId $ InDb _txMetaWalletId)
                                      (HD.HdAccountIx _txMetaAccountId)

            toPayDistr :: (Address, Coin) -> V1.PaymentDistribution
            toPayDistr (addr, c) = V1.PaymentDistribution (V1 addr) (V1 c)

            mSlot = Kernel.accountTxSlot db hdAccountId _txMetaId
            isPending = Kernel.accountIsTxPending db hdAccountId _txMetaId

            (status, confirmations) = dynamicTxMeta mSlot k current isPending

dynamicTxMeta :: Maybe SlotId -> Word -> Word -> Bool -> (V1.TransactionStatus, Word)
dynamicTxMeta mSlot k currentSlot isPending = case isPending of
    True  -> (V1.Applying, 0)
    False ->
        case mSlot of
        Nothing     -> (V1.WontApply, 0)
        Just (SlotId (EpochIndex w64) (UnsafeLocalSlotIndex w16)) ->
            case ((fromIntegral currentSlot) - w64*(fromIntegral k) + (fromIntegral w16) >= fromIntegral k) of -- TODO: fix
            True  -> (V1.InNewestBlocks, fromIntegral w64) -- TODO: fix
            False -> (V1.Persisted, fromIntegral w16)      -- TODO: fix

toSorting :: S.SortOperations V1.Transaction -> Either GetTxError (Maybe TxMeta.Sorting)
toSorting S.NoSorts = Right Nothing
toSorting (S.SortOp (sop :: S.SortOperation ix V1.Transaction) _) =
    case symbolVal (Proxy @(IndexToQueryParam V1.Transaction ix)) of
        -- :x :x type-safe?
        "created_at" -> Right $ Just $ TxMeta.Sorting TxMeta.SortByCreationAt (toSortingDirection sop)
        txt -> Left $ GetTxInvalidSortingOpearation txt


toSortingDirection :: S.SortOperation ix a -> TxMeta.SortDirection
toSortingDirection (S.SortByIndex srt _) = case srt of
    S.SortAscending  -> TxMeta.Ascending
    S.SortDescending -> TxMeta.Descending

{-}
getTransactions
    :: MonadIO m
    => Kernel.PassiveWallet
    -> Maybe V1.WalletId
    -> Maybe V1.AccountIndex
    -> Maybe (V1 Core.Address)
    -> RequestParams
    -> FilterOperations V1.Transaction
    -> SortOperations V1.Transaction
    -> m (Either GetTxError [V1.Transaction])
-}
