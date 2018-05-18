{-# LANGUAGE TypeFamilies #-}

-- | Types related to genesis core data.

module Pos.Core.Genesis.Types
       (
         GenesisWStakeholders (..)
       , GenesisDelegation (..)
       , GenesisVssCertificatesMap (..)
       , noGenesisDelegation
       , mkGenesisDelegation
       , recreateGenesisDelegation

         -- * GenesisSpec
       , TestnetBalanceOptions (..)
       , FakeAvvmOptions (..)
       , GenesisInitializer (..)
       , GenesisAvvmBalances (..)
       , GenesisNonAvvmBalances (..)
       , GenesisProtocolConstants (..)
       , GenesisSpec (..)
       , mkGenesisSpec
       , genesisProtocolConstantsToProtocolConstants
       , genesisProtocolConstantsFromProtocolConstants

       -- * GenesisData
       , GenesisData (..)
       ) where

import           Universum

import           Control.Lens (at)
import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Aeson.TH (deriveJSON)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as Buildable
import           Fmt (genericF)
import           Formatting (bprint, build, fixed, int, sformat, (%))
import           Serokell.Aeson.Options (defaultOptions)
import           Serokell.Util (allDistinct, mapJson)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (Address, Coin, CoinPortion, SharedSeed, StakeholderId,
                                  addressHash, decodeTextAddress, unsafeAddCoin, unsafeGetCoin,
                                  unsafeIntegerToCoin)
import           Pos.Core.Delegation (ProxySKHeavy)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..), VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting.Types (Timestamp)
import           Pos.Core.Ssc.Types (VssCertificatesMap, getVssCertificatesMap)
import           Pos.Core.Update.Types (BlockVersionData)
import           Pos.Crypto.Configuration (ProtocolMagic)
import           Pos.Crypto.Signing (RedeemPublicKey, isSelfSignedPsk, pskDelegatePk, pskIssuerPk)
import           Pos.Util.Util (toAesonError)

-- | Wrapper around weighted stakeholders map to be used in genesis
-- core data.
--
-- Each 'Word16' is a weight. I.e. if stakeholder A has weight "1"
-- and stakeholder B has weight "3", during the bootstrap era
-- all stake in the system will be divided between A and B
-- in proportion of 1:3.
newtype GenesisWStakeholders = GenesisWStakeholders
    { getGenesisWStakeholders :: Map StakeholderId Word16
    } deriving (Show, Eq, Monoid)

instance Buildable GenesisWStakeholders where
    build (GenesisWStakeholders m) =
        bprint ("GenesisWStakeholders: "%mapJson) m

deriving instance ToJSON GenesisWStakeholders
deriving instance FromJSON GenesisWStakeholders

-- | Predefined balances of non avvm entries.
newtype GenesisVssCertificatesMap = GenesisVssCertificatesMap
    { getGenesisVssCertificatesMap :: VssCertificatesMap
    } deriving (Show, Eq, Monoid)

instance Buildable GenesisVssCertificatesMap where
    build (GenesisVssCertificatesMap m) =
        bprint ("GenesisVssCertificatesMap: "%mapJson) (getVssCertificatesMap m)

instance ToJSON GenesisVssCertificatesMap where
    toJSON = toJSON . getGenesisVssCertificatesMap

instance FromJSON GenesisVssCertificatesMap where
    parseJSON val = GenesisVssCertificatesMap <$> parseJSON val

-- | This type contains genesis state of heavyweight delegation. It
-- wraps a map where keys are issuers (i. e. stakeholders who
-- delegated) and values are proxy signing keys. There are some invariants:
-- 1. In each pair delegate must differ from issuer, i. e. no revocations.
-- 2. PSKs must be consistent with keys in the map, i. e. issuer's ID must be
--    equal to the key in the map.
-- 3. Delegates can't be issuers, i. e. transitive delegation is not supported.
--    It's not needed in genesis, it can always be reduced.
newtype GenesisDelegation = UnsafeGenesisDelegation
    { unGenesisDelegation :: HashMap StakeholderId ProxySKHeavy
    } deriving (Show, Eq, ToList, Container)

type instance Element GenesisDelegation = ProxySKHeavy

instance ToJSON GenesisDelegation where
    toJSON = toJSON . unGenesisDelegation

instance FromJSON GenesisDelegation where
    parseJSON = parseJSON >=> \v -> do
        (elems :: HashMap StakeholderId ProxySKHeavy) <- mapM parseJSON v
        toAesonError $ recreateGenesisDelegation elems

-- | Empty 'GenesisDelegation'.
noGenesisDelegation :: GenesisDelegation
noGenesisDelegation = UnsafeGenesisDelegation mempty

-- | Safe constructor of 'GenesisDelegation' from a list of PSKs.
mkGenesisDelegation ::
       MonadError Text m
    => [ProxySKHeavy]
    -> m GenesisDelegation
mkGenesisDelegation psks = do
    unless (allDistinct $ pskIssuerPk <$> psks) $
        throwError "all issuers must be distinct"
    let res = HM.fromList [(addressHash (pskIssuerPk psk), psk) | psk <- psks]
    recreateGenesisDelegation res

-- | Safe constructor of 'GenesisDelegation' from existing map.
recreateGenesisDelegation ::
       MonadError Text m
    => HashMap StakeholderId ProxySKHeavy
    -> m GenesisDelegation
recreateGenesisDelegation pskMap = do
    forM_ (HM.toList pskMap) $ \(k, psk) ->
        when (addressHash (pskIssuerPk psk) /= k) $
            throwError $ sformat
                ("wrong issuerPk set as key for delegation map: "%
                 "issuer id = "%build%", cert id = "%build)
                k (addressHash (pskIssuerPk psk))
    when (any isSelfSignedPsk pskMap) $
        throwError "there is a self-signed (revocation) psk"
    let isIssuer psk =
            isJust $ pskMap ^. at (addressHash (pskDelegatePk psk))
    when (any isIssuer pskMap) $
        throwError "one of the delegates is also an issuer, don't do it"
    return $ UnsafeGenesisDelegation pskMap

----------------------------------------------------------------------------
-- Genesis Spec
----------------------------------------------------------------------------

-- | These options determine balances of nodes specific for testnet.
data TestnetBalanceOptions = TestnetBalanceOptions
    { tboPoors          :: !Word
    -- ^ Number of poor nodes (with small balance).
    , tboRichmen        :: !Word
    -- ^ Number of rich nodes (with huge balance).
    , tboTotalBalance   :: !Word64
    -- ^ Total balance owned by these nodes.
    , tboRichmenShare   :: !Double
    -- ^ Portion of stake owned by all richmen together.
    , tboUseHDAddresses :: !Bool
    -- ^ Whether generate plain addresses or with hd payload.
    } deriving (Show)

instance Buildable TestnetBalanceOptions where
    build TestnetBalanceOptions {..} =
        bprint
            (int%" poor guys, "%
             int%" rich guys , "%
             "total balance is "%int%
             ", richmen share is "%fixed 3%
             " and useHDAddresses flag is " %build)

            tboPoors
            tboRichmen
            tboTotalBalance
            tboRichmenShare
            tboUseHDAddresses

deriveJSON defaultOptions ''TestnetBalanceOptions

-- | These options determines balances of fake AVVM nodes which didn't
-- really go through vending, but pretend they did.
data FakeAvvmOptions = FakeAvvmOptions
    { faoCount      :: !Word
    , faoOneBalance :: !Word64
    } deriving (Show, Generic)

instance Buildable FakeAvvmOptions where
    build = genericF

deriveJSON defaultOptions ''FakeAvvmOptions

-- | This data type contains various options which determine genesis
-- stakes, balanaces, heavy delegation, etc.
data GenesisInitializer = GenesisInitializer
    { giTestBalance       :: !TestnetBalanceOptions
    , giFakeAvvmBalance   :: !FakeAvvmOptions
    , giAvvmBalanceFactor :: !CoinPortion
    -- ^ Avvm balances will be multiplied by this factor.
    , giUseHeavyDlg       :: !Bool
    -- ^ Whether to use heavyweight delegation for bootstrap era
    -- stakeholders.
    , giSeed              :: !Integer
      -- ^ Seed to use to generate secret data. There are two
      -- ways to use it:
      --
      -- 1. Keep it secret and use genesis data generated from it.
      -- 2. Just use it directly and keep it public if you want
      -- to deploy testing cluster.
    } deriving (Show)

instance (Hashable Address, Buildable Address) =>
         Buildable GenesisInitializer where
    build GenesisInitializer {..} = bprint
        ("GenesisInitializer {\n"%
            "  "%build%"\n"%
            "  "%build%"\n"%
            "  avvm balance factor: "%build%"\n"%
            "  heavyDlg: "%build%"\n"%
            "  seed: "%int%"\n"%
            "}\n"
        )
        giTestBalance
        giFakeAvvmBalance
        giAvvmBalanceFactor
        giUseHeavyDlg
        giSeed

deriveJSON defaultOptions ''GenesisInitializer

-- | Predefined balances of avvm entries.
newtype GenesisAvvmBalances = GenesisAvvmBalances
    { getGenesisAvvmBalances :: HashMap RedeemPublicKey Coin
    } deriving (Show, Eq, Semigroup, Monoid, ToList, Container)

type instance Element GenesisAvvmBalances = Coin

deriving instance ToJSON GenesisAvvmBalances
deriving instance FromJSON GenesisAvvmBalances

-- | Predefined balances of non avvm entries.
newtype GenesisNonAvvmBalances = GenesisNonAvvmBalances
    { getGenesisNonAvvmBalances :: HashMap Address Coin
    } deriving (Show, Eq)

instance (Hashable Address, Buildable Address) =>
         Buildable GenesisNonAvvmBalances where
    build (GenesisNonAvvmBalances m) =
        bprint ("GenesisNonAvvmBalances: " %mapJson) m

deriving instance Hashable Address => Monoid GenesisNonAvvmBalances

instance ToJSON GenesisNonAvvmBalances where
    toJSON = toJSON . convert . getGenesisNonAvvmBalances
      where
        convert :: HashMap Address Coin -> HashMap Text Integer
        convert = HM.fromList . map f . HM.toList
        f :: (Address, Coin) -> (Text, Integer)
        f = bimap pretty (toInteger . unsafeGetCoin)

instance FromJSON GenesisNonAvvmBalances where
    parseJSON = toAesonError . convertNonAvvmDataToBalances <=< parseJSON

-- | Generate genesis address distribution out of avvm
-- parameters. Txdistr of the utxo is all empty. Redelegate it in
-- calling funciton.
convertNonAvvmDataToBalances
    :: forall m .
       ( MonadError Text m, Bi Address )
    => HashMap Text Integer
    -> m GenesisNonAvvmBalances
convertNonAvvmDataToBalances balances = GenesisNonAvvmBalances <$> balances'
  where
    balances' :: m (HashMap Address Coin)
    balances' = HM.fromListWith unsafeAddCoin <$> traverse convert (HM.toList balances)
    convert :: (Text, Integer) -> m (Address, Coin)
    convert (txt, i) = do
        addr <- either throwError pure $ decodeTextAddress txt
        return (addr, unsafeIntegerToCoin i)

-- | 'GensisProtocolConstants' are not really part of genesis global state,
-- but they affect consensus, so they are part of 'GenesisSpec' and
-- 'GenesisData'.
data GenesisProtocolConstants = GenesisProtocolConstants
    { -- | Security parameter from the paper.
      gpcK             :: !Int
      -- | Magic constant for separating real/testnet.
    , gpcProtocolMagic :: !ProtocolMagic
      -- | VSS certificates max timeout to live (number of epochs).
    , gpcVssMaxTTL     :: !VssMaxTTL
      -- | VSS certificates min timeout to live (number of epochs).
    , gpcVssMinTTL     :: !VssMinTTL
    } deriving (Show, Eq, Generic)

deriveJSON defaultOptions ''GenesisProtocolConstants

genesisProtocolConstantsToProtocolConstants
    :: GenesisProtocolConstants
    -> ProtocolConstants
genesisProtocolConstantsToProtocolConstants GenesisProtocolConstants {..} =
    ProtocolConstants
        { pcK = gpcK
        , pcVssMinTTL = gpcVssMinTTL
        , pcVssMaxTTL = gpcVssMaxTTL
        }

genesisProtocolConstantsFromProtocolConstants
    :: ProtocolConstants
    -> ProtocolMagic
    -> GenesisProtocolConstants
genesisProtocolConstantsFromProtocolConstants ProtocolConstants {..} pm =
    GenesisProtocolConstants
        { gpcK = pcK
        , gpcProtocolMagic = pm
        , gpcVssMinTTL = pcVssMinTTL
        , gpcVssMaxTTL = pcVssMaxTTL
        }

-- | Specification how to generate full genesis data.
data GenesisSpec = UnsafeGenesisSpec
    { gsAvvmDistr         :: !GenesisAvvmBalances
    -- ^ Genesis data describes avvm utxo.
    , gsFtsSeed           :: !SharedSeed
    -- ^ Seed for FTS for 0-th epoch.
    , gsHeavyDelegation   :: !GenesisDelegation
    -- ^ Genesis state of heavyweight delegation. Will be concatenated
    -- with genesis delegation for bootstrap stakeholders if
    -- 'tiUseHeavyDlg' is 'True'.
    , gsBlockVersionData  :: !BlockVersionData
    -- ^ Genesis 'BlockVersionData'.
    , gsProtocolConstants :: !GenesisProtocolConstants
    -- ^ Other constants which affect consensus.
    , gsInitializer       :: !GenesisInitializer
    -- ^ Other data which depend on genesis type.
    } deriving (Show, Generic)

deriveJSON defaultOptions ''GenesisSpec

-- | Safe constructor for 'GenesisSpec'. Throws error if something
-- goes wrong.
mkGenesisSpec
    :: GenesisAvvmBalances
    -> SharedSeed
    -> GenesisDelegation
    -> BlockVersionData
    -> GenesisProtocolConstants
    -> GenesisInitializer
    -> Either String GenesisSpec
mkGenesisSpec avvmDistr seed delega bvd pc specType = do
    let avvmKeys = HM.keys $ getGenesisAvvmBalances avvmDistr
    unless (allDistinct avvmKeys) $
        throwError $ "mkGenesisSpec: there are duplicates in avvm balances"

    -- All checks passed
    pure $ UnsafeGenesisSpec avvmDistr seed delega bvd pc specType

----------------------------------------------------------------------------
-- GenesisData
----------------------------------------------------------------------------

-- | Genesis data contains all data which determines consensus
-- rules. It must be same for all nodes. It's used to initialize
-- global state, slotting, etc.
data GenesisData = GenesisData
    { gdBootStakeholders :: !GenesisWStakeholders
    , gdHeavyDelegation  :: !GenesisDelegation
    , gdStartTime        :: !Timestamp
    , gdVssCerts         :: !GenesisVssCertificatesMap
    , gdNonAvvmBalances  :: !GenesisNonAvvmBalances
    , gdBlockVersionData :: !BlockVersionData
    , gdProtocolConsts   :: !GenesisProtocolConstants
    , gdAvvmDistr        :: !GenesisAvvmBalances
    , gdFtsSeed          :: !SharedSeed
    } deriving (Show, Eq)
