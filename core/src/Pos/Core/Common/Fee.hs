module Pos.Core.Common.Fee
       ( Coeff(..)
       , TxSizeLinear(..)
       , calculateTxSizeLinear
       , txSizeLinearMinValue
       , TxFeePolicy(..)
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import           Data.Aeson.Types (Parser, withScientific)
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import           Data.Fixed (Fixed (..), Nano, resolution, showFixed)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM.S
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, shown, (%))
import           Serokell.Data.Memory.Units (Byte, toBytes)

import           Pos.Binary.Class (Bi (..), decode, decodeKnownCborDataItem,
                                   decodeUnknownCborDataItem, encode, encodeKnownCborDataItem,
                                   encodeListLen, encodeUnknownCborDataItem, enforceSize)
import           Pos.Util.Util (aesonError, toAesonError)

-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Ord, Show, Generic, NFData, Num)

instance Buildable Coeff where
    build (Coeff x) = fromString (showFixed True x)

instance Hashable Coeff

instance ToJSON Coeff where
    toJSON (Coeff v) = toJSON (realToFrac @_ @Double v)

instance FromJSON Coeff where
    parseJSON = withScientific "Coeff" $ \sc -> do
        -- Code below is resistant to changes in precision of 'Coeff'.
        let
            rat = toRational sc * toRational res
            fxd = MkFixed (numerator rat)
            res = resolution fxd
            bad = denominator rat /= 1
        when bad $ aesonError "Fixed precision for coefficient exceeded"
        return $ Coeff fxd

instance Bi Coeff where
    encode (Coeff n) = encode n
    decode = Coeff <$> decode @Nano

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear = TxSizeLinear !Coeff !Coeff
    deriving (Eq, Ord, Show, Generic)

instance NFData TxSizeLinear

instance Buildable TxSizeLinear where
    build (TxSizeLinear a b) =
        bprint (build%" + "%build%"*s") a b

instance Hashable TxSizeLinear

instance ToJSON TxSizeLinear where
    toJSON (TxSizeLinear a b) = object [
        "a" .= a,
        "b" .= b
        ]

instance FromJSON TxSizeLinear where
    parseJSON = withObject "TxSizeLinear" $ \o -> do
        TxSizeLinear
            <$> (o .: "a")
            <*> (o .: "b")

instance Bi TxSizeLinear where
    encode (TxSizeLinear a b) = encodeListLen 2 <> encode a <> encode b
    decode = do
        enforceSize "TxSizeLinear" 2
        !a <- decode @Coeff
        !b <- decode @Coeff
        return $ TxSizeLinear a b

calculateTxSizeLinear :: TxSizeLinear -> Byte -> Nano
calculateTxSizeLinear
    (TxSizeLinear (Coeff a) (Coeff b))
    (fromInteger . toBytes -> txSize) =
        a + b * txSize

txSizeLinearMinValue :: TxSizeLinear -> Nano
txSizeLinearMinValue (TxSizeLinear (Coeff minVal) _) = minVal

-- | Transaction fee policy represents a formula to compute the minimal allowed
-- fee for a transaction. Transactions with lesser fees won't be accepted. The
-- minimal fee may depend on the properties of a transaction (for example, its
-- size in bytes), so the policy can't be represented simply as a number.
--
-- Recall that a transaction fee is the difference between the sum of its
-- inputs and the sum of its outputs. The transaction is accepted when
-- @minimal_fee(tx) <= fee(tx)@, where @minimal_fee@ is the function defined
-- by the policy.
--
-- The policy can change during the lifetime of the blockchain (using the
-- update mechanism). At the moment we have just one policy type (a linear
-- equation on the transaction size), but in the future other policies may
-- be added. To make this future-proof, we also have an "unknown" policy used
-- by older node versions (the ones that haven't updated yet).
data TxFeePolicy
    = TxFeePolicyTxSizeLinear !TxSizeLinear
    | TxFeePolicyUnknown !Word8 !ByteString
    deriving (Eq, Ord, Show, Generic)

instance NFData TxFeePolicy

instance Buildable TxFeePolicy where
    build (TxFeePolicyTxSizeLinear tsp) =
        bprint ("policy(tx-size-linear): "%build) tsp
    build (TxFeePolicyUnknown v bs) =
        bprint ("policy(unknown:"%build%"): "%shown) v bs

instance Hashable TxFeePolicy

instance ToJSON TxFeePolicy where
    toJSON =
        object . \case
            TxFeePolicyTxSizeLinear linear -> ["txSizeLinear" .= linear]
            TxFeePolicyUnknown policyTag policyPayload ->
                ["unknown" .= (policyTag, decodeUtf8 @Text policyPayload)]

instance FromJSON TxFeePolicy where
    parseJSON = withObject "TxFeePolicy" $ \o -> do
        (policyName, policyBody) <- toAesonError $ case HM.S.toList o of
            []  -> Left "TxFeePolicy: none provided"
            [a] -> Right a
            _   -> Left "TxFeePolicy: ambiguous choice"
        let
          policyParser :: FromJSON p => Parser p
          policyParser = parseJSON policyBody
        case policyName of
            "txSizeLinear" ->
                TxFeePolicyTxSizeLinear <$> policyParser
            "unknown" ->
                mkTxFeePolicyUnknown <$> policyParser
            _ ->
                aesonError "TxFeePolicy: unknown policy name"
        where
            mkTxFeePolicyUnknown (policyTag, policyPayload) =
                TxFeePolicyUnknown policyTag
                    (encodeUtf8 @Text @ByteString policyPayload)

instance Bi TxFeePolicy where
    encode policy = case policy of
        TxFeePolicyTxSizeLinear txSizeLinear ->
            encodeListLen 2 <> encode (0 :: Word8)
                            <> encodeKnownCborDataItem txSizeLinear
        TxFeePolicyUnknown word8 bs          ->
            encodeListLen 2 <> encode word8
                            <> encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "TxFeePolicy" 2
        tag <- decode @Word8
        case tag of
            0 -> TxFeePolicyTxSizeLinear <$> decodeKnownCborDataItem
            _ -> TxFeePolicyUnknown tag  <$> decodeUnknownCborDataItem
