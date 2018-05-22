-- | Types defining the main blockchain.

module Pos.Core.Block.Main.Types
       ( MainBlockchain
       , MainProof (..)
       , MainConsensusData (..)
       , MainExtraHeaderData (..)
       , MainBlockHeader
       , MainBody (..)
       , MainExtraBodyData (..)
       , MainBlock

       , BlockHeaderAttributes
       , BlockBodyAttributes
       , BlockSignature (..)
       , MainToSign (..)
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Fmt (genericF)
import           Formatting (bprint, build, builder, (%))

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), cborError, deriveSimpleBi,
                                   encodeListLen, enforceSize)
import           Pos.Core.Block.Blockchain (GenericBlock (..), GenericBlockHeader (..))
import           Pos.Core.Common (ChainDifficulty, HeaderHash)
import           Pos.Core.Delegation (DlgPayload, ProxySigHeavy, ProxySigLight)
import           Pos.Core.Slotting.Types (SlotId (..))
import           Pos.Core.Ssc (SscPayload, SscProof)
import           Pos.Core.Txp (TxPayload, TxProof)
import           Pos.Core.Update (UpdatePayload, UpdateProof)
import           Pos.Core.Update.Types (BlockVersion, SoftwareVersion)
import           Pos.Crypto (Hash, PublicKey, Signature)
import           Pos.Data.Attributes (Attributes, areAttributesKnown)

-- | Represents blockchain consisting of main blocks, i. e. blocks
-- with actual payload (transactions, SSC, update system, etc.).
data MainBlockchain

-- | Proof of everything contained in the payload.
data MainProof = MainProof
    { mpTxProof       :: !TxProof
    , mpMpcProof      :: !SscProof
    , mpProxySKsProof :: !(Hash DlgPayload)
    , mpUpdateProof   :: !UpdateProof
    } deriving (Eq, Show, Generic)

instance NFData MainProof

instance Buildable MainProof where
    build = genericF

instance Bi MainProof where
    encode bc =  encodeListLen 4
              <> encode (mpTxProof bc)
              <> encode (mpMpcProof bc)
              <> encode (mpProxySKsProof bc)
              <> encode (mpUpdateProof bc)
    decode = do
        enforceSize "MainProof" 4
        MainProof <$> decode <*>
                      decode <*>
                      decode <*>
                      decode

-- | Signature of the block. Can be either regular signature from the
-- issuer or delegated signature having a constraint on epoch indices
-- (it means the signature is valid only if block's slot id has epoch
-- inside the constrained interval).
data BlockSignature
    = BlockSignature (Signature MainToSign)
    | BlockPSignatureLight (ProxySigLight MainToSign)
    | BlockPSignatureHeavy (ProxySigHeavy MainToSign)
    deriving (Show, Eq, Generic)

instance NFData MainProof => NFData BlockSignature

instance Buildable BlockSignature where
    build (BlockSignature s)       = bprint ("BlockSignature: "%build) s
    build (BlockPSignatureLight s) = bprint ("BlockPSignatureLight: "%build) s
    build (BlockPSignatureHeavy s) = bprint ("BlockPSignatureHeavy: "%build) s

instance Bi BlockSignature where
    encode input = case input of
        BlockSignature sig       -> encodeListLen 2 <> encode (0 :: Word8) <> encode sig
        BlockPSignatureLight pxy -> encodeListLen 2 <> encode (1 :: Word8) <> encode pxy
        BlockPSignatureHeavy pxy -> encodeListLen 2 <> encode (2 :: Word8) <> encode pxy
    decode = do
        enforceSize "BlockSignature" 2
        tag <- decode @Word8
        case tag of
          0 -> BlockSignature <$> decode
          1 -> BlockPSignatureLight <$> decode
          2 -> BlockPSignatureHeavy <$> decode
          _ -> cborError $ "decode@BlockSignature: unknown tag: " <> show tag

-- | Data to be signed in main block.
data MainToSign
    = MainToSign
    { _msHeaderHash  :: !HeaderHash  -- ^ Hash of previous header
                                     --    in the chain
    , _msBodyProof   :: !MainProof
    , _msSlot        :: !SlotId
    , _msChainDiff   :: !ChainDifficulty
    , _msExtraHeader :: !MainExtraHeaderData
    } deriving Generic

deriving instance Show MainToSign
deriving instance Eq MainToSign

instance Bi MainToSign where
    encode mts = encodeListLen 5
               <> encode (_msHeaderHash mts)
               <> encode (_msBodyProof mts)
               <> encode (_msSlot mts)
               <> encode (_msChainDiff mts)
               <> encode (_msExtraHeader mts)
    decode = do
        enforceSize "MainToSign" 5
        MainToSign <$> decode <*>
                          decode <*>
                          decode <*>
                          decode <*>
                          decode

data MainConsensusData = MainConsensusData
    { -- | Id of the slot for which this block was generated.
      _mcdSlot       :: !SlotId
    , -- | Public key of the slot leader. It's essential to have it here,
      -- because FTS gives us only hash of public key (aka 'StakeholderId').
      _mcdLeaderKey  :: !PublicKey
    , -- | Difficulty of chain ending in this block.
      _mcdDifficulty :: !ChainDifficulty
    , -- | Signature given by slot leader.
      _mcdSignature  :: !BlockSignature
    } deriving (Generic, Show, Eq)

instance NFData MainConsensusData

instance Bi MainConsensusData where
    encode cd =  encodeListLen 4
              <> encode (_mcdSlot cd)
              <> encode (_mcdLeaderKey cd)
              <> encode (_mcdDifficulty cd)
              <> encode (_mcdSignature cd)
    decode = do
        enforceSize "MainConsensusData" 4
        MainConsensusData <$> decode <*>
                                 decode <*>
                                 decode <*>
                                 decode

-- | Represents main block header attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending header with new
-- fields via softfork.
type BlockHeaderAttributes = Attributes ()

-- | Represents main block header extra data
data MainExtraHeaderData = MainExtraHeaderData
    { -- | Version of block.
      _mehBlockVersion    :: !BlockVersion
    , -- | Software version.
      _mehSoftwareVersion :: !SoftwareVersion
    , -- | Header attributes
      _mehAttributes      :: !BlockHeaderAttributes
    , -- | Extra body data Hash
      _mehEBDataProof     :: !(Hash MainExtraBodyData)
    } deriving (Eq, Show, Generic)

instance NFData MainExtraHeaderData

instance Buildable MainExtraHeaderData where
    build MainExtraHeaderData {..} =
      bprint ( "    block: v"%build%"\n"
             % "    software: "%build%"\n"
             % builder
             )
            _mehBlockVersion
            _mehSoftwareVersion
            formattedExtra
      where
        formattedExtra
            | areAttributesKnown _mehAttributes = mempty
            | otherwise = bprint ("    attributes: "%build%"\n") _mehAttributes

-- | Header of generic main block.
type MainBlockHeader = GenericBlockHeader MainBlockchain

-- | In our cryptocurrency, body consists of payloads of all block
-- components.
data MainBody = MainBody
    { -- | Txp payload.
      _mbTxPayload     :: !TxPayload
    , -- | Ssc payload.
      _mbSscPayload    :: !SscPayload
    , -- | Heavyweight delegation payload (no-ttl certificates).
      _mbDlgPayload    :: !DlgPayload
      -- | Additional update information for the update system.
    , _mbUpdatePayload :: !UpdatePayload
    } deriving (Eq, Show, Generic, Typeable)

instance NFData MainBody

instance Bi MainBody where
    encode bc =  encodeListLen 4
              <> encode (_mbTxPayload  bc)
              <> encode (_mbSscPayload bc)
              <> encode (_mbDlgPayload bc)
              <> encode (_mbUpdatePayload bc)
    decode = do
        enforceSize "MainBody" 4
        MainBody <$> decode <*>
                        decode <*>
                        decode <*>
                        decode

-- | Represents main block body attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending block with new
-- fields via softfork.
type BlockBodyAttributes = Attributes ()

-- | Represents main block extra data
newtype MainExtraBodyData = MainExtraBodyData
    { _mebAttributes  :: BlockBodyAttributes
    } deriving (Eq, Show, Generic, NFData)

instance Buildable MainExtraBodyData where
    build (MainExtraBodyData attrs)
        | areAttributesKnown attrs = "no extra data"
        | otherwise = bprint ("extra data has attributes: "%build) attrs

deriveSimpleBi ''MainExtraHeaderData [
    Cons 'MainExtraHeaderData [
        Field [| _mehBlockVersion    :: BlockVersion              |],
        Field [| _mehSoftwareVersion :: SoftwareVersion           |],
        Field [| _mehAttributes      :: BlockHeaderAttributes  |],
        Field [| _mehEBDataProof     :: Hash MainExtraBodyData |]
    ]]

deriveSimpleBi ''MainExtraBodyData [
    Cons 'MainExtraBodyData [
        Field [| _mebAttributes :: BlockBodyAttributes |]
    ]]

-- | MainBlock is a block with transactions and MPC messages. It's the
-- main part of our consensus algorithm.
type MainBlock = GenericBlock MainBlockchain

-- instance NFData MainBlock
