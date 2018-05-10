-- | Serialization of SSC types.

module Pos.Binary.Ssc.Types () where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi,
                                   deriveSimpleBiCxt, encodeListLen, enforceSize)
<<<<<<< HEAD
=======
import           Pos.Core.Configuration (HasConfiguration)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Core.Ssc (CommitmentsMap, Opening, OpeningsMap, SharesMap, SignedCommitment,
                               VssCertificatesMap (..))
import           Pos.Ssc.Types (SscGlobalState (..), SscSecretStorage (..))
import           Pos.Ssc.VssCertData (VssCertData (..))

<<<<<<< HEAD
instance Bi VssCertData where
=======
instance HasConfiguration => Bi VssCertData where
>>>>>>> CHW-82-84, orphan branch
    encode VssCertData {..} = mconcat
        [ encodeListLen 6
        , encode lastKnownEoS
        -- It may look weird to encode 'VssCertificatesMap' as a
        -- map, but it's done this way for historical reasons.
        , encode (getVssCertificatesMap certs)
        , encode whenInsMap
        , encode whenInsSet
        , encode whenExpire
        , encode expiredCerts
        ]
    decode = do
        enforceSize "VssCertData" 6
        lastKnownEoS <- decode
        certs <- UnsafeVssCertificatesMap <$> decode
        whenInsMap <- decode
        whenInsSet <- decode
        whenExpire <- decode
        expiredCerts <- decode
        return VssCertData {..}

<<<<<<< HEAD
deriveSimpleBiCxt [t|()|] ''SscGlobalState [
=======
deriveSimpleBiCxt [t|HasConfiguration|] ''SscGlobalState [
>>>>>>> CHW-82-84, orphan branch
    Cons 'SscGlobalState [
        Field [| _sgsCommitments     :: CommitmentsMap |],
        Field [| _sgsOpenings        :: OpeningsMap    |],
        Field [| _sgsShares          :: SharesMap      |],
        Field [| _sgsVssCertificates :: VssCertData    |]
    ]]

deriveSimpleBi ''SscSecretStorage [
    Cons 'SscSecretStorage [
        Field [| sssCommitment :: SignedCommitment |],
        Field [| sssOpening    :: Opening          |],
        Field [| sssEpoch      :: EpochIndex       |]
    ]]
