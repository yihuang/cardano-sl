module Test.Pos.Core.Dummy
       ( dummyProtocolConstants
       , dummyK
       , dummyEpochSlots
       , dummySlotSecurityParam
       , dummyGenesisHash
       , dummyConfig
       ) where

import           Universum

import           Pos.Core (BlockCount, Config (..), GenesisHash (..),
                     ProtocolConstants (..), SlotCount, VssMaxTTL (..),
                     VssMinTTL (..), kEpochSlots, kSlotSecurityParam,
                     pcBlkSecurityParam)
import           Pos.Crypto (unsafeHash)

import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)

dummyProtocolConstants :: ProtocolConstants
dummyProtocolConstants = ProtocolConstants
    { pcK         = 10
    , pcVssMinTTL = VssMinTTL 2
    , pcVssMaxTTL = VssMaxTTL 6
    }

dummyK :: BlockCount
dummyK = pcBlkSecurityParam dummyProtocolConstants

dummyEpochSlots :: SlotCount
dummyEpochSlots = kEpochSlots dummyK

dummySlotSecurityParam :: SlotCount
dummySlotSecurityParam = kSlotSecurityParam dummyK

dummyGenesisHash :: GenesisHash
dummyGenesisHash = GenesisHash $ unsafeHash @Text "patak"

dummyConfig :: Config
dummyConfig = Config dummyProtocolMagic dummyProtocolConstants dummyGenesisHash
