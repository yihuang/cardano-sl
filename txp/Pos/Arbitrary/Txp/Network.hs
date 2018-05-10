{-# LANGUAGE TemplateHaskell #-}

-- | 'Arbitrary' instances for 'Pos.Txp.Network' types defined in 'src'

module Pos.Arbitrary.Txp.Network () where

import           Universum

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Txp ()
import           Pos.Communication.Types.Relay (DataMsg (..))
<<<<<<< HEAD
import           Pos.Core (HasProtocolMagic)
import           Pos.Txp.Network.Types (TxMsgContents (..))

instance HasProtocolMagic => Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasProtocolMagic => Arbitrary (DataMsg TxMsgContents) where
=======
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Txp.Network.Types (TxMsgContents (..))

instance HasConfiguration => Arbitrary TxMsgContents where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary (DataMsg TxMsgContents) where
>>>>>>> CHW-82-84, orphan branch
    arbitrary = DataMsg <$> arbitrary
    shrink = genericShrink
