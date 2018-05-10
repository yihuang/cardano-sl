-- | Arbitrary instances for Update System types

module Pos.Arbitrary.Update.MemState
       (
       ) where

import           Universum

import           Test.QuickCheck (Arbitrary (..))

import           Pos.Arbitrary.Update.Core ()
import           Pos.Binary.Class (biSize)
<<<<<<< HEAD
import           Pos.Core.Configuration (HasProtocolMagic)
=======
import           Pos.Core.Configuration (HasConfiguration)
>>>>>>> CHW-82-84, orphan branch
import qualified Pos.Update.MemState as Upd

import           Test.Pos.Crypto.Arbitrary ()

<<<<<<< HEAD
instance HasProtocolMagic => Arbitrary Upd.MemPool where
=======
instance HasConfiguration => Arbitrary Upd.MemPool where
>>>>>>> CHW-82-84, orphan branch
    arbitrary = do
        proposals <- arbitrary
        votes <- arbitrary
        return $ Upd.MemPool proposals votes (biSize proposals + biSize votes)
