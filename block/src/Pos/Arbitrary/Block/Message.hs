
module Pos.Arbitrary.Block.Message
       (
       ) where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Arbitrary.Block ()
import           Pos.Arbitrary.Ssc (SscPayloadDependsOnSlot (..))
import           Pos.Arbitrary.Txp ()
import           Pos.Arbitrary.Update ()
import           Pos.Binary.Class (Bi, Raw)
import qualified Pos.Block.Network.Types as T
<<<<<<< HEAD
import           Pos.Core (HasProtocolConstants, HasProtocolMagic, HasGenesisHash)
=======
import           Pos.Core (HasConfiguration)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Core.Ssc (SscPayload, SscProof)

------------------------------------------------------------------------------------------
-- Block network types
------------------------------------------------------------------------------------------

<<<<<<< HEAD
instance Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary T.MsgGetBlocks where
=======
instance HasConfiguration => Arbitrary T.MsgGetHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasConfiguration => Arbitrary T.MsgGetBlocks where
>>>>>>> CHW-82-84, orphan branch
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary SscPayload
         , Arbitrary SscProof
         , Bi Raw
<<<<<<< HEAD
         , HasProtocolConstants
         , HasProtocolMagic
=======
         , HasConfiguration
>>>>>>> CHW-82-84, orphan branch
         ) =>
         Arbitrary T.MsgHeaders where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ( Arbitrary SscPayload
         , Arbitrary SscProof
         , Arbitrary SscPayloadDependsOnSlot
<<<<<<< HEAD
         , HasProtocolConstants
         , HasProtocolMagic
         , HasGenesisHash
=======
         , HasConfiguration
>>>>>>> CHW-82-84, orphan branch
         ) =>
         Arbitrary T.MsgBlock where
    arbitrary = genericArbitrary
    shrink = genericShrink
