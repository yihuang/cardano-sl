{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary serialization of core block types.

module Pos.Binary.Core.Blockchain
       (
       ) where

import           Codec.CBOR.Decoding (decodeWordCanonical)
import           Codec.CBOR.Encoding (encodeWord)
import           Universum

import           Pos.Binary.Class (Bi (..), decodeListLenCanonicalOf, encodeListLen)
import           Pos.Core.Block.Genesis.Chain ()
import           Pos.Core.Block.Main.Chain ()
import           Pos.Core.Block.Union.Types (BlockHeader (..))
import           Pos.Util.Util (cborError)



----------------------------------------------------------------------------
-- BlockHeader
----------------------------------------------------------------------------

instance Bi BlockHeader where
   encode x = encodeListLen 2 <> encodeWord tag <> body
     where
       (tag, body) = case x of
         BlockHeaderGenesis bh -> (0, encode bh)
         BlockHeaderMain bh    -> (1, encode bh)

   decode = do
       decodeListLenCanonicalOf 2
       t <- decodeWordCanonical
       case t of
           0 -> BlockHeaderGenesis <$!> decode
           1 -> BlockHeaderMain <$!> decode
           _ -> cborError $ "decode@BlockHeader: unknown tag " <> pretty t
