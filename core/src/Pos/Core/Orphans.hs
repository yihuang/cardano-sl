{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for core

module Pos.Core.Orphans
       (
       ) where

import           Universum

import           Data.Aeson.TH (deriveJSON)
import           Data.Hashable (Hashable, hashWithSalt)
import           Data.Time.Units (Microsecond, Millisecond, Second)
import qualified PlutusCore.Program as PLCore
import qualified PlutusCore.Term as PLCore
import qualified PlutusTypes.ConSig as PLTypes
import qualified PlutusTypes.Type as PLTypes
import qualified Serokell.Aeson.Options as S (defaultOptions)
import qualified Utils.ABT as ABT
import qualified Utils.Names as Names
import qualified Utils.Vars as Vars

import           Pos.Binary.Class (Bi (..), genericDecode, genericEncode, serialize')
import           Pos.Core.Script ()

deriveJSON S.defaultOptions ''Millisecond
deriveJSON S.defaultOptions ''Microsecond
deriveJSON S.defaultOptions ''Second

----------------------------------------------------------------------------
-- Plutus Bi Instances
----------------------------------------------------------------------------

instance Bi Vars.FreeVar where
    encode = genericEncode
    decode = genericDecode

instance Bi Vars.MetaVar where
    encode = genericEncode
    decode = genericDecode

instance Bi Vars.BoundVar where
    encode = genericEncode
    decode = genericDecode

instance Bi PLTypes.TyConSig where
    encode = genericEncode
    decode = genericDecode

instance Bi PLTypes.ConSig where
    encode = genericEncode
    decode = genericDecode

instance Bi a => Bi (Names.Sourced a) where
    encode = genericEncode
    decode = genericDecode

instance Bi ABT.Variable where
    encode = genericEncode
    decode = genericDecode

instance (Typeable f, Bi (f (ABT.Scope f))) => Bi (ABT.ABT f) where
    encode = genericEncode
    decode = genericDecode

instance (Typeable f, Bi (f (ABT.Scope f))) => Bi (ABT.Scope f) where
    encode = genericEncode
    decode = genericDecode

instance (Typeable r, Bi r) => Bi (PLCore.ClauseF r) where
    encode = genericEncode
    decode = genericDecode

instance Bi a => Bi (PLCore.TermF a) where
    encode = genericEncode
    decode = genericDecode

instance Bi a => Bi (PLTypes.TypeF a) where
    encode = genericEncode
    decode = genericDecode

instance Bi PLCore.SimplePattern where
    encode = genericEncode
    decode = genericDecode

instance Bi PLCore.PrimData where
    encode = genericEncode
    decode = genericDecode

instance Bi PLCore.Program where
    encode = genericEncode
    decode = genericDecode

instance Hashable PLCore.Term where
    hashWithSalt s = hashWithSalt s . serialize'

instance Hashable PLCore.Program where
    hashWithSalt s = hashWithSalt s . serialize'
