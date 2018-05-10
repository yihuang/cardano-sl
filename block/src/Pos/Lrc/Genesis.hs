-- | Computation of LRC genesis data.

module Pos.Lrc.Genesis
    ( genesisLeaders
    ) where

import           Universum

import qualified Data.HashMap.Strict as HM

<<<<<<< HEAD
import           Pos.Core (GenesisData (..), SharedSeed (..), SlotLeaders, genesisData, HasGenesisData, HasProtocolConstants, HasGeneratedSecrets)
=======
import           Pos.Core (GenesisData (..), HasConfiguration, SharedSeed (..), SlotLeaders, genesisData)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Lrc.Fts (followTheSatoshi)
import           Pos.Txp.GenesisUtxo (genesisUtxo)
import           Pos.Txp.Toil (GenesisUtxo (..), Utxo, utxoToStakes)


-- | Compute leaders of the 0-th epoch from initial shared seed and stake distribution.
<<<<<<< HEAD
genesisLeaders :: (HasGenesisData, HasGeneratedSecrets, HasProtocolConstants) => SlotLeaders
=======
genesisLeaders :: HasConfiguration => SlotLeaders
>>>>>>> CHW-82-84, orphan branch
genesisLeaders = followTheSatoshiUtxo (gdFtsSeed genesisData) utxo
  where
    GenesisUtxo utxo = genesisUtxo

-- This should not be exported unless it is *needed* elsewhere
followTheSatoshiUtxo ::
<<<<<<< HEAD
       (HasGenesisData, HasGeneratedSecrets, HasProtocolConstants)
=======
       HasConfiguration
>>>>>>> CHW-82-84, orphan branch
    => SharedSeed
    -> Utxo
    -> SlotLeaders
followTheSatoshiUtxo seed utxo =
    followTheSatoshi seed $ HM.toList $ utxoToStakes utxo
