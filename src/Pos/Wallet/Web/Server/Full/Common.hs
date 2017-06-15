{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Module containing common parts for all wallet modes.

module Pos.Wallet.Web.Server.Full.Common
       ( nat
       , convertHandler
       ) where

import           Universum

import qualified Control.Monad.Catch           as Catch
import           Control.Monad.Except          (MonadError (throwError))
import qualified Control.Monad.Reader          as Mtl
import qualified Ether
import           Mockable                      (runProduction)
import           Servant.Server                (Handler)
import           Servant.Utils.Enter           ((:~>) (..))

import           Pos.Communication.PeerState   (PeerStateSnapshot, WithPeerState (..),
                                                getAllStates, peerStateFromSnapshot)
import           Pos.Context                   (NodeContext, NodeContextTag)
import           Pos.DB                        (NodeDBs, getNodeDBs)
import           Pos.Delegation.Class          (DelegationVar, askDelegationState)
import           Pos.Ssc.Extra                 (SscState)
import           Pos.Ssc.Extra.Class           (askSscMem)
import           Pos.Txp                       (GenericTxpLocalData, askTxpMem)
import           Pos.Util.JsonLog              (JsonLogConfig (..))
import           Pos.Wallet.SscType            (WalletSscType)
import           Pos.Wallet.Web.Server.Sockets (ConnectionsVar, getWalletWebSockets)
import           Pos.Wallet.Web.State          (WalletState, getWalletWebState)

import           Pos.Wallet.Web.Mode           (WalletWebMode, WalletWebModeContext (..),
                                                unWalletWebMode)
import           Pos.WorkMode                  (RealModeContext (..), TxpExtra_TMP)


nat :: WalletWebMode (WalletWebMode :~> Handler)
nat = do
    ws         <- getWalletWebState
    tlw        <- askTxpMem
    ssc        <- askSscMem
    delWrap    <- askDelegationState
    psCtx      <- getAllStates
    nc         <- Ether.ask @NodeContextTag
    modernDB   <- getNodeDBs
    conn       <- getWalletWebSockets
    pure $ NT (convertHandler nc modernDB tlw ssc ws delWrap
                              psCtx conn)

convertHandler
    :: NodeContext WalletSscType              -- (.. insert monad `m` here ..)
    -> NodeDBs
    -> GenericTxpLocalData TxpExtra_TMP
    -> SscState WalletSscType
    -> WalletState
    -> DelegationVar
    -> PeerStateSnapshot
    -> ConnectionsVar
    -> WalletWebMode a
    -> Handler a
convertHandler nc modernDBs tlw ssc ws delWrap psCtx
               conn handler =
    liftIO (walletRunner handler) `Catch.catches` excHandlers
  where

    walletRunner :: forall a . WalletWebMode a -> IO a
    walletRunner act = runProduction $ do
        peerStateCtx <- peerStateFromSnapshot psCtx
        Mtl.runReaderT (unWalletWebMode act) $
            WalletWebModeContext
                ws
                conn
                (RealModeContext
                    modernDBs
                    ssc
                    tlw
                    delWrap
                    peerStateCtx
                    JsonLogDisabled
                    "wallet-api"
                    nc)

    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError
