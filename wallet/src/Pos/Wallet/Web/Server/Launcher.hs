{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Server launcher

module Pos.Wallet.Web.Server.Launcher
       ( walletApplication
       , walletServer
       , walletServeImpl
       , walletServerOuts
       , walletDocumentationImpl

       , bracketWalletWebDB
       , bracketWalletWS
       ) where

import           Universum

import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (Settings)
import           Serokell.AcidState.ExtendedState (ExtendedState)
import           Servant.Server (Handler, Server, serve)

import qualified Data.ByteString.Char8 as BS8

import           Ntp.Client (NtpStatus)

import           Pos.Client.Txp.Network (sendTxOuts)
import           Pos.Communication (OutSpecs)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (sendTx))
import           Pos.Infra.Util.TimeWarp (NetworkAddress)
import           Pos.Util (bracketWithTrace)
import           Pos.Util.CompileInfo (HasCompileInfo)
--import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, logInfo)
import           Pos.Wallet.Web.Account (findKey, myRootAddresses)
import           Pos.Wallet.Web.Api (WalletSwaggerApi, swaggerWalletApi)
import           Pos.Wallet.Web.Mode (MonadFullWalletWebMode,
                     MonadWalletWebMode, MonadWalletWebSockets)
import           Pos.Wallet.Web.Server.Handlers (servantHandlersWithSwagger)
import           Pos.Wallet.Web.Sockets (ConnectionsVar, closeWSConnections,
                     getWalletWebSockets, initWSConnections,
                     upgradeApplicationWS)
import           Pos.Wallet.Web.State (closeState, openState)
import           Pos.Wallet.Web.State.Storage (WalletStorage)
import           Pos.Wallet.Web.Tracking (syncWallet)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)
import           Pos.Web (TlsParams, serveDocImpl, serveImpl)

-- TODO [CSM-407]: Mixture of logic seems to be here

walletServeImpl
    :: (MonadIO m)
    => m Application     -- ^ Application getter
    -> NetworkAddress    -- ^ IP and port to listen
    -> Maybe TlsParams
    -> Maybe Settings
    -> Maybe (Word16 -> IO ())
    -> m ()
walletServeImpl app (ip, port) = serveImpl app (BS8.unpack ip) port

walletDocumentationImpl
    :: (MonadIO m)
    => m Application     -- ^ Application getter
    -> NetworkAddress    -- ^ IP and port to listen
    -> Maybe TlsParams
    -> Maybe Settings
    -> Maybe (Word16 -> IO ())
    -> m ()
walletDocumentationImpl app (ip, port) = serveDocImpl app (BS8.unpack ip) port

walletApplication
    :: ( MonadWalletWebMode ctx m
       , MonadWalletWebSockets ctx m)
    => TraceNamed IO
    -> m (Server (WalletSwaggerApi (TraceNamed m)))
    -> m Application
walletApplication logTrace serv = do
    wsConn <- getWalletWebSockets
    upgradeApplicationWS logTrace wsConn . serve (swaggerWalletApi logTrace) <$> serv

walletServer
    :: forall ctx m.
       ( MonadFullWalletWebMode ctx m, HasCompileInfo )
    => TraceNamed m
    -> ProtocolMagic
    -> Diffusion m
    -> TVar NtpStatus
    -> (forall x. m x -> Handler x)
    -> m (Server (WalletSwaggerApi (TraceNamed m)))
walletServer logTrace pm diffusion ntpStatus nat = do
    mapM_ (findKey >=> syncWallet . eskToWalletDecrCredentials) =<< myRootAddresses
    return $ servantHandlersWithSwagger logTrace pm ntpStatus submitTx nat
  where
    -- Diffusion layer takes care of submitting transactions.
    submitTx = sendTx diffusion

bracketWalletWebDB
    :: ( MonadIO m
       , MonadMask m
       )
    => TraceNamed m
    -> FilePath  -- ^ Path to wallet acid-state
    -> Bool      -- ^ Rebuild flag for acid-state
    -> (ExtendedState WalletStorage -> m a)
    -> m a
bracketWalletWebDB logTrace daedalusDbPath dbRebuild =
    bracketWithTrace logTrace msg (openState dbRebuild daedalusDbPath) closeState
  where
    msg = "bracketWalletWebDB"

bracketWalletWS
    :: ( MonadIO m
       , MonadMask m
       )
    => TraceNamed m
    -> (ConnectionsVar -> m a)
    -> m a
bracketWalletWS logTrace = bracketWithTrace logTrace msg initWS closeWSConnections
  where
    initWS = logInfo logTrace "walletServeImpl initWsConnection" >> initWSConnections
    msg = "bracketWalletWS"

walletServerOuts :: OutSpecs
walletServerOuts = sendTxOuts
