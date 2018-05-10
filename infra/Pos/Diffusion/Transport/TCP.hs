-- | Convenient TCP transport acquire and release with common configuration
-- options.

module Pos.Diffusion.Transport.TCP
    ( bracketTransportTCP
    ) where

<<<<<<< HEAD
import           Universum hiding (bracket)

import           Control.Exception (bracket, throwIO)
import           Data.Time.Units (Microsecond)
import           Formatting (sformat, shown, (%))

import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT
import qualified Network.Transport.TCP as TCP
import           Pos.Util.Trace (Trace, traceWith)

-- | Bracket a TCP transport with
--
--   - Given connection timeout in us
--   - Given address (possibly unaddressable)
--   - A fair QDisc
--   - Check the peer host against resolved host (prevents easy denial-of-service)
--   - Do not crash the server if 'accept' fails; instead, use the given
--     'Trace' to log the reason and continue trying to accept new connections
bracketTransportTCP
    :: Trace IO Text
    -> Microsecond
    -> TCP.TCPAddr
    -> (NT.Transport -> IO a)
    -> IO a
bracketTransportTCP logTrace connectionTimeout tcpAddr k = bracket
    (createTransportTCP logTrace connectionTimeout tcpAddr)
    NT.closeTransport
    k

createTransportTCP
    :: Trace IO Text -- ^ Whenever there's an error accepting a new connection.
    -> Microsecond   -- ^ Connection timeout
    -> TCP.TCPAddr
    -> IO NT.Transport
createTransportTCP logTrace connectionTimeout addrInfo = do
=======
import           Universum

import           Data.Time.Units (Microsecond)
import           Formatting (sformat, shown, (%))
import           System.Wlog (WithLogger, logError, usingLoggerName, askLoggerName)

import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT (closeTransport)
import           Network.Transport.Abstract (Transport)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP as TCP

bracketTransportTCP
    :: ( MonadIO m
       , MonadIO n
       , MonadThrow m
       , MonadMask m
       , WithLogger m
       )
    => Microsecond
    -> TCP.TCPAddr
    -> (Transport n -> m a)
    -> m a
bracketTransportTCP connectionTimeout tcpAddr k = bracket
    (createTransportTCP connectionTimeout tcpAddr)
    snd
    (k . fst)

createTransportTCP
    :: ( MonadIO n
       , MonadIO m
       , WithLogger m
       , MonadThrow m
       )
    => Microsecond -- ^ Connection timeout
    -> TCP.TCPAddr
    -> m (Transport n, m ())
createTransportTCP connectionTimeout addrInfo = do
    loggerName <- askLoggerName
>>>>>>> CHW-82-84, orphan branch
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral connectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = \e ->
<<<<<<< HEAD
                   traceWith logTrace (sformat ("Exception in tcp server: " % shown) e)
             })
    TCP.createTransport addrInfo tcpParams >>= either throwIO pure
=======
                     usingLoggerName (loggerName <> "transport") $
                         logError $ sformat ("Exception in tcp server: " % shown) e
             })
    transportE <- liftIO $ TCP.createTransport addrInfo tcpParams
    case transportE of
        Left e -> do
            logError $ sformat ("Error creating TCP transport: " % shown) e
            throwM e
        Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)
>>>>>>> CHW-82-84, orphan branch
