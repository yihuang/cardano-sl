{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

<<<<<<< HEAD
import           Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)
import           Control.Exception (throwIO, finally)
import           Control.Monad (forM, forM_, when)
import           Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Functor.Contravariant (contramap)
import qualified Data.Set as S
import           Data.Void (Void, absurd)
import           GHC.Generics (Generic)
import           Network.Discovery.Abstract
import qualified Network.Discovery.Transport.Kademlia as K
import           Network.Transport (Transport (..))
import qualified Network.Transport.TCP as TCP
import           Node
import           Node.Message.Binary (BinaryP, binaryPacking)
import           Pos.Util.Trace (stdoutTrace)
=======
import           Control.Exception.Safe (throwM, finally)
import           Control.Monad (forM, forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as S
import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Data.Void (Void, absurd)
import           GHC.Generics (Generic)
import           Mockable.Concurrent (ThreadId, delay, fork, killThread)
import           Mockable.Production
import           Network.Discovery.Abstract
import qualified Network.Discovery.Transport.Kademlia as K
import           Network.Transport.Abstract (Transport (..))
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP as TCP
import           Node
import           Node.Message.Binary (BinaryP, binaryPacking)
>>>>>>> CHW-82-84, orphan branch
import           System.Environment (getArgs)
import           System.Random

data Pong = Pong BS.ByteString
deriving instance Generic Pong
deriving instance Show Pong
instance Binary Pong where

type Packing = BinaryP

instance Message Void where
    messageCode _ = 0
    formatMessage = absurd

worker
    :: NodeId
    -> StdGen
<<<<<<< HEAD
    -> NetworkDiscovery K.KademliaDiscoveryErrorCode
    -> Converse Packing BS.ByteString
    -> IO ()
=======
    -> NetworkDiscovery K.KademliaDiscoveryErrorCode Production
    -> Converse Packing BS.ByteString Production
    -> Production ()
>>>>>>> CHW-82-84, orphan branch
worker anId generator discovery = pingWorker generator
    where
    pingWorker
       :: StdGen
<<<<<<< HEAD
       -> Converse Packing BS.ByteString
       -> IO ()
    pingWorker gen converse = loop gen
        where
        loop g = do
            let (us, gen') = randomR (1000,2000000) g
            threadDelay us
            _ <- knownPeers discovery
            _ <- discoverPeers discovery
            peerSet <- knownPeers discovery
            putStrLn $ show anId ++ " has peer set: " ++ show peerSet
            forM_ (S.toList peerSet) $ \addr -> converseWith converse (NodeId addr) $
                \_peerData -> Conversation $ \(cactions :: ConversationActions Void Pong) -> do
                    received <- recv cactions maxBound
                    case received of
                        Just (Pong _) -> putStrLn $ show anId ++ " heard PONG from " ++ show addr
=======
       -> Converse Packing BS.ByteString Production
       -> Production ()
    pingWorker gen converse = loop gen
        where
        loop g = do
            let (i, gen') = randomR (1000,2000000) g
                us = fromMicroseconds i :: Microsecond
            delay us
            _ <- knownPeers discovery
            _ <- discoverPeers discovery
            peerSet <- knownPeers discovery
            liftIO . putStrLn $ show anId ++ " has peer set: " ++ show peerSet
            forM_ (S.toList peerSet) $ \addr -> converseWith converse (NodeId addr) $
                \_peerData -> Conversation $ \(cactions :: ConversationActions Void Pong Production) -> do
                    received <- recv cactions maxBound
                    case received of
                        Just (Pong _) -> liftIO . putStrLn $ show anId ++ " heard PONG from " ++ show addr
>>>>>>> CHW-82-84, orphan branch
                        Nothing -> error "Unexpected end of input"
            loop gen'

listeners
    :: NodeId
    -> BS.ByteString
<<<<<<< HEAD
    -> [Listener Packing BS.ByteString]
listeners anId peerData = [pongListener]
    where
    pongListener :: Listener Packing BS.ByteString
    pongListener = Listener $ \_ peerId (cactions :: ConversationActions Pong Void) -> do
        putStrLn $ show anId ++  " heard PING from " ++ show peerId ++ " with peer data " ++ B8.unpack peerData
        send cactions (Pong "")

makeNode :: Transport
         -> Int
         -> IO ThreadId
=======
    -> [Listener Packing BS.ByteString Production]
listeners anId peerData = [pongListener]
    where
    pongListener :: Listener Packing BS.ByteString Production
    pongListener = Listener $ \_ peerId (cactions :: ConversationActions Pong Void Production) -> do
        liftIO . putStrLn $ show anId ++  " heard PING from " ++ show peerId ++ " with peer data " ++ B8.unpack peerData
        send cactions (Pong "")

makeNode :: Transport Production
         -> Int
         -> Production (ThreadId Production)
>>>>>>> CHW-82-84, orphan branch
makeNode transport i = do
    let port = 3000 + i
        host = "127.0.0.1"
        addr = (host, fromIntegral port)
        anId = makeId i
        initialPeer =
            if i == 0
            -- First node uses itself as initial peer, else it'll panic because
            -- its initial peer appears to be down.
            then K.Peer host (fromIntegral port)
            else K.Peer host (fromIntegral (port - 1))
        kademliaConfig = K.KademliaConfiguration addr addr anId
        prng1 = mkStdGen (2 * i)
        prng2 = mkStdGen ((2 * i) + 1)
<<<<<<< HEAD
    putStrLn $ "Starting node " ++ show i
    forkIO $ node (contramap snd stdoutTrace) (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
                prng1 binaryPacking (B8.pack "my peer data!") defaultNodeEnvironment $ \node' ->
        NodeAction (listeners . nodeId $ node') $ \converse -> do
            putStrLn $ "Making discovery for node " ++ show i
=======
    liftIO . putStrLn $ "Starting node " ++ show i
    fork $ node (simpleNodeEndPoint transport) (const noReceiveDelay) (const noReceiveDelay)
                prng1 binaryPacking (B8.pack "my peer data!") defaultNodeEnvironment $ \node' ->
        NodeAction (listeners . nodeId $ node') $ \converse -> do
            liftIO . putStrLn $ "Making discovery for node " ++ show i
>>>>>>> CHW-82-84, orphan branch
            discovery <- K.kademliaDiscovery kademliaConfig initialPeer (nodeEndPointAddress node')
            worker (nodeId node') prng2 discovery converse `finally` closeDiscovery discovery
    where
    makeId anId
        | anId < 10 = B8.pack ("node_identifier_0" ++ show anId)
        | otherwise = B8.pack ("node_identifier_" ++ show anId)

main :: IO ()
<<<<<<< HEAD
main = do

    args <- getArgs
=======
main = runProduction $ do

    args <- liftIO getArgs
>>>>>>> CHW-82-84, orphan branch
    number <- case args of
        [arg0] | Just number <- read arg0 -> return number
        _ -> error "Input argument must be a number"

    when (number > 99 || number < 1) $ error "Give a number in [1,99]"

    let params = TCP.defaultTCPParameters { TCP.tcpCheckPeerHost = True }
<<<<<<< HEAD
    transport <- do
        transportOrError <-
            TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" "10128") params
        either throwIO return transportOrError

    putStrLn $ "Spawning " ++ show number ++ " nodes"
    nodeThreads <- forM [0..number] (makeNode transport)

    putStrLn "Hit return to stop"
    _ <- getChar

    putStrLn "Stopping nodes"
=======
    transport_ <- do
        transportOrError <- liftIO $
            TCP.createTransport (TCP.defaultTCPAddr "127.0.0.1" "10128") params
        either throwM return transportOrError
    let transport = concrete transport_

    liftIO . putStrLn $ "Spawning " ++ show number ++ " nodes"
    nodeThreads <- forM [0..number] (makeNode transport)

    liftIO $ putStrLn "Hit return to stop"
    _ <- liftIO $ getChar

    liftIO $ putStrLn "Stopping nodes"
>>>>>>> CHW-82-84, orphan branch
    forM_ nodeThreads killThread
    closeTransport transport
