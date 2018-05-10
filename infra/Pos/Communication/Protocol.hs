{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Protocol
       ( module Pos.Communication.Types.Protocol
<<<<<<< HEAD
=======
       , hoistSendActions
>>>>>>> CHW-82-84, orphan branch
       , mapListener
       , mapListener'
       , Message (..)
       , MessageCode
       , unpackLSpecs
<<<<<<< HEAD
=======
       , hoistMkListeners
>>>>>>> CHW-82-84, orphan branch
       , makeSendActions
       , makeEnqueueMsg
       , checkProtocolMagic
       , checkingInSpecs
       , constantListeners

       -- * OnNewSlot constraints
       , LocalOnNewSlotComm
       , OnNewSlotComm
       ) where

import           Universum

<<<<<<< HEAD
import qualified Control.Concurrent.STM as STM
import           Control.Exception (throwIO)
=======
>>>>>>> CHW-82-84, orphan branch
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Buildable as B
import           Formatting (bprint, build, sformat, (%))
import           Mockable (Async, Delay, Mockable, Mockables, SharedAtomic)
<<<<<<< HEAD
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Node as N
import           Node.Message.Class (Message (..), MessageCode, messageCode)
import           Serokell.Util.Text (listJson)
import           Pos.Util.Trace (Trace, Severity (..), traceWith)

import           Pos.Communication.Types.Protocol
=======
import qualified Node as N
import           Node.Message.Class (Message (..), MessageCode, messageCode)
import           Serokell.Util.Text (listJson)
import           System.Wlog (WithLogger, logWarning)

import           Pos.Communication.Types.Protocol
import           Pos.Core.Configuration (HasConfiguration)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Recovery.Info (MonadRecoveryInfo)
import           Pos.Reporting (MonadReporting)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Slotting (MonadSlots)

mapListener
<<<<<<< HEAD
    :: (forall t. IO t -> IO t) -> Listener -> Listener
=======
    :: (forall t. m t -> m t) -> Listener m -> Listener m
>>>>>>> CHW-82-84, orphan branch
mapListener = mapListener' $ const identity

mapListener'
    :: (forall snd rcv. Message rcv => N.NodeId
<<<<<<< HEAD
          -> N.ConversationActions snd rcv
          -> N.ConversationActions snd rcv)
    -> (forall t. IO t -> IO t) -> Listener -> Listener
mapListener' caMapper mapper (N.Listener f) =
    N.Listener $ \d nId -> mapper . f d nId . caMapper nId

makeEnqueueMsg
    :: Trace IO (Severity, Text)
    -> VerInfo
    -> (forall t . Msg -> (NodeId -> VerInfo -> N.Conversation PackingType t) -> IO (Map NodeId (STM.TVar (OQ.PacketStatus t))))
    -> EnqueueMsg
makeEnqueueMsg logTrace ourVerInfo enqueue = \msg mkConv -> enqueue msg $ \nodeId pVI ->
    alternativeConversations logTrace nodeId ourVerInfo pVI (mkConv nodeId pVI)

alternativeConversations
    :: Trace IO (Severity, Text)
    -> NodeId
    -> VerInfo -- ^ Ours
    -> VerInfo -- ^ Theirs
    -> NonEmpty (Conversation t)
    -> N.Conversation PackingType t
alternativeConversations logTrace nid ourVerInfo theirVerInfo convs
    -- FIXME seems dubious: we also have checkProtocolMagic.
    -- Do we need both?
    -- Well, in good faith, we don't need either, since the network should not
    -- be aware of protocol magic.
=======
          -> N.ConversationActions snd rcv m
          -> N.ConversationActions snd rcv m)
    -> (forall t. m t -> m t) -> Listener m -> Listener m
mapListener' caMapper mapper (N.Listener f) =
    N.Listener $ \d nId -> mapper . f d nId . caMapper nId

hoistSendActions
    :: forall n m .
       ( Functor m )
    => (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> SendActions n
    -> SendActions m
hoistSendActions nat rnat SendActions {..} = SendActions withConnectionTo' enqueueMsg''
  where
    withConnectionTo'
        :: forall t . NodeId -> (PeerData -> NonEmpty (Conversation m t)) -> m t
    withConnectionTo' nodeId k =
        nat $ withConnectionTo nodeId $ \peerData ->
            flip map (k peerData) $ \(Conversation l) ->
                Conversation $ \cactions ->
                    rnat (l (N.hoistConversationActions nat cactions))

    enqueueMsg''
        :: forall t .
           Msg
        -> (NodeId -> PeerData -> NonEmpty (Conversation m t))
        -> m (Map NodeId (m t))
    enqueueMsg'' msg k = (fmap . fmap) nat $
        nat $ enqueueMsg msg $ \peer pVI ->
            let convs = k peer pVI
                convert (Conversation l) = Conversation $ \cactions ->
                    rnat (l (N.hoistConversationActions nat cactions))
            in  map convert convs

hoistMkListeners
    :: (forall a. m a -> n a)
    -> (forall a. n a -> m a)
    -> MkListeners m
    -> MkListeners n
hoistMkListeners nat rnat (MkListeners act ins outs) = MkListeners act' ins outs
  where
    act' v p = let ls = act v p in map (N.hoistListener nat rnat) ls

makeEnqueueMsg
    :: forall m .
       ( WithLogger m
       , MonadThrow m
       )
    => VerInfo
    -> (forall t . Msg -> (NodeId -> VerInfo -> N.Conversation PackingType m t) -> m (Map NodeId (m t)))
    -> EnqueueMsg m
makeEnqueueMsg ourVerInfo enqueue = \msg mkConv -> enqueue msg $ \nodeId pVI ->
    alternativeConversations nodeId ourVerInfo pVI (mkConv nodeId pVI)

alternativeConversations
    :: forall m t .
       ( WithLogger m
       , MonadThrow m
       )
    => NodeId
    -> VerInfo -- ^ Ours
    -> VerInfo -- ^ Theirs
    -> NonEmpty (Conversation m t)
    -> N.Conversation PackingType m t
alternativeConversations nid ourVerInfo theirVerInfo convs
>>>>>>> CHW-82-84, orphan branch
    | vIMagic ourVerInfo /= vIMagic theirVerInfo =
        throwErrs (one $ MismatchedProtocolMagic (vIMagic ourVerInfo) (vIMagic theirVerInfo)) (NE.head convs)
    | otherwise =
        let alts = map (checkingOutSpecs' nid (vIInHandlers theirVerInfo)) convs
        in  case sequence alts of
                Left (Conversation l) -> N.Conversation $ \conv -> do
                    mapM_ logOSNR alts
                    l conv
                Right errs -> throwErrs errs (NE.head convs)
  where

    ourOutSpecs = vIOutHandlers ourVerInfo

    throwErrs
        :: forall e x .
           ( Exception e, Buildable e )
        => NonEmpty e
<<<<<<< HEAD
        -> Conversation x
        -> N.Conversation PackingType x
    throwErrs errs (Conversation l) = N.Conversation $ \conv -> do
        let _ = l conv
        traceWith logTrace (Warning, sformat ("Failed to choose appropriate conversation: "%listJson) errs)
        throwIO $ NE.head errs
=======
        -> Conversation m x
        -> N.Conversation PackingType m x
    throwErrs errs (Conversation l) = N.Conversation $ \conv -> do
        let _ = l conv
        logWarning $ sformat
            ("Failed to choose appropriate conversation: "%listJson)
            errs
        throwM $ NE.head errs
>>>>>>> CHW-82-84, orphan branch

    fstArg :: (a -> b) -> Proxy a
    fstArg _ = Proxy

<<<<<<< HEAD
    logOSNR (Right e@(OutSpecNotReported _ _)) = traceWith logTrace (Warning, sformat build e)
=======
    logOSNR (Right e@(OutSpecNotReported _ _)) = logWarning $ sformat build e
>>>>>>> CHW-82-84, orphan branch
    logOSNR _                                  = pure ()

    checkingOutSpecs' nodeId peerInSpecs conv@(Conversation h) =
        checkingOutSpecs (sndMsgCode, ConvHandler rcvMsgCode) nodeId peerInSpecs conv
      where
        sndMsgCode = messageCode . sndProxy $ fstArg h
        rcvMsgCode = messageCode . rcvProxy $ fstArg h

    -- This is kind of last resort, in general we should handle conversation
    --    to be supported by external peer on higher level
    checkingOutSpecs spec nodeId peerInSpecs action =
        if | spec `notInSpecs` ourOutSpecs ->
                  Right $ OutSpecNotReported ourOutSpecs spec
           | spec `notInSpecs` peerInSpecs ->
                  Right $ PeerInSpecNotReported peerInSpecs nodeId spec
           | otherwise -> Left action

makeSendActions
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> VerInfo
    -> (forall t .
            Msg
        -> (NodeId -> VerInfo -> N.Conversation PackingType t)
        -> IO (Map NodeId (STM.TVar (OQ.PacketStatus t)))
       )
    -> Converse PackingType PeerData
    -> SendActions
makeSendActions logTrace ourVerInfo enqueue converse = SendActions
    { withConnectionTo = \nodeId mkConv -> N.converseWith converse nodeId $ \pVI ->
          alternativeConversations logTrace nodeId ourVerInfo pVI (mkConv pVI)
    , enqueueMsg = \msg mkConv -> waitForDequeues <$> (makeEnqueueMsg logTrace ourVerInfo enqueue msg mkConv)
=======
    :: forall m .
       ( WithLogger m
       , MonadThrow m
       )
    => VerInfo
    -> (forall t . Msg -> (NodeId -> VerInfo -> N.Conversation PackingType m t) -> m (Map NodeId (m t)))
    -> Converse PackingType PeerData m
    -> SendActions m
makeSendActions ourVerInfo enqueue converse = SendActions
    { withConnectionTo = \nodeId mkConv -> N.converseWith converse nodeId $ \pVI ->
          alternativeConversations nodeId ourVerInfo pVI (mkConv pVI)
    , enqueueMsg = makeEnqueueMsg ourVerInfo enqueue
>>>>>>> CHW-82-84, orphan branch
    }

data SpecError
    = OutSpecNotReported HandlerSpecs (MessageCode, HandlerSpec)
    | PeerInSpecNotReported HandlerSpecs NodeId (MessageCode, HandlerSpec)
    deriving (Generic, Show)

instance Exception SpecError

instance Buildable SpecError where
    build (OutSpecNotReported outSpecs spec) =
        bprint
          ("Sending "%build%": endpoint not reported to be used for sending. Our out specs: "%build)
          spec outSpecs
    build (PeerInSpecNotReported inSpecs nodeId spec) =
        bprint
          ("Attempting to send to "%build%": endpoint unsupported by peer "%build%". In specs: "%build)
          spec nodeId inSpecs

data MismatchedProtocolMagic
    = MismatchedProtocolMagic Int32 Int32
    deriving (Generic, Show)

instance Exception MismatchedProtocolMagic

instance Buildable MismatchedProtocolMagic where
    build (MismatchedProtocolMagic ourMagic theirMagic) =
        bprint
          ("Mismatched protocolMagic, our: "%build%", their: "%build) ourMagic theirMagic


type LocalOnNewSlotComm ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , MonadSlots ctx m
    , MonadMask m
<<<<<<< HEAD
=======
    , WithLogger m
>>>>>>> CHW-82-84, orphan branch
    , Mockables m [Async, Delay]
    , MonadReporting ctx m
    , HasShutdownContext ctx
    , MonadRecoveryInfo m
<<<<<<< HEAD
=======
    , HasConfiguration
>>>>>>> CHW-82-84, orphan branch
    )

type OnNewSlotComm ctx m =
    ( LocalOnNewSlotComm ctx m
    , MonadThrow m
    , Mockable SharedAtomic m
<<<<<<< HEAD
    )

-- FIXME network layer is not concerned with this.
-- The protocol magic should not even be known to the diffusion layer.
checkProtocolMagic
    :: VerInfo
    -> VerInfo
    -> IO ()
    -> IO ()
=======
    , HasConfiguration
    )

checkProtocolMagic
    :: WithLogger m
    => VerInfo
    -> VerInfo
    -> m ()
    -> m ()
>>>>>>> CHW-82-84, orphan branch
checkProtocolMagic (vIMagic -> ourMagic) (vIMagic -> theirMagic) action
    -- Check that protocolMagic is the same
    | ourMagic == theirMagic = action
    | otherwise =
<<<<<<< HEAD
        throwIO $ MismatchedProtocolMagic ourMagic theirMagic

checkingInSpecs
    :: Trace IO (Severity, Text)
    -> VerInfo
    -> VerInfo
    -> (MessageCode, HandlerSpec)
    -> NodeId
    -> IO ()
    -> IO ()
checkingInSpecs logTrace ourVerInfo peerVerInfo' spec nodeId action =
    if | spec `notInSpecs` vIInHandlers ourVerInfo ->
              traceWith logTrace (Warning, sformat ("Endpoint is served, but not reported " % build) spec)
       | spec `notInSpecs` vIOutHandlers peerVerInfo' ->
              traceWith logTrace (Warning, sformat ("Peer " % build % " attempting to use endpoint he didn't report to use " % build) nodeId spec)
       | otherwise -> action

rcvProxy :: Proxy (ConversationActions snd rcv) -> Proxy rcv
rcvProxy _ = Proxy

sndProxy :: Proxy (ConversationActions snd rcv) -> Proxy snd
sndProxy _ = Proxy

-- Provides set of listeners which doesn't depend on PeerData
constantListeners :: [(ListenerSpec, OutSpecs)] -> MkListeners
=======
        logWarning $ sformat ("Mismatched protocolMagic, our: "%build%", their: "%build) ourMagic theirMagic

checkingInSpecs
    :: WithLogger m
    => VerInfo
    -> VerInfo
    -> (MessageCode, HandlerSpec)
    -> NodeId
    -> m ()
    -> m ()
checkingInSpecs ourVerInfo peerVerInfo' spec nodeId action =
    if | spec `notInSpecs` vIInHandlers ourVerInfo ->
              logWarning $ sformat
                ("Endpoint is served, but not reported " % build) spec
       | spec `notInSpecs` vIOutHandlers peerVerInfo' ->
              logWarning $ sformat
                ("Peer " % build % " attempting to use endpoint he didn't report to use " % build)
                nodeId spec
       | otherwise -> action

rcvProxy :: Proxy (ConversationActions snd rcv m) -> Proxy rcv
rcvProxy _ = Proxy

sndProxy :: Proxy (ConversationActions snd rcv m) -> Proxy snd
sndProxy _ = Proxy

-- Provides set of listeners which doesn't depend on PeerData
constantListeners :: [(ListenerSpec m, OutSpecs)] -> MkListeners m
>>>>>>> CHW-82-84, orphan branch
constantListeners = toMkL . unpackLSpecs . second mconcat . unzip
  where
    toMkL (lGet, ins, outs) = MkListeners (\vI _ -> lGet vI) ins outs

<<<<<<< HEAD
unpackLSpecs :: ([ListenerSpec], OutSpecs) -> (VerInfo -> [Listener], InSpecs, OutSpecs)
=======
unpackLSpecs :: ([ListenerSpec m], OutSpecs) -> (VerInfo -> [Listener m], InSpecs, OutSpecs)
>>>>>>> CHW-82-84, orphan branch
unpackLSpecs =
    over _1 (\ls verInfo -> fmap ($ verInfo) ls) .
    over _2 (InSpecs . HM.fromList) .
    convert . first (map lsToPair)
  where
    lsToPair (ListenerSpec h spec) = (h, spec)
    convert :: ([(l, i)], out) -> ([l], [i], out)
    convert (xs, out) = (map fst xs, map snd xs, out)
