{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Diffusion.Full.Block
    ( getBlocks
    , requestTip
    , announceBlockHeader
    , handleHeadersCommunication

    , blockListeners
    ) where

import           Universum

<<<<<<< HEAD
import qualified Control.Concurrent.STM as Conc
import           Control.Exception (Exception (..), throwIO)
=======
import           Control.Exception.Safe (Exception (..))
>>>>>>> CHW-82-84, orphan branch
import           Control.Lens (to)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text.Buildable as B
<<<<<<< HEAD
import           Formatting (bprint, build, int, sformat, shown, stext, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Serokell.Util.Text (listJson)
=======
import           Data.Time.Units (toMicroseconds, fromMicroseconds)
import           Formatting (bprint, build, int, sformat, shown, stext, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Serokell.Util.Text (listJson)
import           System.Wlog (logDebug, logWarning)
>>>>>>> CHW-82-84, orphan branch

import           Pos.Binary.Communication ()
import           Pos.Block.Network (MsgBlock (..), MsgGetBlocks (..), MsgGetHeaders (..),
                                    MsgHeaders (..))
import           Pos.Communication.Listener (listenerConv)
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (mlMsgGetBlocks, mlMsgHeaders, mlMsgBlock,
                                           mlMsgGetHeaders)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, ListenerSpec, MkListeners (..),
                                             MsgType (..), NodeId, Origin (..), OutSpecs,
                                             constantListeners, waitForConversations,
<<<<<<< HEAD
                                             waitForDequeues, recvLimited)
import           Pos.Core (BlockVersionData, HeaderHash, ProtocolConstants (..),
                           headerHash, bvdSlotDuration, prevBlockL)
import           Pos.Core.Block (Block, BlockHeader (..), MainBlockHeader, blockHeader)
import           Pos.Crypto (shortHashF)
import           Pos.DB (DBError (DBMalformed))
=======
                                             recvLimited)
import           Pos.Core (BlockVersionData, HeaderHash, ProtocolConstants (..), bvdSlotDuration,
                           headerHash, prevBlockL)
import           Pos.Core.Block (Block, BlockHeader (..), MainBlockHeader, blockHeader)
import           Pos.Crypto (shortHashF)
import           Pos.DB (DBError (DBMalformed))
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Exception (cardanoExceptionFromException, cardanoExceptionToException)
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (Bucket)
-- Dubious having this security stuff in here.
import           Pos.Security.Params (AttackTarget (..), AttackType (..), NodeAttackedError (..),
                                      SecurityParams (..))
import           Pos.Util (_neHead, _neLast)
<<<<<<< HEAD
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..),
                                  toOldestFirst, _NewestFirst, _OldestFirst)
import           Pos.Util.Timer (Timer, startTimer)
import           Pos.Util.TimeWarp (NetworkAddress, nodeIdToAddress)
import           Pos.Util.Trace (Trace, Severity (..), traceWith)
=======
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..), nonEmptyNewestFirst,
                                  toOldestFirst, _NewestFirst, _OldestFirst)
import           Pos.Util.Timer (Timer, setTimerDuration, startTimer)
import           Pos.Util.TimeWarp (NetworkAddress, nodeIdToAddress)
>>>>>>> CHW-82-84, orphan branch

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

----------------------------------------------------------------------------
-- Exceptions
----------------------------------------------------------------------------

data BlockNetLogicException
    = DialogUnexpected Text
      -- ^ Node's response in any network/block related logic was
      -- unexpected.
    | BlockNetLogicInternal Text
      -- ^ We don't expect this to happen. Most probably it's internal
      -- logic error.
    deriving (Show)

instance B.Buildable BlockNetLogicException where
    build e = bprint ("BlockNetLogicException: "%shown) e

instance Exception BlockNetLogicException where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

----------------------------------------------------------------------------
-- Networking
----------------------------------------------------------------------------

-- | Expects sending message to exactly one node. Receives result or
-- fails if no result was obtained (no nodes available, timeout, etc).
<<<<<<< HEAD
enqueueMsgSingle
    :: (t2 -> (t1 -> t -> NonEmpty x) -> IO (Map NodeId (Conc.TVar (OQ.PacketStatus b))))
    -> t2
    -> x
    -> IO b
enqueueMsgSingle enqueue msg conv = do
    results <- enqueue msg (\_ _ -> one conv) >>= waitForConversations . waitForDequeues
    case toList results of
        [] ->      liftIO $ throwIO $ DialogUnexpected $
            "enqueueMsgSingle: contacted no peers"
        (_:_:_) -> liftIO $ throwIO $ DialogUnexpected $
=======
enqueueMsgSingle ::
       ( MonadThrow m )
    => (t2 -> (t1 -> t -> NonEmpty x) -> m (Map NodeId (m b)))
    -> t2
    -> x
    -> m b
enqueueMsgSingle enqueue msg conv = do
    results <- enqueue msg (\_ _ -> one conv) >>= waitForConversations
    case toList results of
        [] ->      throwM $ DialogUnexpected $
            "enqueueMsgSingle: contacted no peers"
        (_:_:_) -> throwM $ DialogUnexpected $
>>>>>>> CHW-82-84, orphan branch
            "enqueueMsgSingle: contacted more than one peers, probably internal error"
        [x] -> pure x

-- | Get some blocks from the network.
-- No verification is done
getBlocks
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> Word -- ^ Historical: limit on how many headers you can get back... always 2200
    -> EnqueueMsg
    -> NodeId
    -> HeaderHash
    -> [HeaderHash]
    -> IO (OldestFirst [] Block)
getBlocks logTrace logic recoveryHeadersMessage enqueue nodeId tipHeaderHash checkpoints = do
=======
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> Word -- ^ Historical: limit on how many headers you can get back... always 2200
    -> EnqueueMsg d
    -> NodeId
    -> BlockHeader
    -> [HeaderHash]
    -> d (OldestFirst [] Block)
getBlocks logic recoveryHeadersMessage enqueue nodeId tipHeader checkpoints = do
>>>>>>> CHW-82-84, orphan branch
    -- It is apparently an error to request headers for the tipHeader and
    -- [tipHeader], i.e. 1 checkpoint equal to the header of the block that
    -- you want. Sure, it's a silly thing to do, but should it be an error?
    --
    -- Anyway, the procedure was and still is: if it's just one block you want,
    -- then you can skip requesting the headers and go straight to requesting
    -- the block itself.
    bvd <- getAdoptedBVData logic
    blocks <- if singleBlockHeader
<<<<<<< HEAD
              then requestBlocks bvd (OldestFirst (one tipHeaderHash))
              else requestAndClassifyHeaders bvd >>= requestBlocks bvd . fmap headerHash
    pure (OldestFirst (reverse (toList blocks)))
  where

    requestAndClassifyHeaders :: BlockVersionData -> IO (OldestFirst [] BlockHeader)
    requestAndClassifyHeaders bvd = do
        OldestFirst headers <- toOldestFirst <$> requestHeaders bvd
        -- Logic layer gives us the suffix of the chain that we don't have.
        -- Possibly empty.
        -- 'requestHeaders' gives a NonEmpty; we drop it to a [].
        getLcaMainChain logic (OldestFirst (toList headers))

    singleBlockHeader :: Bool
    singleBlockHeader = case checkpoints of
        [checkpointHash] -> checkpointHash == tipHeaderHash
=======
              then requestBlocks bvd (OldestFirst (one tipHeader))
              else requestAndClassifyHeaders bvd >>= requestBlocks bvd
    pure (OldestFirst (reverse (toList blocks)))
  where

    requestAndClassifyHeaders :: BlockVersionData -> d (OldestFirst NE BlockHeader)
    requestAndClassifyHeaders bvd = do
        headers <- toOldestFirst <$> requestHeaders bvd
        getLcaMainChain logic headers >>= \case
            Nothing -> throwM $ DialogUnexpected $ "Got headers, but couldn't compute " <>
                                                   "LCA to ask for blocks"
            Just (lca :: HeaderHash) -> do
                -- Headers list is (oldest to newest)
                -- [n1,n2,...nj,lca,nj+2,...nk] we drop [n1..lca] and
                -- return [nj+2..nk], as we already have lca in our
                -- local db. Usually this function does 1 iterations
                -- as it's a common case that [n1..nj] is absent and
                -- lca is the oldest header.
                let dropUntilLca = NE.dropWhile (\h -> h ^. prevBlockL /= lca)
                case nonEmpty (dropUntilLca $ getOldestFirst headers) of
                    Nothing -> throwM $ DialogUnexpected $
                                   "All headers are older than LCA, nothing to query"
                    Just headersSuffix -> pure (OldestFirst headersSuffix)

    singleBlockHeader :: Bool
    singleBlockHeader = case checkpoints of
        [checkpointHash] -> checkpointHash == tipHash
>>>>>>> CHW-82-84, orphan branch
        _                -> False
    mgh :: MsgGetHeaders
    mgh = MsgGetHeaders
        { mghFrom = checkpoints
<<<<<<< HEAD
        , mghTo = Just tipHeaderHash
        }

=======
        , mghTo = Just tipHash
        }

    tipHash :: HeaderHash
    tipHash = headerHash tipHeader

>>>>>>> CHW-82-84, orphan branch
    -- | Make message which requests chain of blocks which is based on our
    -- tip. LcaChild is the first block after LCA we don't
    -- know. WantedBlock is the newest one we want to get.
    mkBlocksRequest :: HeaderHash -> HeaderHash -> MsgGetBlocks
    mkBlocksRequest lcaChild wantedBlock =
        MsgGetBlocks
        { mgbFrom = lcaChild
        , mgbTo = wantedBlock
        }

<<<<<<< HEAD
    requestHeaders :: BlockVersionData -> IO (NewestFirst NE BlockHeader)
=======
    requestHeaders :: BlockVersionData -> d (NewestFirst NE BlockHeader)
>>>>>>> CHW-82-84, orphan branch
    requestHeaders bvd = enqueueMsgSingle
        enqueue
        (MsgRequestBlockHeaders (Just (S.singleton nodeId)))
        (Conversation (requestHeadersConversation bvd))

    requestHeadersConversation
        :: BlockVersionData
<<<<<<< HEAD
        -> ConversationActions MsgGetHeaders MsgHeaders
        -> IO (NewestFirst NE BlockHeader)
    requestHeadersConversation bvd conv = do
        traceWith logTrace (Debug, sformat ("requestHeaders: sending "%build) mgh)
=======
        -> ConversationActions MsgGetHeaders MsgHeaders d
        -> d (NewestFirst NE BlockHeader)
    requestHeadersConversation bvd conv = do
        logDebug $ sformat ("requestHeaders: sending "%build) mgh
>>>>>>> CHW-82-84, orphan branch
        send conv mgh
        mHeaders <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
        inRecovery <- recoveryInProgress logic
        -- TODO: it's very suspicious to see False here as RequestHeaders
        -- is only called when we're in recovery mode.
<<<<<<< HEAD
        traceWith logTrace (Debug, sformat ("requestHeaders: inRecovery = "%shown) inRecovery)
        case mHeaders of
            Nothing -> do
                traceWith logTrace (Warning, "requestHeaders: received Nothing as a response on MsgGetHeaders")
                throwIO $ DialogUnexpected $
                    sformat ("requestHeaders: received Nothing from "%build) nodeId
            Just (MsgNoHeaders t) -> do
                traceWith logTrace (Warning, "requestHeaders: received MsgNoHeaders: " <> t)
                throwIO $ DialogUnexpected $
=======
        logDebug $ sformat ("requestHeaders: inRecovery = "%shown) inRecovery
        case mHeaders of
            Nothing -> do
                logWarning "requestHeaders: received Nothing as a response on MsgGetHeaders"
                throwM $ DialogUnexpected $
                    sformat ("requestHeaders: received Nothing from "%build) nodeId
            Just (MsgNoHeaders t) -> do
                logWarning $ "requestHeaders: received MsgNoHeaders: " <> t
                throwM $ DialogUnexpected $
>>>>>>> CHW-82-84, orphan branch
                    sformat ("requestHeaders: received MsgNoHeaders from "%
                             build%", msg: "%stext)
                            nodeId
                            t
            Just (MsgHeaders headers) -> do
<<<<<<< HEAD
                traceWith logTrace (Debug, sformat
                    ("requestHeaders: received "%int%" headers from nodeId "%build)
                    (headers ^. _NewestFirst . to NE.length)
                    nodeId)
                return headers

    requestBlocks :: BlockVersionData -> OldestFirst [] HeaderHash -> IO (NewestFirst [] Block)
    requestBlocks _   (OldestFirst [])     = pure (NewestFirst [])
    requestBlocks bvd (OldestFirst (b:bs)) = enqueueMsgSingle
        enqueue
        (MsgRequestBlocks (S.singleton nodeId))
        (Conversation $ requestBlocksConversation bvd (OldestFirst (b :| bs)))

    requestBlocksConversation
        :: BlockVersionData
        -> OldestFirst NE HeaderHash
        -> ConversationActions MsgGetBlocks MsgBlock
        -> IO (NewestFirst [] Block)
=======
                logDebug $ sformat
                    ("requestHeaders: received "%int%" headers from nodeId "%build)
                    (headers ^. _NewestFirst . to NE.length)
                    nodeId
                return headers

    requestBlocks :: BlockVersionData -> OldestFirst NE BlockHeader -> d (NewestFirst NE Block)
    requestBlocks bvd headers = enqueueMsgSingle
        enqueue
        (MsgRequestBlocks (S.singleton nodeId))
        (Conversation $ requestBlocksConversation bvd headers)

    requestBlocksConversation
        :: BlockVersionData
        -> OldestFirst NE BlockHeader
        -> ConversationActions MsgGetBlocks MsgBlock d
        -> d (NewestFirst NE Block)
>>>>>>> CHW-82-84, orphan branch
    requestBlocksConversation bvd headers conv = do
        -- Preserved behaviour from existing logic code: all of the headers
        -- except for the first and last are tossed away.
        -- TODO don't be so wasteful [CSL-2148]
        let oldestHeader = headers ^. _OldestFirst . _neHead
            newestHeader = headers ^. _OldestFirst . _neLast
            numBlocks = length headers
            lcaChild = oldestHeader
<<<<<<< HEAD
        traceWith logTrace (Debug, sformat ("Requesting blocks from "%shortHashF%" to "%shortHashF)
                           lcaChild
                           newestHeader)
        send conv $ mkBlocksRequest lcaChild newestHeader
        traceWith logTrace (Debug, "Requested blocks, waiting for the response")
=======
            newestHash = headerHash newestHeader
            lcaChildHash = headerHash lcaChild
        logDebug $ sformat ("Requesting blocks from "%shortHashF%" to "%shortHashF)
                           lcaChildHash
                           newestHash
        send conv $ mkBlocksRequest lcaChildHash newestHash
        logDebug "Requested blocks, waiting for the response"
>>>>>>> CHW-82-84, orphan branch
        chainE <- runExceptT (retrieveBlocks conv bvd numBlocks)
        case chainE of
            Left e -> do
                let msg = sformat ("Error retrieving blocks from "%shortHashF%
                                   " to "%shortHashF%" from peer "%
                                   build%": "%stext)
<<<<<<< HEAD
                                  lcaChild newestHeader nodeId e
                traceWith logTrace (Warning, msg)
                throwIO $ DialogUnexpected msg
            Right bs -> return bs
=======
                                  lcaChildHash newestHash nodeId e
                logWarning msg
                throwM $ DialogUnexpected msg
            Right bs -> case nonEmptyNewestFirst bs of
                Nothing -> do
                    let msg = sformat ("Peer gave an empty blocks list")
                    throwM $ DialogUnexpected msg
                Just blocks -> return blocks
>>>>>>> CHW-82-84, orphan branch

    -- A piece of the block retrieval conversation in which the blocks are
    -- pulled in one-by-one.
    retrieveBlocks
<<<<<<< HEAD
        :: ConversationActions MsgGetBlocks MsgBlock
        -> BlockVersionData
        -> Int
        -> ExceptT Text IO (NewestFirst [] Block)
=======
        :: ConversationActions MsgGetBlocks MsgBlock d
        -> BlockVersionData
        -> Int
        -> ExceptT Text d (NewestFirst [] Block)
>>>>>>> CHW-82-84, orphan branch
    retrieveBlocks conv bvd numBlocks = retrieveBlocksDo conv bvd numBlocks []

    -- Content of retrieveBlocks.
    -- Receive a given number of blocks. If the server doesn't send this
    -- many blocks, an error will be given.
    --
    -- Copied from the old logic but modified to use an accumulator rather
    -- than fmapping (<|). That changed the order so we're now NewestFirst
    -- (presumably the server sends them oldest first, as that assumption was
    -- required for the old version to correctly say OldestFirst).
    retrieveBlocksDo
<<<<<<< HEAD
        :: ConversationActions MsgGetBlocks MsgBlock
        -> BlockVersionData
        -> Int        -- ^ Index of block we're requesting
        -> [Block]    -- ^ Accumulator
        -> ExceptT Text IO (NewestFirst [] Block)
=======
        :: ConversationActions MsgGetBlocks MsgBlock d
        -> BlockVersionData
        -> Int        -- ^ Index of block we're requesting
        -> [Block]    -- ^ Accumulator
        -> ExceptT Text d (NewestFirst [] Block)
>>>>>>> CHW-82-84, orphan branch
    retrieveBlocksDo conv bvd !i !acc
        | i <= 0    = pure $ NewestFirst acc
        | otherwise = lift (recvLimited conv (mlMsgBlock bvd)) >>= \case
              Nothing ->
                  throwError $ sformat ("Block retrieval cut short by peer at index #"%int) i
              Just (MsgNoBlock t) ->
                  throwError $ sformat ("Peer failed to produce block #"%int%": "%stext) i t
              Just (MsgBlock block) -> do
                  retrieveBlocksDo conv bvd (i - 1) (block : acc)

requestTip
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> EnqueueMsg
    -> Word
    -> IO (Map NodeId (IO BlockHeader))
requestTip logTrace logic enqueue recoveryHeadersMessage = fmap waitForDequeues $
    enqueue (MsgRequestBlockHeaders Nothing) $ \nodeId _ -> pure . Conversation $
        \(conv :: ConversationActions MsgGetHeaders MsgHeaders) -> do
            traceWith logTrace (Debug, "Requesting tip...")
            bvd <- getAdoptedBVData logic
            send conv (MsgGetHeaders [] Nothing)
            received <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
            case received of
                Just headers -> handleTip nodeId headers
                Nothing      -> throwIO $ DialogUnexpected "peer didnt' respond with tips"
  where
    handleTip nodeId (MsgHeaders (NewestFirst (tip:|[]))) = do
        traceWith logTrace (Debug, sformat ("Got tip "%shortHashF%" from "%shown%", processing") (headerHash tip) nodeId)
        pure tip
    handleTip _ t = do
        traceWith logTrace (Warning, sformat ("requestTip: got enexpected response: "%shown) t)
        throwIO $ DialogUnexpected "peer sent more than one tip"

-- | Announce a block header.
announceBlockHeader
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> ProtocolConstants
    -> Word
    -> EnqueueMsg
    -> MainBlockHeader
    -> IO (Map NodeId (IO ()))
announceBlockHeader logTrace logic protocolConstants recoveryHeadersMessage enqueue header =  do
    traceWith logTrace (Debug, sformat ("Announcing header to others:\n"%build) header)
    waitForDequeues <$> enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
=======
    :: forall d t .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> EnqueueMsg d
    -> (BlockHeader -> NodeId -> d t)
    -> d (Map NodeId (d t))
requestTip logic enqueue k =
    enqueue (MsgRequestBlockHeaders Nothing) $ \nodeId _ -> pure . Conversation $
        \(conv :: ConversationActions MsgGetHeaders MsgHeaders m) -> do
            logDebug "Requesting tip..."
            bvd <- getAdoptedBVData logic
            send conv (MsgGetHeaders [] Nothing)
            received <- recvLimited conv (mlMsgHeaders bvd 2200)
            case received of
                Just headers -> handleTip nodeId headers
                Nothing      -> throwM $ DialogUnexpected "peer didnt' respond with tips"
  where
    handleTip nodeId (MsgHeaders (NewestFirst (tip:|[]))) = do
        logDebug $ sformat ("Got tip "%shortHashF%", processing") (headerHash tip)
        k tip nodeId
    handleTip _ t = do
        logWarning $ sformat ("requestTip: got enexpected response: "%shown) t
        throwM $ DialogUnexpected "peer sent more than one tip"

-- | Announce a block header.
announceBlockHeader
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> ProtocolConstants
    -> Word
    -> EnqueueMsg d
    -> MainBlockHeader
    -> d (Map NodeId (d ()))
announceBlockHeader logic protocolConstants recoveryHeadersMessage enqueue header =  do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
>>>>>>> CHW-82-84, orphan branch
  where
    announceBlockDo nodeId = pure $ Conversation $ \cA -> do
        -- TODO figure out what this security stuff is doing and judge whether
        -- it needs to change / be removed.
        let sparams = securityParams logic
        -- Copied from Pos.Security.Util but made pure. The existing
        -- implementation was tied to a reader rather than taking a
        -- SecurityParams value as a function argument.
            shouldIgnoreAddress :: NetworkAddress -> Bool
            shouldIgnoreAddress addr = and
                [ AttackNoBlocks `elem` spAttackTypes sparams
                , NetworkAddressTarget addr `elem` spAttackTargets sparams
                ]
<<<<<<< HEAD
            throwOnIgnored :: NodeId -> IO ()
            throwOnIgnored nId =
                whenJust (nodeIdToAddress nId) $ \addr ->
                    when (shouldIgnoreAddress addr) $
                        throwIO AttackNoBlocksTriggered
        -- TODO the when condition is not necessary, as it's a part of the
        -- conjunction in shouldIgnoreAddress
        when (AttackNoBlocks `elem` spAttackTypes sparams) (throwOnIgnored nodeId)
        traceWith logTrace (Debug,
            sformat
                ("Announcing block"%shortHashF%" to "%build)
                (headerHash header)
                nodeId)
        send cA $ MsgHeaders (one (BlockHeaderMain header))
        -- After we announce, the peer is given an opportunity to request more
        -- headers within the same conversation.
        handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage cA
=======
            throwOnIgnored :: NodeId -> d ()
            throwOnIgnored nId =
                whenJust (nodeIdToAddress nId) $ \addr ->
                    when (shouldIgnoreAddress addr) $
                        throwM AttackNoBlocksTriggered
        -- TODO the when condition is not necessary, as it's a part of the
        -- conjunction in shouldIgnoreAddress
        when (AttackNoBlocks `elem` spAttackTypes sparams) (throwOnIgnored nodeId)
        logDebug $
            sformat
                ("Announcing block"%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send cA $ MsgHeaders (one (BlockHeaderMain header))
        -- After we announce, the peer is given an opportunity to request more
        -- headers within the same conversation.
        handleHeadersCommunication logic protocolConstants recoveryHeadersMessage cA
>>>>>>> CHW-82-84, orphan branch

-- | A conversation for incoming MsgGetHeaders messages.
-- For each of these messages, we'll try to send back the relevant headers,
-- until the client closes up.
handleHeadersCommunication
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> ProtocolConstants
    -> Word
    -> ConversationActions MsgHeaders MsgGetHeaders
    -> IO ()
handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage conv = do
    let bc = fromIntegral (pcK protocolConstants)
    whenJustM (recvLimited conv (mlMsgGetHeaders bc)) $ \mgh@(MsgGetHeaders {..}) -> do
        traceWith logTrace (Debug, sformat ("Got request on handleGetHeaders: "%build) mgh)
=======
    :: forall d .
       ( DiffusionWorkMode d
       )
    => Logic d
    -> ProtocolConstants
    -> Word
    -> ConversationActions MsgHeaders MsgGetHeaders d
    -> d ()
handleHeadersCommunication logic protocolConstants recoveryHeadersMessage conv = do
    let bc = fromIntegral (pcK protocolConstants)
    whenJustM (recvLimited conv (mlMsgGetHeaders bc)) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug $ sformat ("Got request on handleGetHeaders: "%build) mgh
>>>>>>> CHW-82-84, orphan branch
        -- FIXME
        -- Diffusion layer is entirely capable of serving blocks even if the
        -- logic layer is in recovery mode.
        ifM (recoveryInProgress logic) onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                -- This is how a peer requests our tip: empty checkpoint list,
                -- Nothing for the limiting hash.
                ([], Nothing) -> Right . one <$> getLastMainHeader
                -- This is how a peer requests one particular header: empty
                -- checkpoint list, Just for the limiting hash.
                ([], Just h)  -> do
                    mHeader <- getBlockHeader logic h
                    pure . maybeToRight "getBlockHeader returned Nothing" . fmap one $ mHeader
                -- This is how a peer requests a chain of headers.
                -- NB: if the limiting hash is Nothing, getBlockHeaders will
                -- substitute our current tip.
                (c1:cxs, _)   ->
                    first show <$>
                    getBlockHeaders logic (Just recoveryHeadersMessage) (c1:|cxs) mghTo
            either onNoHeaders handleSuccess headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
<<<<<<< HEAD
    getLastMainHeader :: IO BlockHeader
=======
    getLastMainHeader :: d BlockHeader
>>>>>>> CHW-82-84, orphan branch
    getLastMainHeader = do
        tip :: Block <- getTip logic
        let tipHeader = tip ^. blockHeader
        case tip of
            Left _  -> do
                mHeader <- getBlockHeader logic (tip ^. prevBlockL)
                pure $ fromMaybe tipHeader mHeader
            Right _ -> pure tipHeader
<<<<<<< HEAD
    handleSuccess :: NewestFirst NE BlockHeader -> IO ()
    handleSuccess h = do
        send conv (MsgHeaders h)
        traceWith logTrace (Debug, "handleGetHeaders: responded successfully")
        handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage conv
    onNoHeaders reason = do
        let err = "getheadersFromManyTo returned Nothing, reason: " <> reason
        traceWith logTrace (Warning, err)
        send conv (MsgNoHeaders err)
    onRecovery = do
        traceWith logTrace (Debug, "handleGetHeaders: not responding, we're in recovery mode")
=======
    handleSuccess :: NewestFirst NE BlockHeader -> d ()
    handleSuccess h = do
        send conv (MsgHeaders h)
        logDebug "handleGetHeaders: responded successfully"
        handleHeadersCommunication logic protocolConstants recoveryHeadersMessage conv
    onNoHeaders reason = do
        let err = "getheadersFromManyTo returned Nothing, reason: " <> reason
        logWarning err
        send conv (MsgNoHeaders err)
    onRecovery = do
        logDebug "handleGetHeaders: not responding, we're in recovery mode"
>>>>>>> CHW-82-84, orphan branch
        send conv (MsgNoHeaders "server node is in recovery mode")


-- |
-- = Listeners

-- | All block-related listeners.
blockListeners
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Logic IO
=======
    :: ( DiffusionWorkMode m
       )
    => Logic m
>>>>>>> CHW-82-84, orphan branch
    -> ProtocolConstants
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> Timer -- ^ Keepalive timer
<<<<<<< HEAD
    -> MkListeners
blockListeners logTrace logic protocolConstants recoveryHeadersMessage oq keepaliveTimer = constantListeners $
    [ -- Peer wants some block headers from us.
      handleGetHeaders logTrace logic protocolConstants recoveryHeadersMessage oq
      -- Peer wants some blocks from us.
    , handleGetBlocks logTrace logic recoveryHeadersMessage oq
      -- Peer has a block header for us (yes, singular only).
    , handleBlockHeaders logTrace logic oq recoveryHeadersMessage keepaliveTimer
=======
    -> MkListeners m
blockListeners logic protocolConstants recoveryHeadersMessage oq keepaliveTimer = constantListeners $
    [ -- Peer wants some block headers from us.
      handleGetHeaders logic protocolConstants recoveryHeadersMessage oq
      -- Peer wants some blocks from us.
    , handleGetBlocks logic recoveryHeadersMessage oq
      -- Peer has a block header for us (yes, singular only).
    , handleBlockHeaders logic oq keepaliveTimer
>>>>>>> CHW-82-84, orphan branch
    ]

----------------------------------------------------------------------------
-- Getters (return currently stored data)
----------------------------------------------------------------------------

-- | Handles GetHeaders request which means client wants to get
-- headers from some checkpoints that are older than optional @to@
-- field.
handleGetHeaders
<<<<<<< HEAD
    :: forall pack.
       Trace IO (Severity, Text)
    -> Logic IO
    -> ProtocolConstants
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec, OutSpecs)
handleGetHeaders logTrace logic protocolConstants recoveryHeadersMessage oq = listenerConv logTrace oq $ \__ourVerInfo nodeId conv -> do
    traceWith logTrace (Debug, "handleGetHeaders: request from " <> show nodeId)
    handleHeadersCommunication logTrace logic protocolConstants recoveryHeadersMessage conv
=======
    :: forall pack m.
       ( DiffusionWorkMode m )
    => Logic m
    -> ProtocolConstants
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetHeaders logic protocolConstants recoveryHeadersMessage oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    logDebug $ "handleGetHeaders: request from " <> show nodeId
    handleHeadersCommunication logic protocolConstants recoveryHeadersMessage conv
>>>>>>> CHW-82-84, orphan branch

-- | Handler for a GetBlocks request from a client.
-- It looks up the Block corresponding to each HeaderHash and sends it.
handleGetBlocks
<<<<<<< HEAD
    :: forall pack.
       Trace IO (Severity, Text)
    -> Logic IO
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec, OutSpecs)
handleGetBlocks logTrace logic recoveryHeadersMessage oq = listenerConv logTrace oq $ \__ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv mlMsgGetBlocks
    whenJust mbMsg $ \mgb@MsgGetBlocks{..} -> do
        traceWith logTrace (Debug, sformat ("handleGetBlocks: got request "%build%" from "%build)
            mgb nodeId)
=======
    :: forall pack m.
       ( DiffusionWorkMode m )
    => Logic m
    -> Word
    -> OQ.OutboundQ pack NodeId Bucket
    -> (ListenerSpec m, OutSpecs)
handleGetBlocks logic recoveryHeadersMessage oq = listenerConv oq $ \__ourVerInfo nodeId conv -> do
    mbMsg <- recvLimited conv mlMsgGetBlocks
    whenJust mbMsg $ \mgb@MsgGetBlocks{..} -> do
        logDebug $ sformat ("handleGetBlocks: got request "%build%" from "%build)
            mgb nodeId
>>>>>>> CHW-82-84, orphan branch
        -- [CSL-2148] will probably make this a faster, streaming style:
        -- get the blocks directly from headers rather than getting the list
        -- of headers, then one-by-one getting the corresponding blocks.
        -- As such, the DBMalformed error below (failMalformed) won't be
        -- necessary: the streaming thing (probably a conduit) can determine
        -- whether the DB is malformed. Really, this listener has no business
        -- deciding that the database is malformed.
        hashesM <- getHashesRange logic (Just recoveryHeadersMessage) mgbFrom mgbTo
        case hashesM of
            Right hashes -> do
<<<<<<< HEAD
                traceWith logTrace (Debug, sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one")
                    (length hashes) nodeId )
=======
                logDebug $ sformat
                    ("handleGetBlocks: started sending "%int%
                     " blocks to "%build%" one-by-one: "%listJson)
                    (length hashes) nodeId hashes
>>>>>>> CHW-82-84, orphan branch
                for_ hashes $ \hHash ->
                    getBlock logic hHash >>= \case
                        Just b -> send conv $ MsgBlock b
                        Nothing  -> do
                            send conv $ MsgNoBlock ("Couldn't retrieve block with hash " <>
                                                    pretty hHash)
                            failMalformed
<<<<<<< HEAD
                traceWith logTrace (Debug, "handleGetBlocks: blocks sending done")
            Left e -> traceWith logTrace (Warning, "getBlocksByHeaders@retrieveHeaders returned error: " <> show e)
  where
    -- See note above in the definition of handleGetBlocks [CSL-2148].
    failMalformed =
        throwIO $ DBMalformed $
=======
                logDebug "handleGetBlocks: blocks sending done"
            Left e -> logWarning $ "getBlocksByHeaders@retrieveHeaders returned error: " <> show e
  where
    -- See note above in the definition of handleGetBlocks [CSL-2148].
    failMalformed =
        throwM $ DBMalformed $
>>>>>>> CHW-82-84, orphan branch
        "handleGetBlocks: getHashesRange returned header that doesn't " <>
        "have corresponding block in storage."

----------------------------------------------------------------------------
-- Header propagation
----------------------------------------------------------------------------

-- | Handles MsgHeaders request, unsolicited usecase
handleBlockHeaders
<<<<<<< HEAD
    :: forall pack .
       Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> Word
    -> Timer
    -> (ListenerSpec, OutSpecs)
handleBlockHeaders logTrace logic oq recoveryHeadersMessage keepaliveTimer =
  listenerConv @MsgGetHeaders logTrace oq $ \__ourVerInfo nodeId conv -> do
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
    traceWith logTrace (Debug, "handleBlockHeaders: got some unsolicited block header(s)")
    bvd <- getAdoptedBVData logic
    mHeaders <- recvLimited conv (mlMsgHeaders bvd (fromIntegral recoveryHeadersMessage))
    whenJust mHeaders $ \case
        (MsgHeaders headers) -> do
            -- Reset the keepalive timer.
            slotDuration <- bvdSlotDuration <$> getAdoptedBVData logic
            startTimer (3 * slotDuration) keepaliveTimer
            handleUnsolicitedHeaders logTrace logic (getNewestFirst headers) nodeId
=======
    :: forall pack m.
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> Timer
    -> (ListenerSpec m, OutSpecs)
handleBlockHeaders logic oq keepaliveTimer =
  listenerConv @MsgGetHeaders oq $ \__ourVerInfo nodeId conv -> do
    -- The type of the messages we send is set to 'MsgGetHeaders' for
    -- protocol compatibility reasons only. We could use 'Void' here because
    -- we don't really send any messages.
    logDebug "handleBlockHeaders: got some unsolicited block header(s)"
    bvd <- getAdoptedBVData logic
    mHeaders <- recvLimited conv (mlMsgHeaders bvd 2200)
    whenJust mHeaders $ \case
        (MsgHeaders headers) -> do
            -- Reset the keepalive timer.
            slotDuration <- toMicroseconds . bvdSlotDuration <$> getAdoptedBVData logic
            setTimerDuration keepaliveTimer $ fromMicroseconds (3 * slotDuration)
            startTimer keepaliveTimer
            handleUnsolicitedHeaders logic (getNewestFirst headers) nodeId
>>>>>>> CHW-82-84, orphan branch
        _ -> pass -- Why would somebody propagate 'MsgNoHeaders'? We don't care.

-- Second case of 'handleBlockheaders'
handleUnsolicitedHeaders
<<<<<<< HEAD
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> NonEmpty BlockHeader
    -> NodeId
    -> IO ()
handleUnsolicitedHeaders _ logic (header :| []) nodeId =
    postBlockHeader logic header nodeId
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders logTrace _ (h:|hs) _ = do
    traceWith logTrace (Warning, "Someone sent us nonzero amount of headers we didn't expect")
    traceWith logTrace (Warning, sformat ("Here they are: "%listJson) (h:hs))
=======
    :: ( DiffusionWorkMode m )
    => Logic m
    -> NonEmpty BlockHeader
    -> NodeId
    -> m ()
handleUnsolicitedHeaders logic (header :| []) nodeId =
    postBlockHeader logic header nodeId
-- TODO: ban node for sending more than one unsolicited header.
handleUnsolicitedHeaders _ (h:|hs) _ = do
    logWarning "Someone sent us nonzero amount of headers we didn't expect"
    logWarning $ sformat ("Here they are: "%listJson) (h:hs)
>>>>>>> CHW-82-84, orphan branch
