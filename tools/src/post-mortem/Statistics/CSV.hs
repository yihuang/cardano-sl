module Statistics.CSV
    ( txCntInChainMemPoolToCSV
    , focusToCSV
    ) where

import           Control.Monad.Random (MonadRandom (..), evalRandT)
import qualified Data.Text as T
import           System.IO (hPutStrLn)
import           System.Random (mkStdGen)

<<<<<<< HEAD
import           Pos.Util.JsonLog.Events (JLMemPool (..), MemPoolModifyReason (..))
=======
import           Pos.Txp.MemState.Types (MemPoolModifyReason (..))
import           Pos.Util.JsonLog (JLMemPool (..))
>>>>>>> CHW-82-84, orphan branch
import           Statistics.Focus (Focus (..))
import           Types
import           Universum

txCntInChainMemPoolToCSV :: FilePath
                         -> Double
<<<<<<< HEAD
                         -> [(NodeId, Timestamp, Int)]
                         -> [(NodeId, Timestamp, JLMemPool)]
=======
                         -> [(NodeIndex, Timestamp, Int)]
                         -> [(NodeIndex, Timestamp, JLMemPool)]
>>>>>>> CHW-82-84, orphan branch
                         -> IO ()
txCntInChainMemPoolToCSV f sp txCnt mp =
    flip evalRandT (mkStdGen 918273) $ liftIO $ withFile f WriteMode $ \h -> do
        hPutStrLn h "time,txCount,txType,node"
        for_ txCnt $ \(n, ts, cnt) -> csvLine h "written" n ts (fromIntegral cnt)
        for_ mp $ \(n, ts, p@JLMemPool{..}) ->
            whenM (inSample jlmReason) $ do
                csvLine h (toTxType "Wait" p) n ts jlmWait
                csvLine h (toTxType "Modify" p) n ts jlmModify
                csvLine h (toTxType "SizeAfter" p) n ts (fromIntegral jlmSizeAfter)
  where
<<<<<<< HEAD
    csvLine :: MonadIO m => Handle -> String -> NodeId -> Timestamp -> Integer -> m ()
    csvLine h txType node time txCount = liftIO $ hPutStrLn h $ show (fromIntegral time :: Integer)
        ++ "," ++ show txCount ++ "," ++ txType ++ "," ++ toString node
=======
    csvLine :: MonadIO m => Handle -> String -> NodeIndex -> Timestamp -> Integer -> m ()
    csvLine h txType node time txCount = liftIO $ hPutStrLn h $ show (fromIntegral time :: Integer) ++ "," ++ show txCount ++ "," ++ txType ++ "," ++ show node
>>>>>>> CHW-82-84, orphan branch

    draw :: MonadRandom m => m Bool
    draw = (<= sp) <$> getRandomR (0, 1)

    inSample :: MonadRandom m => MemPoolModifyReason -> m Bool
<<<<<<< HEAD
    inSample ProcessTransaction = draw
=======
    inSample (ProcessTransaction _) = draw
>>>>>>> CHW-82-84, orphan branch
    inSample _                      = return True

    toTxType :: String -> JLMemPool -> String
    toTxType s JLMemPool{..} =
        let reason = case jlmReason of
<<<<<<< HEAD
                ApplyBlock             -> "ApplyBlock"
                ApplyBlockWithRollback -> "ApplyBlockWithRollback"
                ProcessTransaction     -> "ProcessTransaction"
        in  "mp_" ++ reason ++ "_" ++ s

focusToCSV :: FilePath -> [(Timestamp, NodeId, Focus)] -> IO ()
=======
                ApplyBlock           -> "ApplyBlock"
                CreateBlock          -> "CreateBlock"
                ProcessTransaction _ -> "ProcessTransaction"
                Custom t             -> toString t
                Unknown              -> "Unknown"
        in  "mp_" ++ reason ++ "_" ++ s

focusToCSV :: FilePath -> [(Timestamp, NodeIndex, Focus)] -> IO ()
>>>>>>> CHW-82-84, orphan branch
focusToCSV f xs = withFile f WriteMode $ \h -> do
    hPutStrLn h "time,delta_first_seconds,delta_step_seconds,node,type,block/error"
    case xs of
        []                -> return ()
        ((ts0, _, _) : _) -> foldM_ (step h ts0) ts0 xs
  where
<<<<<<< HEAD
    step :: Handle -> Timestamp -> Timestamp -> (Timestamp, NodeId, Focus) -> IO Timestamp
=======
    step :: Handle -> Timestamp -> Timestamp -> (Timestamp, NodeIndex, Focus) -> IO Timestamp
>>>>>>> CHW-82-84, orphan branch
    step h ts0 ts (ts', n, y) = do
        let dt0 = fromIntegral (ts' - ts0) / 1000000 :: Double
            dt  = fromIntegral (ts' - ts ) / 1000000 :: Double
        case y of
            Received me         -> csvLine h ts' dt0 dt n "received" $ maybe "" show me
            InCreatedBlock hash -> csvLine h ts' dt0 dt n "created" $ T.take 6 hash
            InAdoptedBlock hash -> csvLine h ts' dt0 dt n "adopted" $ T.take 6 hash
        return ts'

<<<<<<< HEAD
    csvLine :: Handle -> Timestamp -> Double -> Double -> NodeId -> String -> BlockHash -> IO ()
    csvLine h ts dt0 dt node t he =
        hPutStrLn h $ show ts ++ "," ++ show dt0 ++ "," ++ show dt ++ "," ++ show node ++ "," ++ t
            ++ "," ++ toString he
=======
    csvLine :: Handle -> Timestamp -> Double -> Double -> NodeIndex -> String -> BlockHash -> IO ()
    csvLine h ts dt0 dt node t he =
        hPutStrLn h $ show ts ++ "," ++ show dt0 ++ "," ++ show dt ++ "," ++ show node ++ "," ++ t ++ "," ++ toString he
>>>>>>> CHW-82-84, orphan branch
