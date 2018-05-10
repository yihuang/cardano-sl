module JSONLog
    ( jsonLogs
    , parseLogP
    , IndexedJLTimedEvent (..)
    , runParseLogs
    ) where

<<<<<<< HEAD
import           Data.Attoparsec.Text (Parser, parseOnly, takeTill)
=======
import           Data.Attoparsec.Text (Parser, decimal, parseOnly, string)
>>>>>>> CHW-82-84, orphan branch
import           Pipes
import           Pipes.ByteString (fromHandle)
import           Pipes.Interleave (interleave)
import qualified Pipes.Prelude as P
import           System.Directory (listDirectory)
import           System.FilePath ((</>))

<<<<<<< HEAD
import           Pos.Util.JsonLog.Events (JLEvent, JLTimedEvent (..))
=======
import           Pos.Util.JsonLog (JLEvent, JLTimedEvent (..))
>>>>>>> CHW-82-84, orphan branch
import           Types
import           Universum
import           Util.Aeson (parseJSONP)
import           Util.Safe (runWithFiles)

<<<<<<< HEAD
jsonLogs :: FilePath -> IO [(Text, FilePath)]
=======
jsonLogs :: FilePath -> IO [(Int, FilePath)]
>>>>>>> CHW-82-84, orphan branch
jsonLogs logDir = do
    files <- listDirectory logDir
    return $ map (second (logDir </>)) $ mapMaybe f files
  where
<<<<<<< HEAD
    f :: FilePath -> Maybe (Text, FilePath)
    f logFile = case parseOnly nodeIndexParser $ toText logFile of
        Right name -> Just (name, logFile)
        Left _  -> Nothing

nodeIndexParser :: Parser Text
nodeIndexParser = takeTill (== '.') <* ".json"
=======
    f :: FilePath -> Maybe (Int, FilePath)
    f logFile = case parseOnly nodeIndexParser $ toText logFile of
        Right n -> Just (n, logFile)
        Left _  -> Nothing

nodeIndexParser :: Parser Int
nodeIndexParser = string "node" *> decimal <* string ".json"
>>>>>>> CHW-82-84, orphan branch

parseLogP :: MonadIO m => Handle -> Producer JLTimedEvent m ()
parseLogP h = fromHandle h >-> parseJSONP

data IndexedJLTimedEvent = IndexedJLTimedEvent
<<<<<<< HEAD
    { ijlNode      :: !NodeId
=======
    { ijlNode      :: !NodeIndex
>>>>>>> CHW-82-84, orphan branch
    , ijlTimestamp :: !Timestamp
    , ijlEvent     :: !JLEvent
    }

instance Eq IndexedJLTimedEvent where

    (==) = (==) `on` ijlTimestamp

instance Ord IndexedJLTimedEvent where

    compare = compare `on` ijlTimestamp

runParseLogs :: FilePath -> (Producer IndexedJLTimedEvent IO () -> IO r) -> IO r
runParseLogs logDir f = do
    xs <- jsonLogs logDir
    runWithFiles xs ReadMode $ \ys -> f $ interleave (map (uncurry producer) ys)
  where
<<<<<<< HEAD
    producer :: NodeId -> Handle -> Producer IndexedJLTimedEvent IO ()
=======
    producer :: Int -> Handle -> Producer IndexedJLTimedEvent IO ()
>>>>>>> CHW-82-84, orphan branch
    producer n h = parseLogP h >-> P.map (\JLTimedEvent{..} ->
        IndexedJLTimedEvent { ijlNode      = n
                            , ijlTimestamp = fromIntegral jlTimestamp
                            , ijlEvent     = jlEvent
                            })
