-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.Client.CLI.Util
       ( printFlags
       , printInfoOnStart
       , attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
       , readLoggerConfig
       , stakeholderIdParser
       , dumpGenesisData
       , dumpConfiguration
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Yaml as Yaml
import           Formatting (sformat, shown, (%))
import           System.Wlog (LoggerConfig (..), parseLoggerConfig, productionB)
import           Text.Parsec (parserFail, try)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Text as P

import           Pos.Binary.Core ()
import           Pos.Block.Configuration (blockConfiguration)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..))
import           Pos.Client.CLI.Options (configurationOptions)
import           Pos.Configuration (nodeConfiguration)
import           Pos.Core (StakeholderId, Timestamp (..))
import           Pos.Core.Configuration (HasConfiguration, canonicalGenesisJson, coreConfiguration,
                                         genesisData, prettyGenesisJson)
import           Pos.Core.Genesis (gdStartTime)
import           Pos.Crypto (decodeAbstractHash)
import           Pos.Delegation.Configuration (dlgConfiguration)
import           Pos.Ntp.Configuration (NtpConfiguration)
import           Pos.Launcher.Configuration (Configuration (..), HasConfigurations)
import           Pos.Security.Params (AttackTarget (..), AttackType (..))
import           Pos.Ssc.Configuration (sscConfiguration)
import           Pos.Txp.Configuration (txpConfiguration)
import           Pos.Update.Configuration (updateConfiguration)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.TimeWarp (addrParser)
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem, logInfo)

printFlags :: Trace IO LogItem -> IO ()
printFlags logTrace =
    inAssertMode $ logInfo logTrace "Asserts are ON"

printInfoOnStart ::
       (HasConfigurations)
    => Trace IO LogItem
    -> CommonNodeArgs
    -> NtpConfiguration
    -> IO ()
printInfoOnStart logTrace CommonNodeArgs {..} ntpConfig = do
    whenJust cnaDumpGenesisDataPath $ dumpGenesisData logTrace True
    when cnaDumpConfiguration $ dumpConfiguration ntpConfig
    printFlags logTrace
    t <- round . (* 1000000) <$> getPOSIXTime
    mapM_ (logInfo logTrace) $
        [ sformat ("System start time is " % shown) $ gdStartTime genesisData
        , sformat ("Current time is "%shown) (Timestamp t)
        , sformat ("Using configs and genesis:\n"%shown)
                  (configurationOptions commonArgs)
        ]

attackTypeParser :: P.Parser AttackType
attackTypeParser = P.string "No" >>
    AttackNoBlocks <$ (P.string "Blocks") <|>
    AttackNoCommitments <$ (P.string "Commitments")

stakeholderIdParser :: P.Parser StakeholderId
stakeholderIdParser = do
    token <- some P.alphaNum
    either (parserFail . toString) return $
        decodeAbstractHash (toText token)

attackTargetParser :: P.Parser AttackTarget
attackTargetParser =
    (PubKeyAddressTarget <$> try stakeholderIdParser) <|>
    (NetworkAddressTarget <$> addrParser)

-- | Default logger config. Will be used if `--log-config` argument is
-- not passed.
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = productionB

-- | Reads logger config from given path. By default returns
-- 'defaultLoggerConfig'.
readLoggerConfig :: Maybe FilePath -> IO LoggerConfig
readLoggerConfig = maybe (return defaultLoggerConfig) parseLoggerConfig

-- | Dump our 'GenesisData' into a file.
dumpGenesisData ::
       (HasConfiguration) => Trace IO LogItem -> Bool -> FilePath -> IO ()
dumpGenesisData logTrace canonical path = do
    let (canonicalJsonBytes, jsonHash) = canonicalGenesisJson genesisData
    let prettyJsonStr = prettyGenesisJson genesisData
    logInfo logTrace $ sformat ("Writing JSON with hash "%shown%" to "%shown) jsonHash path
    liftIO $ case canonical of
        True  -> BSL.writeFile path canonicalJsonBytes
        False -> writeFile path (toText prettyJsonStr)

-- | Dump our configuration into stdout and exit.
dumpConfiguration
    :: (HasConfigurations)
    => NtpConfiguration
    -> IO ()
dumpConfiguration ntpConfig = do
    let conf =
            Configuration
            { ccCore = coreConfiguration
            , ccNtp = ntpConfig
            , ccUpdate = updateConfiguration
            , ccSsc = sscConfiguration
            , ccDlg = dlgConfiguration
            , ccTxp = txpConfiguration
            , ccBlock = blockConfiguration
            , ccNode = nodeConfiguration
            }
    putText . decodeUtf8 . Yaml.encode $ conf
    exitSuccess
