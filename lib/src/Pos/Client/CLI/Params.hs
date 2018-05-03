-- | Getter params from Args

module Pos.Client.CLI.Params
       ( wlogParams 
       , getKeyfilePath
       , getNodeParams
       , gtSscParams
       ) where

import           Universum

import           Data.Default (def)
import           Data.Functor.Contravariant (contramap)
import qualified Data.Yaml as Yaml
import           System.Wlog (LoggerName)

import           Pos.Behavior (BehaviorConfig (..))
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (CommonArgs (..))
import           Pos.Client.CLI.Secrets (prepareUserSecret)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Crypto (VssKeyPair)
import           Pos.Launcher.Param (WlogParams (..), NodeParams (..))
import           Pos.Network.CLI (intNetworkConfigOpts)
import           Pos.Ssc (SscParams (..))
import           Pos.Update.Params (UpdateParams (..))
import           Pos.Util.UserSecret (peekUserSecret)
import           Pos.Util.Trace (Trace)
import           Pos.Util.Trace.Unstructured (LogItem, publicPrivateLogItem)
import           Pos.Util.Util (eitherToThrow)

wlogParams :: LoggerName -> CommonNodeArgs -> WlogParams
wlogParams defaultName CommonNodeArgs{..} =
    WlogParams
    { wpHandlerPrefix = logPrefix commonArgs
    , wpConfigPath    = logConfig commonArgs
    , wpDefaultName   = defaultName
    , wpConsoleLog    = Nothing -- no override by default
    }

gtSscParams :: CommonNodeArgs -> VssKeyPair -> BehaviorConfig -> SscParams
gtSscParams CommonNodeArgs {..} vssSK BehaviorConfig{..} =
    SscParams
    { spSscEnabled = True
    , spVssKeyPair = vssSK
    , spBehavior   = bcSscBehavior
    }

getKeyfilePath :: CommonNodeArgs -> FilePath
getKeyfilePath CommonNodeArgs {..}
    = case devGenesisSecretI of
          Nothing -> keyfilePath
          Just i  -> "node-" ++ show i ++ "." ++ keyfilePath

getNodeParams ::
       ( HasConfiguration
       )
    => Trace IO LogItem
    -> CommonNodeArgs
    -> NodeArgs
    -> IO NodeParams
getNodeParams logTrace cArgs@CommonNodeArgs{..} NodeArgs{..} = do
    (primarySK, userSecret) <-
        prepareUserSecret logTrace cArgs =<< peekUserSecret logTrace (getKeyfilePath cArgs)
    npNetworkConfig <- intNetworkConfigOpts networkConfigOpts (contramap publicPrivateLogItem logTrace)
    npBehaviorConfig <- case behaviorConfigPath of
        Nothing -> pure def
        Just fp -> eitherToThrow =<< liftIO (Yaml.decodeFileEither fp)
    pure NodeParams
        { npDbPathM = dbPath
        , npRebuildDb = rebuildDB
        , npSecretKey = primarySK
        , npUserSecret = userSecret
        , npJLFile = jlPath
        , npReportServers = reportServers commonArgs
        , npUpdateParams = UpdateParams
            { upUpdatePath    = updateLatestPath
            , upUpdateWithPkg = updateWithPackage
            , upUpdateServers = updateServers commonArgs
            }
        , npRoute53Params = route53Params
        , npEnableMetrics = enableMetrics
        , npEkgParams = ekgParams
        , npStatsdParams = statsdParams
        , ..
        }
