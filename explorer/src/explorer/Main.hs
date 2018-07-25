{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)

import           ExplorerNodeOptions (ExplorerArgs (..), ExplorerNodeArgs (..),
                     getExplorerNodeOptions)
import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..),
                     getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Core (epochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Explorer.DB (explorerInitDB)
import           Pos.Explorer.ExtraContext (makeExtraCtx)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Txp (ExplorerExtraModifier,
                     explorerTxpGlobalSettings)
import           Pos.Explorer.Web (ExplorerProd, explorerPlugin, notifierPlugin,
                     runExplorerProd)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeParams (..), NodeResources (..), bracketNodeResources,
                     runNode, runRealMode, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..))
import           Pos.Launcher.Resource (getRealLoggerConfig)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import qualified Pos.Util.Log as Log
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed, appendName, namedTrace)
import           Pos.Util.UserSecret (usVss)
import           Pos.Worker.Update (updateTriggerWorker)

loggerName :: Log.LoggerName
loggerName = "explorer"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getExplorerNodeOptions
    let loggingParams = CLI.loggingParams loggerName (enaCommonNodeArgs args)
    lh <- Log.setupLogging =<< getRealLoggerConfig loggingParams
    let logTrace = appendName loggerName $ namedTrace lh
    Log.loggerBracket lh loggerName . logException loggerName $ do
        Log.logInfo "[Attention] Software is built with explorer part"
        action logTrace args

action
    :: ( MonadIO m
       , MonadThrow m
       , Log.WithLogger m
       )
    => TraceNamed m
    -> ExplorerNodeArgs
    -> m ()
action logTrace (ExplorerNodeArgs (cArgs@CommonNodeArgs{..}) ExplorerArgs{..}) =
    withConfigurations logTrace blPath conf $ \ntpConfig pm ->
    withCompileInfo $ do
        CLI.printInfoOnStart logTrace cArgs ntpConfig
        Log.logInfo $ "Explorer is enabled!"
        currentParams <- getNodeParams loggerName cArgs nodeArgs

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        let plugins :: [Diffusion ExplorerProd -> ExplorerProd ()]
            plugins =
                [ explorerPlugin logTrace webPort
                , notifierPlugin logTrace NotifierSettings{ nsPort = notifierPort }
                , updateTriggerWorker logTrace
                ]
        bracketNodeResources logTrace currentParams sscParams
            (explorerTxpGlobalSettings logTrace pm)
            (explorerInitDB pm epochSlots) $ \nr@NodeResources {..} ->
                runExplorerRealMode logTrace pm nr (runNode logTrace pm nr plugins)
  where

    blPath :: Maybe AssetLockPath
    blPath = AssetLockPath <$> cnaAssetLockPath

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runExplorerRealMode
        :: (HasConfigurations,HasCompileInfo)
        => TraceNamed IO
        -> ProtocolMagic
        -> NodeResources ExplorerExtraModifier
        -> (Diffusion ExplorerProd -> ExplorerProd ())
        -> IO ()
    runExplorerRealMode logTrace' pm nr@NodeResources{..} go =
        let NodeContext {..} = nrContext
            extraCtx = makeExtraCtx
            explorerModeToRealMode  = runExplorerProd extraCtx
         in runRealMode logTrace' pm nr $ \diffusion ->
                explorerModeToRealMode (go (hoistDiffusion (lift . lift) explorerModeToRealMode diffusion))

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }
