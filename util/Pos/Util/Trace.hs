{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pos.Util.Trace
    ( Trace (..)
    , trace
    , traceWith
    , noTrace
    -- TODO put wlog tracing into its own module.
    , wlogTrace
    , klogTrace
    , Severity (..)
    ) where

import           Universum hiding (trace)
import           Data.Functor.Contravariant (Contravariant (..), Op (..))
import qualified System.Wlog as Wlog
import qualified Katip as K

-- | Abstracts logging.
newtype Trace m s = Trace
    { runTrace :: Op (m ()) s
    }

-- | abstract libraries' severity
data Severity = Debug | Info | Warning | Notice | Error

-- | translate Severity to System.Wlog.Severity
sev2wlog :: Severity -> Wlog.Severity
sev2wlog = \case
    Debug   -> Wlog.Debug
    Info    -> Wlog.Info
    Notice  -> Wlog.Notice
    Warning -> Wlog.Warning
    Error   -> Wlog.Error

-- | translate Severity to Katip.Severity
sev2klog :: Severity -> K.Severity
sev2klog = \case
    Debug   -> K.DebugS
    Info    -> K.InfoS
    Notice  -> K.NoticeS
    Warning -> K.WarningS
    Error   -> K.ErrorS

instance Contravariant (Trace m) where
    contramap f = Trace . contramap f . runTrace

trace :: Trace m s -> s -> m ()
trace = getOp . runTrace

-- | Alias to 'trace' so that you don't clash with 'Debug.trace' in case it's
-- imported (Universum exports it).
traceWith :: Trace m s -> s -> m ()
traceWith = trace

-- | A 'Trace' that ignores everything. NB this actually turns off logging: it
-- doesn't force the logged messages.
noTrace :: Applicative m => Trace m a
noTrace = Trace $ Op $ const (pure ())

-- | translate
s2wname :: Text -> Wlog.LoggerName
s2wname s = Wlog.LoggerName s

-- | A 'Trace' that uses log-warper.
wlogTrace :: Text -> Trace IO (Severity, Text)
wlogTrace loggerName = Trace $ Op $ \(severity, txt) ->
    Wlog.usingLoggerName (s2wname loggerName) $ Wlog.logMessage (sev2wlog severity) txt

-- | translate
s2kname :: Text -> K.Namespace
s2kname s = K.Namespace [s]

-- | A 'Trace' that uses katip.
klogTrace :: Text -> IO (Trace IO (Severity, Text))
klogTrace loggerName = do
    hScribe <- K.mkHandleScribe K.ColorIfTerminal stdout K.InfoS K.V0
    le <- K.registerScribe "stdout" hScribe K.defaultScribeSettings =<< K.initLogEnv (s2kname loggerName) "production"
    let tracer = Trace $ Op $ \(severity, txt) -> do
                    K.runKatipContextT le () "trace" $
                      K.logItemM Nothing (sev2klog severity) $ K.logStr txt
    pure tracer

