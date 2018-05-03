{-# LANGUAGE RankNTypes #-}

module Pos.Util.TimeLimit
       ( WaitingDelta (..)
       , logLongAction
       , logWaitLinear
       , logWaitInf
       ) where

import           Universum

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (withAsyncWithUnmask)
import           Data.Time.Units (Microsecond, Second, convertUnit, toMicroseconds)
import           Formatting (sformat, shown, stext, (%))

import           Pos.Util.Trace (Trace, traceWith)

-- | Data type to represent waiting strategy for printing warnings
-- if action take too much time.
--
-- [LW-4]: this probably will be moved somewhere from here
data WaitingDelta
    = WaitOnce      Second              -- ^ wait s seconds and stop execution
    | WaitLinear    Second              -- ^ wait s, s * 2, s * 3  , s * 4  , ...      seconds
    | WaitGeometric Microsecond Double  -- ^ wait m, m * q, m * q^2, m * q^3, ... microseconds
    deriving (Show)

-- | Run action and print warning if it takes more time than expected.
logLongAction
    :: forall a. Trace IO Text -> WaitingDelta -> Text -> IO a -> IO a
logLongAction logTrace delta actionTag action =
    -- Previous implementation was
    --
    --   bracket (fork $ waitAndWarn delta) killThread (const action)
    --
    -- but this has a subtle problem: 'killThread' can be interrupted even
    -- when exceptions are masked, so it's possible that the forked thread is
    -- left running, polluting the logs with misinformation.
    --
    -- 'withAsync' is assumed to take care of this, and indeed it does for
    -- 'Production's implementation, which uses the definition from the async
    -- package: 'uninterruptibleCancel' is used to kill the thread.
    --
    -- thinking even more about it, unmasking auxilary thread is crucial if
    -- this function is going to be called under 'mask'.
    withAsyncWithUnmask (\unmask -> unmask $ waitAndWarn delta) (const action)
  where
    printWarning t = traceWith logTrace (sformat ("Action `"%stext%"` took more than "%shown) actionTag t)

    -- [LW-4]: avoid code duplication somehow (during refactoring)
    waitAndWarn (WaitOnce      s  ) = do
        threadDelay (fromIntegral (toMicroseconds s))
        printWarning s
    waitAndWarn (WaitLinear    s  ) =
        let waitLoop acc = do
                threadDelay (fromIntegral (toMicroseconds s))
                printWarning acc
                waitLoop (acc + s)
        in waitLoop s
    waitAndWarn (WaitGeometric s q) =
        let waitLoop acc t = do
                threadDelay (fromIntegral (toMicroseconds t))
                let newAcc = acc + t
                let newT   = round $ fromIntegral t * q
                printWarning (convertUnit newAcc :: Second)
                waitLoop newAcc newT
        in waitLoop 0 s

{- Helper functions to avoid dealing with data type -}

-- | Specialization of 'logLongAction' with 'WaiLinear'.
logWaitLinear :: Trace IO Text -> Second -> Text -> IO a -> IO a
logWaitLinear logTrace = logLongAction logTrace . WaitLinear

-- | Specialization of 'logWarningLongAction' with 'WaitGeometric'
-- with parameter @1.3@. Accepts 'Second'.
logWaitInf :: Trace IO Text -> Second -> Text -> IO a -> IO a
logWaitInf logTrace = logLongAction logTrace . (`WaitGeometric` 1.3) . convertUnit
