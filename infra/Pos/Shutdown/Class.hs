{-# LANGUAGE TypeFamilies #-}

module Pos.Shutdown.Class
       ( HasShutdownContext(..)
       ) where

import           Universum

import           Pos.Shutdown.Types (ShutdownContext)
<<<<<<< HEAD
import           Control.Lens.Lens (lens)

class HasShutdownContext ctx where
    shutdownContext :: Lens' ctx ShutdownContext

instance HasShutdownContext ShutdownContext where
    shutdownContext = lens identity (\_ x -> x)
=======

class HasShutdownContext ctx where
    shutdownContext :: Lens' ctx ShutdownContext
>>>>>>> CHW-82-84, orphan branch
