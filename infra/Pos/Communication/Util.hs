-- | Communication-specific utility functions.

{-# LANGUAGE RankNTypes #-}

module Pos.Communication.Util
       ( Action
       , ActionSpec (..)
       , mapActionSpec
       , toAction
       , localSpecs
<<<<<<< HEAD
=======

       , wrapListener
       , wrapActionSpec
>>>>>>> CHW-82-84, orphan branch
       ) where

import           Universum

<<<<<<< HEAD
import           Pos.Communication.Protocol (OutSpecs)
=======
import           System.Wlog (LoggerName, modifyLoggerName, HasLoggerName)

import           Pos.Communication.Protocol (Listener, mapListener, OutSpecs)
>>>>>>> CHW-82-84, orphan branch
import           Pos.Diffusion.Types (Diffusion)

type Action m a = Diffusion m -> m a

newtype ActionSpec m a = ActionSpec (Action m a)

mapActionSpec
    :: (Diffusion m -> Diffusion m)
    -> (forall t. m t -> m t) -> ActionSpec m a -> ActionSpec m a
mapActionSpec saMapper aMapper (ActionSpec f) =
    ActionSpec $ \sA -> aMapper $ f (saMapper sA)

toAction
    :: (Diffusion m -> m a) -> ActionSpec m a
toAction h = ActionSpec $ h

localSpecs :: m a -> (ActionSpec m a, OutSpecs)
localSpecs h = (ActionSpec $ \__sA -> h, mempty)
<<<<<<< HEAD
=======

wrapListener
  :: ( HasLoggerName m )
  => LoggerName -> Listener m -> Listener m
wrapListener lname = modifyLogger lname
  where
    modifyLogger _name = mapListener $ modifyLoggerName (<> lname)

wrapActionSpec
  :: ( HasLoggerName m )
  => LoggerName -> ActionSpec m a -> ActionSpec m a
wrapActionSpec lname = modifyLogger lname
  where
    modifyLogger _name = mapActionSpec identity $ modifyLoggerName
                                    (<> lname)
>>>>>>> CHW-82-84, orphan branch
