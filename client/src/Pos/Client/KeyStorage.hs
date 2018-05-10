{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Client.KeyStorage
<<<<<<< HEAD
       ( MonadKeysRead (..)
       , MonadKeys (..)
       , getSecretDefault
       , modifySecretPureDefault
       , modifySecretDefault

       , getPrimaryKey
       , getSecretKeys
       , getSecretKeysPlain
       , addSecretKey
       , deleteAllSecretKeys
       , deleteSecretKeyBy
       , newSecretKey
       , KeyData
       , KeyError (..)
       , AllUserSecrets (..)
       , keyDataFromFile
       ) where
=======
    ( MonadKeysRead (..)
    , MonadKeys (..)
    , AllUserSecrets (..)
    , AllUserPublics (..)
    , getSecretDefault
    , getPublicDefault
    , modifySecretPureDefault
    , modifyPublicPureDefault
    , modifySecretDefault
    , modifyPublicDefault
    , getSecretKeys
    , getPublicKeys
    , getSecretKeysPlain
    , getPublicKeysPlain
    , addSecretKey
    , addPublicKey
    , deleteAllSecretKeys
    , deleteAllPublicKeys
    , deleteSecretKeyBy
    , deletePublicKeyBy
    ) where
>>>>>>> CHW-82-84, orphan branch

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens ((<%=), (<>~))
import           Serokell.Util (modifyTVarS)
<<<<<<< HEAD
import           System.Wlog (WithLogger)

import           Pos.Binary.Crypto ()
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, SecretKey, hash, runSecureRandom,
                             safeKeyGen)
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret, peekUserSecret, usKeys,
                                      usPrimKey, writeUserSecret)

type KeyData = TVar UserSecret
=======

import           Pos.Binary.Crypto ()
import           Pos.Crypto (EncryptedSecretKey, PublicKey, hash)
import           Pos.Util.UserSecret (HasUserSecret (..), UserSecret, usKeys,
                                      writeUserSecret)
import           Pos.Util.UserPublic (HasUserPublic (..), UserPublic, upKeys,
                                      writeUserPublic)
>>>>>>> CHW-82-84, orphan branch

----------------------------------------------------------------------
-- MonadKeys class and default functions
----------------------------------------------------------------------

class Monad m => MonadKeysRead m where
    getSecret :: m UserSecret
<<<<<<< HEAD

class MonadKeysRead m => MonadKeys m where
    modifySecret :: (UserSecret -> UserSecret) -> m ()
=======
    getPublic :: m UserPublic

class MonadKeysRead m => MonadKeys m where
    modifySecret :: (UserSecret -> UserSecret) -> m ()
    modifyPublic :: (UserPublic -> UserPublic) -> m ()
>>>>>>> CHW-82-84, orphan branch

type HasKeysContext ctx m =
    ( MonadReader ctx m
    , HasUserSecret ctx
<<<<<<< HEAD
=======
    , HasUserPublic ctx
>>>>>>> CHW-82-84, orphan branch
    , MonadIO m
    )

getSecretDefault :: HasKeysContext ctx m => m UserSecret
getSecretDefault = view userSecret >>= atomically . STM.readTVar

<<<<<<< HEAD
=======
getPublicDefault :: HasKeysContext ctx m => m UserPublic
getPublicDefault = view userPublic >>= atomically . STM.readTVar

>>>>>>> CHW-82-84, orphan branch
modifySecretPureDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretPureDefault f = do
    us <- view userSecret
    void $ atomically $ modifyTVarS us (identity <%= f)

<<<<<<< HEAD
=======
modifyPublicPureDefault :: HasKeysContext ctx m => (UserPublic -> UserPublic) -> m ()
modifyPublicPureDefault f = do
    up <- view userPublic
    void $ atomically $ modifyTVarS up (identity <%= f)

>>>>>>> CHW-82-84, orphan branch
modifySecretDefault :: HasKeysContext ctx m => (UserSecret -> UserSecret) -> m ()
modifySecretDefault f = do
    us <- view userSecret
    new <- atomically $ modifyTVarS us (identity <%= f)
    writeUserSecret new

<<<<<<< HEAD
=======
modifyPublicDefault :: HasKeysContext ctx m => (UserPublic -> UserPublic) -> m ()
modifyPublicDefault f = do
    up <- view userPublic
    new <- atomically $ modifyTVarS up (identity <%= f)
    writeUserPublic new

>>>>>>> CHW-82-84, orphan branch
----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

<<<<<<< HEAD
getPrimaryKey :: MonadKeysRead m => m (Maybe SecretKey)
getPrimaryKey = view usPrimKey <$> getSecret

=======
>>>>>>> CHW-82-84, orphan branch
newtype AllUserSecrets = AllUserSecrets
    { getAllUserSecrets :: [EncryptedSecretKey]
    } deriving (ToList, Container)

<<<<<<< HEAD
type instance Element AllUserSecrets = EncryptedSecretKey

getSecretKeys :: MonadKeysRead m => m AllUserSecrets
getSecretKeys = AllUserSecrets . view usKeys <$> getSecret
=======
newtype AllUserPublics = AllUserPublics
    { getAllUserPublics :: [PublicKey]
    } deriving (ToList, Container)

type instance Element AllUserSecrets = EncryptedSecretKey
type instance Element AllUserPublics = PublicKey

getSecretKeys :: MonadKeysRead m => m AllUserSecrets
getSecretKeys = AllUserSecrets <$> getSecretKeysPlain

getPublicKeys :: MonadKeysRead m => m AllUserPublics
getPublicKeys = AllUserPublics <$> getPublicKeysPlain
>>>>>>> CHW-82-84, orphan branch

getSecretKeysPlain :: MonadKeysRead m => m [EncryptedSecretKey]
getSecretKeysPlain = view usKeys <$> getSecret

<<<<<<< HEAD
=======
getPublicKeysPlain :: MonadKeysRead m => m [PublicKey]
getPublicKeysPlain = view upKeys <$> getPublic

>>>>>>> CHW-82-84, orphan branch
addSecretKey :: MonadKeys m => EncryptedSecretKey -> m ()
addSecretKey sk = modifySecret $ \us ->
    if view usKeys us `containsKey` sk
    then us
    else us & usKeys <>~ [sk]
<<<<<<< HEAD
=======
  where
    containsKey ls k = hash k `elem` map hash ls

addPublicKey :: MonadKeys m => PublicKey -> m ()
addPublicKey pk = modifyPublic $ \up ->
    if view upKeys up `containsPublicKey` pk
    then up
    else up & upKeys <>~ [pk]
  where
    containsPublicKey = flip elem
>>>>>>> CHW-82-84, orphan branch

deleteAllSecretKeys :: MonadKeys m => m ()
deleteAllSecretKeys = modifySecret (usKeys .~ [])

<<<<<<< HEAD
deleteSecretKeyBy :: MonadKeys m => (EncryptedSecretKey -> Bool) -> m ()
deleteSecretKeyBy predicate = modifySecret (usKeys %~ filter (not . predicate))

-- | Helper for generating a new secret key
newSecretKey :: (MonadIO m, MonadKeys m) => PassPhrase -> m EncryptedSecretKey
newSecretKey pp = do
    (_, sk) <- liftIO $ runSecureRandom $ safeKeyGen pp
    addSecretKey sk
    pure sk

------------------------------------------------------------------------
-- Common functions
------------------------------------------------------------------------

containsKey :: [EncryptedSecretKey] -> EncryptedSecretKey -> Bool
containsKey ls k = hash k `elem` map hash ls

keyDataFromFile :: (MonadIO m, WithLogger m) => FilePath -> m KeyData
keyDataFromFile fp = peekUserSecret fp >>= liftIO . STM.newTVarIO

data KeyError =
    PrimaryKey !Text -- ^ Failed attempt to delete primary key
    deriving (Show)

instance Exception KeyError
=======
deleteAllPublicKeys :: MonadKeys m => m ()
deleteAllPublicKeys = modifyPublic (upKeys .~ [])

deleteSecretKeyBy :: MonadKeys m => (EncryptedSecretKey -> Bool) -> m ()
deleteSecretKeyBy predicate = modifySecret (usKeys %~ filter (not . predicate))

deletePublicKeyBy :: MonadKeys m => (PublicKey -> Bool) -> m ()
deletePublicKeyBy predicate = modifyPublic (upKeys %~ filter (not . predicate))
>>>>>>> CHW-82-84, orphan branch
