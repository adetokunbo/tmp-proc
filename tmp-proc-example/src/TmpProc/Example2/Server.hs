{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module TmpProc.Example2.Server
  ( -- * Server implementation
    AppEnv(..)
  , runServer'
  , runServer
  , waiApp
  ) where

import           Control.Exception          (try, throw)
import           Control.Monad.Catch        (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks,
                                             runReaderT)
import           Control.Monad.Trans.Except (ExceptT (..))
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (Port, run)
import           Servant.API                ((:<|>) (..))
import           Servant.Server             (Handler (..), ServerT,
                                             err401, errBody, serve, hoistServer)

import           TmpProc.Example2.Routes    (ContactsAPI, contactsAPI)
import           TmpProc.Example2.Schema    (Contact, ContactID)

import qualified TmpProc.Example2.Cache     as Cache
import qualified TmpProc.Example2.Database  as DB


{-| Runs 'waiApp' on the given port. -}
runServer' :: IO AppEnv -> Port -> IO ()
runServer' mkEnv port = mkEnv >>= run port . waiApp


{-| An 'Application' that runs the server using the given DB and Cache. -}
waiApp :: AppEnv -> Application
waiApp env =
  let
    hoist' = Handler . ExceptT . try . runApp' env
  in
    serve contactsAPI $ hoistServer contactsAPI hoist' server


{-| Runs 'waiApp' using defaults for local development. -}
runServer :: IO ()
runServer = runServer' defaultEnv 8000


fetchContact
  :: (MonadIO m, MonadReader r m, Has DB.Locator r, Has Cache.Connection r)
  => ContactID -> m Contact
fetchContact cid = do
  cache <- grab @Cache.Connection
  (liftIO $ Cache.loadContact cache cid) >>= \case
    Just contact -> pure contact
    Nothing -> do
      db <- grab @DB.Locator
      (liftIO $ DB.fetch db cid) >>= \case
        Just contact -> liftIO (Cache.saveContact cache cid contact) >> pure contact
        Nothing -> throw $ err401 { errBody = "No Contact with this ID" }


createContact
  :: (MonadIO m, MonadReader r m, Has DB.Locator r)
  => Contact -> m ContactID
createContact contact = do
  db <- grab @DB.Locator
  liftIO $ DB.create db contact


server
  :: ( Has Cache.Connection r
     , Has DB.Locator r
     , MonadReader r m
     , MonadIO m
     )
  => ServerT ContactsAPI m
server = fetchContact :<|> createContact


-- | The application-level Monad, provides access to AppEnv via @Reader AppEnv@.
newtype App a = App
  { runApp :: ReaderT AppEnv IO a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadCatch
             , MonadMask
             , MonadThrow
             , MonadReader AppEnv
             , MonadIO
             )


instance Has DB.Locator     AppEnv  where obtain = aeDbLocator
instance Has Cache.Connection  AppEnv  where obtain = aeCacheLocator


defaultEnv :: IO AppEnv
defaultEnv = AppEnv <$> (pure DB.defaultLoc) <*> Cache.defaultConn


-- | Run a 'App' computation with the given environment.
runApp' :: AppEnv -> App a -> IO a
runApp' env = flip runReaderT env . runApp


{-| An application-level environment suitable for storing in a Reader. -}
data AppEnv = AppEnv
  { aeDbLocator    :: !(DB.Locator)
  , aeCacheLocator :: !(Cache.Connection)
  }


{- | General type class representing which @field@ is in @env@. -}
class Has field env where
  obtain :: env -> field


-- | A combinator that simplifies accessing 'Has' fields.
grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
