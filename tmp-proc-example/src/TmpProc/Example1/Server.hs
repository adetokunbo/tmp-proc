{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TmpProc.Example1.Server
  ( -- * Server implementation
    runServer'
  , runServer
  , waiApp
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (Port, run)
import           Servant.API                ((:<|>) (..))
import           Servant.Server             (Handler (..), Server, err401,
                                             errBody, serve)

import           TmpProc.Example1.Routes    (ContactsAPI, contactsAPI)
import           TmpProc.Example1.Schema    (Contact, ContactID)

import qualified TmpProc.Example1.Cache     as Cache
import qualified TmpProc.Example1.Database  as DB


{-| Runs 'waiApp' on the given port. -}
runServer' :: Port -> DB.Locator -> Cache.Locator -> IO ()
runServer' port dbLoc cacheLoc = run port $ waiApp dbLoc cacheLoc


{-| An 'Application' that runs the server using the given DB and Cache. -}
waiApp :: DB.Locator -> Cache.Locator -> Application
waiApp dbLoc cacheLoc = serve contactsAPI $ server dbLoc cacheLoc


{-| Runs 'waiApp' using defaults for local development. -}
runServer :: IO ()
runServer = runServer' 8000 DB.defaultLoc Cache.defaultLoc


fetchContact :: DB.Locator -> Cache.Locator -> ContactID -> Handler Contact
fetchContact dbLoc cacheLoc cid = do
  (liftIO $ Cache.loadContact cacheLoc cid) >>= \case
    Just contact -> pure contact
    Nothing -> (liftIO $ DB.fetch dbLoc cid) >>= \case
      Just contact -> liftIO (Cache.saveContact cacheLoc cid contact) >> pure contact
      Nothing -> Handler $ (throwE $ err401 { errBody = "No Contact with this ID" })


createContact :: DB.Locator -> Contact -> Handler ContactID
createContact dbLoc contact = liftIO $ DB.create dbLoc contact


server :: DB.Locator -> Cache.Locator -> Server ContactsAPI
server dbLoc cacheLoc =
  (fetchContact dbLoc cacheLoc) :<|>
  (createContact dbLoc)
