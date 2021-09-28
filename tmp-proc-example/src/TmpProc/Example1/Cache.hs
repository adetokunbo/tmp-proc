{-# LANGUAGE LambdaCase #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Implements a cache for the demo service

-}
module TmpProc.Example1.Cache
  ( -- * Cache services
    deleteContact
  , loadContact
  , saveContact
  , runRedisAction

    -- * Redis location
  , Locator
  , defaultLoc
  )
where

import           Control.Monad (void)
import           Data.ByteString.Char8 (pack, unpack, ByteString)
import           Database.Redis

import           TmpProc.Example1.Schema

{-| Specifies the @Redis@ instance to use as a cache .-}
type Locator = ConnectInfo

{-| A default for local development .-}
defaultLoc :: Locator
defaultLoc = defaultConnectInfo

runRedisAction :: Locator -> Redis a -> IO a
runRedisAction loc action = do
  connection <- connect loc
  runRedis connection action

saveContact :: Locator -> ContactID -> Contact -> IO ()
saveContact loc cid contact = runRedisAction loc $ void $ setex (toKey cid) 3600 (pack . show $ contact)

loadContact :: Locator -> ContactID -> IO (Maybe Contact)
loadContact loc cid = runRedisAction loc $ do
  (get $ toKey cid) >>= \case
    Right (Just contactString) -> return $ Just (read . unpack $ contactString)
    _ -> return Nothing

deleteContact :: Locator -> ContactID -> IO ()
deleteContact loc cid = do
  connection <- connect loc
  runRedis connection $ do
    _ <- del [pack . show $ cid]
    return ()


toKey :: ContactID -> ByteString
toKey = pack . show
