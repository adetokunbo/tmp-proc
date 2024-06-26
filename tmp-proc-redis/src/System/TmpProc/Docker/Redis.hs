{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Provides an instance of 'Proc' that launches @redis@ as a @tmp proc@.

The instance this module provides can be used in integration tests as is.

It's also possible to write other instances that launch @redis@ in different
ways; for those, this instance can be used as a reference example.
-}
module System.TmpProc.Docker.Redis
  ( -- * 'Proc' instance
    TmpRedis (..)

    -- * Useful definitions
  , aProc
  , aHandle
  , KeyName

    -- * Re-exports
  , module System.TmpProc
  )
where

import Control.Exception (catch)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
import Database.Redis
  ( ConnectTimeout
  , Connection
  , checkedConnect
  , del
  , disconnect
  , parseConnectInfo
  , runRedis
  )
import System.TmpProc
  ( Connectable (..)
  , HList (..)
  , HandlesOf
  , HostIpAddress
  , Pinged (..)
  , Proc (..)
  , ProcHandle (..)
  , SvcURI
  , only
  , startupAll
  , withTmpConn
  )


-- | A singleton 'HList' containing an example 'TmpRedis'.
aProc :: HList '[TmpRedis]
aProc = only $ TmpRedis []


-- | An 'HList' that just contains the handle created from 'aProc'.
aHandle :: IO (HandlesOf '[TmpRedis])
aHandle = startupAll aProc


-- | The name of a key in redis.
type KeyName = C8.ByteString


{- | Provides the capability to launch a redis instance as @tmp proc@.

The constructor receives the names of keys to be dropped on 'reset'.
-}
newtype TmpRedis = TmpRedis [KeyName]


-- | Specifies how to run @redis@ as a @tmp proc@.
instance Proc TmpRedis where
  type Image TmpRedis = "redis:5.0"
  type Name TmpRedis = "a-redis-db"
  uriOf = mkUri'
  runArgs = []
  ping = toPinged . flip withTmpConn (const $ pure ())
  reset = clearKeys


-- | Specifies how to connect to a tmp @redis@ service.
instance Connectable TmpRedis where
  type Conn TmpRedis = Connection
  closeConn = disconnect
  openConn = openConn'


openConn' :: ProcHandle TmpRedis -> IO Connection
openConn' handle = case parseConnectInfo $ C8.unpack $ hUri handle of
  Left _ -> fail $ "invalid redis uri: " ++ C8.unpack (hUri handle)
  Right x -> checkedConnect x


toPinged :: IO a -> IO Pinged
toPinged action =
  ( (action >> pure OK)
      `catch` (\(_ :: ConnectTimeout) -> pure NotOK)
  )
    `catch` (\(_ :: IOError) -> pure NotOK)


mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "redis://" <> C8.pack (Text.unpack ip) <> "/"


clearKeys :: ProcHandle TmpRedis -> IO ()
clearKeys handle@ProcHandle {hProc} =
  let go (TmpRedis []) = pure ()
      go (TmpRedis keys) = withTmpConn handle $ \c -> runRedis c $ void $ del keys
   in go hProc
