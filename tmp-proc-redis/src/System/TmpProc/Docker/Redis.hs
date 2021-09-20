{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : System.TmpProc.Docker.Redis
Description : Provides an instance of @Proc@ for launching redis as a tmp process.
Copyright   : (c) 2021, Tim Emiola
License     : BSD
Maintainer  : adetokunbo@users.noreply.github.com
Stability   : experimental
-}
module System.TmpProc.Docker.Redis
  ( -- * data types
    TmpRedis(..)

    -- * useful definitions
  , aProc
  , aHandle

    -- * key names
  , KeyName

    -- * module re-exports
  , module System.TmpProc.Docker
  )
where

import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text             as Text

import           Database.Redis        (Connection, checkedConnect, del,
                                        disconnect, parseConnectInfo, runRedis)

import           System.TmpProc.Docker (HList (..), HostIpAddress, Proc (..),
                                        Proc2Handle, ProcHandle (..), SvcURI,
                                        startupAll, Connectable(..), withTmpConn)


{-| A singleton 'HList' containing an example 'TmpRedis'. -}
aProc :: HList '[TmpRedis]
aProc = TmpRedis [] `HCons` HNil


{-| An 'HList' that just contains the handle created from 'aProc'. -}
aHandle :: IO (HList (Proc2Handle '[TmpRedis]))
aHandle = startupAll aProc


-- | The name of a key in redis.
type KeyName = C8.ByteString


{-| Represents a tmp redis instance that wi

It specifies the names of keys to be dropped during a reset.

-}
data TmpRedis = TmpRedis [KeyName]


{-| A 'Proc' for running redis as a tmp process. -}
instance Proc TmpRedis where
  type Image TmpRedis = "redis:5.0"
  type Name TmpRedis = "a-redis-db"

  uriOf = mkUri'
  runArgs = []
  ping = flip withTmpConn (const $ pure ())
  reset = clearKeys

instance Connectable TmpRedis where
  type Conn TmpRedis = Connection

  closeConn = disconnect
  openConn = openConn'


openConn' :: ProcHandle TmpRedis -> IO Connection
openConn' handle =  case parseConnectInfo $ C8.unpack $ hUri handle of
  Left e  -> error e
  Right x -> checkedConnect x


mkUri' :: HostIpAddress -> SvcURI
mkUri' ip =  "redis://" <> (C8.pack $ Text.unpack ip) <> "/"


clearKeys :: ProcHandle TmpRedis -> IO ()
clearKeys handle@(ProcHandle {hProc}) =
  let go (TmpRedis []) = pure ()
      go (TmpRedis keys) = withTmpConn handle $ \c -> runRedis c $ void $ del keys
  in
    go hProc
