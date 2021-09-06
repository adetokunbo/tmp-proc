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
Maintainer  : tim.emiola@gmail.com
Stability   : experimental
-}
module System.TmpProc.Docker.Redis
  ( -- * data types
    TmpRedis(..)

    -- * useful definitions
  , aProc
  , aHandle
  , withTmpConnection

    -- * key names
  , KeyName

    -- * module re-exports
  , module System.TmpProc.Docker
  )
where

import           Control.Exception     (bracket)
import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text             as Text

import           Database.Redis        (Connection, checkedConnect, del,
                                        disconnect, parseConnectInfo, runRedis)

import           System.TmpProc.Docker (HList (..), HostIpAddress, Proc (..),
                                        Proc2Handle, ProcHandle (..), SvcURI,
                                        startupAll)


{-| Run an action on redis using a connection to 'ProcHandle' 'TmpRedis'. -}
withTmpConnection :: ProcHandle TmpRedis -> (Connection -> IO a) -> IO a
withTmpConnection handle action = case parseConnectInfo $ C8.unpack $ hUri handle of
  Left e  -> error e
  Right x -> bracket (checkedConnect x) disconnect action


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
  ping = flip withTmpConnection (const $ pure ())
  reset = clearKeys


mkUri' :: HostIpAddress -> SvcURI
mkUri' ip =  "redis://" <> (C8.pack $ Text.unpack ip) <> "/"


clearKeys :: ProcHandle TmpRedis -> IO ()
clearKeys handle = withTmpConnection handle $ \c ->
  runRedis c $ void $ del $ keysOf $ hProc handle
  where keysOf (TmpRedis keys) = keys
