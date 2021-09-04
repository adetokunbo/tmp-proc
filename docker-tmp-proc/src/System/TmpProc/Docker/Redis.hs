{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : System.TmpProc.Docker.Postgres
Description : A specification for launching a postgres process
Copyright   : (c)
License     : BSD
Maintainer  : tim@challengehub.com
Stability   : experimental
-}
module System.TmpProc.Docker.Redis
  ( -- * data types
    RdDocker(..)

    -- * useful definitions
  , aProc
  , aHandle

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

import           System.TmpProc.Docker       (HList (..), HostIpAddress, Proc (..),
                                        Proc2Handle, ProcHandle (..), SvcURI,
                                        startupAll)

{-| Contains just 'RdDocker'. -}
aProc :: HList '[RdDocker]
aProc = RdDocker [] `HCons` HNil


{-| The handle created from just 'aProc'. -}
aHandle :: IO (HList (Proc2Handle '[RdDocker]))
aHandle = startupAll aProc


{-| A data type representing a connection to a postgres database running on docker.

It specifies the names of the tables dropped during a reset.

-}
data RdDocker = RdDocker [KeyName]


{-| Specify how to run Postgres as a Tmp 'Proc'.  -}
instance Proc RdDocker where
  type Image RdDocker = "redis:5.0"
  type Name RdDocker = "a-redis-db"

  uriOf = mkUri'
  runArgs = []
  reset = reset'
  ping = flip withConnectionFrom  (const $ pure ()) . hUri


{-| Makes a uri whose password matches the one specified in 'pgRunArgs'. -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "redis://" <> C8.pack (Text.unpack ip) <> "/"


{-| Drop the tables if any are specified. -}
reset' :: ProcHandle RdDocker -> IO ()
reset' ProcHandle {hUri = uri, hProc} =
  let go (RdDocker []) = pure ()
      go (RdDocker keys) = withConnectionFrom uri $ \c ->
          runRedis c $ void $ del keys
  in go hProc


{-| The name of a key in redis. -}
type KeyName = C8.ByteString


withConnectionFrom :: SvcURI -> (Connection -> IO()) -> IO()
withConnectionFrom uri action = case parseConnectInfo $ C8.unpack uri of
  Left e  -> error e
  Right x -> bracket (checkedConnect x) disconnect action
