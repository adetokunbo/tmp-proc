{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Provides an instance of 'Proc' that launches @RabbitMQ@ as a @tmp proc@.

The instance this module provides can be used in integration tests as is.

It's also possible to write other instances that launch @RabbitMQ@ in different
ways; for those, this instance can be used as a reference example.
-}
module System.TmpProc.Docker.RabbitMQ
  ( -- * 'Proc' instance
    TmpRabbitMQ (..)

    -- * Useful definitions
  , aProc
  , aHandle

    -- * Re-exports
  , module System.TmpProc
  )
where

import qualified Data.ByteString.Char8 as C8
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Network.AMQP
import System.TmpProc
  ( Connectable (..)
  , HList (..)
  , HandlesOf
  , HostIpAddress
  , Proc (..)
  , ProcHandle (..)
  , SvcURI
  , only
  , startupAll
  , toPinged
  , withTmpConn
  )


-- | A singleton 'HList' containing a 'TmpRabbitMQ'.
aProc :: HList '[TmpRabbitMQ]
aProc = only TmpRabbitMQ


-- | An 'HList' that just contains the handle created by 'aProc'.
aHandle :: IO (HandlesOf '[TmpRabbitMQ])
aHandle = startupAll aProc


-- | Provides the capability to launch a RabbitMQ instance as a @tmp proc@.
data TmpRabbitMQ = TmpRabbitMQ


-- | Specifies how to run @RabbitMQ@ as a @tmp proc@.
instance Proc TmpRabbitMQ where
  type Image TmpRabbitMQ = "rabbitmq:3.9"
  type Name TmpRabbitMQ = "a-rabbitmq-server"


  uriOf = mkUri'
  runArgs = []
  ping = toPinged @AMQPException Proxy . openConn'
  reset _ = pure ()
  pingGap = 3 * 1000000


-- | Specifies how to connect to a tmp @RabbitMQ@ service.
instance Connectable TmpRabbitMQ where
  type Conn TmpRabbitMQ = Connection


  openConn = openConn'
  closeConn = closeConnection


-- | Makes a uri using the guest password .
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip =
  "amqp://guest:guest@"
    <> C8.pack (Text.unpack ip)
    <> ":5672@/%2f"


openConn' :: ProcHandle TmpRabbitMQ -> IO Connection
openConn' = openConnection'' . fromURI . C8.unpack . hUri
