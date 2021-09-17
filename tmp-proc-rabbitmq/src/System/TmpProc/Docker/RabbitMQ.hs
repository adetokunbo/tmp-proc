{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Provides an instance of @Proc@ for launching RabbitMQ as a tmp process.

-}
module System.TmpProc.Docker.RabbitMQ
  ( -- * data types
    TmpRabbitMQ(..)

    -- * useful definitions
  , aProc
  , aHandle

    -- * module re-exports
  , module System.TmpProc.Docker
  )
where

import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text             as Text

import           Network.AMQP

import           System.TmpProc.Docker (Connectable (..), HList (..),
                                        HostIpAddress, Proc (..), Proc2Handle,
                                        ProcHandle (..), SvcURI, startupAll,
                                        withTmpConn)


{-| A singleton 'HList' containing a 'TmpRabbitMQ'. -}
aProc :: HList '[TmpRabbitMQ]
aProc = TmpRabbitMQ `HCons` HNil


{-| An 'HList' that just contains the handle created by 'aProc'. -}
aHandle :: IO (HList (Proc2Handle '[TmpRabbitMQ]))
aHandle = startupAll aProc


{-| Represents a connection to a RabbitMQ instance running on docker. -}
data TmpRabbitMQ = TmpRabbitMQ


{-| A 'Proc' for running postgres as a tmp process. -}
instance Proc TmpRabbitMQ where
  type Image TmpRabbitMQ = "rabbitmq:3.9"
  type Name TmpRabbitMQ = "a-rabbitmq-server"

  uriOf = mkUri'
  runArgs = []
  ping = void . openConn'
  reset _ = pure ()
  pingGap = 3 * 1000000

instance Connectable TmpRabbitMQ where
  type Conn TmpRabbitMQ = Connection

  openConn = openConn'
  closeConn = closeConnection


{-| Makes a uri using the guest password . -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip =
  "amqp://guest:guest@"
  <> C8.pack (Text.unpack ip)
  <> ":5672@/%2f"


openConn' :: ProcHandle TmpRabbitMQ -> IO Connection
openConn' = openConnection'' . fromURI . C8.unpack . hUri
