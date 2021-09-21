{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_HADDOCK prune not-home #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Provides an instance of 'Proc' that launches @ZipKin@ as a @tmp proc@.

The instance this module provides can be used in integration tests as is.

It's also possible to write other instances that launch @ZipKin@ in different
ways; for those, this instance can be used as a reference example.

-}
module System.TmpProc.Docker.Zipkin
  ( -- * 'Proc' instance
    TmpZipkin(..)

    -- * Useful definitions
  , aProc
  , aHandle

    -- * Re-exports
  , module System.TmpProc.Docker
  )
where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trace.Class (MonadTrace, rootSpan, alwaysSampled)
import qualified Data.ByteString.Char8     as C8
import           Data.String               (fromString)
import qualified Data.Text                 as Text


import qualified Monitor.Tracing.Zipkin    as ZPK

import           System.TmpProc.Docker     (Connectable (..), HList (..),
                                            HostIpAddress, Proc (..),
                                            Proc2Handle, ProcHandle (..),
                                            SvcURI, startupAll, withTmpConn)


{-| A singleton 'HList' containing a 'TmpZipkin'. -}
aProc :: HList '[TmpZipkin]
aProc = TmpZipkin `HCons` HNil


{-| An 'HList' that just contains the handle created by 'aProc'. -}
aHandle :: IO (HList (Proc2Handle '[TmpZipkin]))
aHandle = startupAll aProc


{-| Provides the capability to launch a Zipkin instance as a @tmp proc@. -}
data TmpZipkin = TmpZipkin


{-| Specifies how to run @ZipKin@ as a @tmp proc@. -}
instance Proc TmpZipkin where
  type Image TmpZipkin = "openzipkin/zipkin-slim"
  type Name TmpZipkin = "a-zipkin-server"

  uriOf = mkUri'
  runArgs = []
  ping h = openConn' h >>= ZPK.run tracedPing

  reset _ = pure ()


{-| Specifies how to connect to a tmp @ZipKin@ service.

In this case, there is not really a connection type, but 'ZPK.Zipkin' provides
a close analogue.

-}
instance Connectable TmpZipkin where
  type Conn TmpZipkin = ZPK.Zipkin

  openConn = openConn'
  closeConn _ = pure ()


openConn' :: ProcHandle TmpZipkin -> IO ZPK.Zipkin
openConn' = ZPK.new . toSettings


{-| Make a simple HTTP uri to the zipkin server. -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


toSettings :: ProcHandle TmpZipkin -> ZPK.Settings
toSettings = fromString . Text.unpack . hAddr


pingAction :: IO ()
pingAction = putStrLn "the trace of this will be sent as a ping"


tracedPing :: (MonadIO m, MonadTrace m) => m ()
tracedPing = rootSpan alwaysSampled "ping" $ liftIO pingAction
