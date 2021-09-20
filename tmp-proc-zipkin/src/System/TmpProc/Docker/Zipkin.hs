{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Provides an instance of @Proc@ for launching Zipkin as a tmp process.

-}
module System.TmpProc.Docker.Zipkin
  ( -- * data types
    TmpZipkin(..)

    -- * useful definitions
  , aProc
  , aHandle

    -- * module re-exports
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


{-| Represents a connection to a Zipkin instance running on docker. -}
data TmpZipkin = TmpZipkin


{-| A 'Proc' for running ZipKin as a tmp process. -}
instance Proc TmpZipkin where
  type Image TmpZipkin = "openzipkin/zipkin-slim"
  type Name TmpZipkin = "a-zipkin-server"

  uriOf = mkUri'
  runArgs = []
  ping h = openConn' h >>= ZPK.run tracedPing

  reset _ = pure ()


instance Connectable TmpZipkin where
  -- * 'Zipkin'  is the closest to a connection type in tracing-control.
  type Conn TmpZipkin = ZPK.Zipkin

  openConn = openConn'
  closeConn _ = pure ()


openConn' :: ProcHandle TmpZipkin -> IO ZPK.Zipkin
openConn' = ZPK.new . toSettings


{-| Make a simple HTTP  uri to the zipkin server. -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


toSettings :: ProcHandle TmpZipkin -> ZPK.Settings
toSettings = fromString . Text.unpack . hAddr


pingAction :: IO ()
pingAction = putStrLn "the trace of this will be sent as a ping"


tracedPing :: (MonadIO m, MonadTrace m) => m ()
tracedPing = rootSpan alwaysSampled "ping" $ liftIO pingAction
