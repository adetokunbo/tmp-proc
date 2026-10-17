{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Test.HttpBin
Copyright   : (c) 2026 Tim Emiola
Maintainer  : adetokunbo@users.noreply.github.com
SPDX-License-Identifier: BSD3

Test helper types and functions for testing with an httpbin container.
-}
module Test.HttpBin where

import qualified Data.ByteString.Char8 as C8
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status (statusCode)
import System.TmpProc
  ( HandlesOf
  , HostIpAddress
  , Pinged (..)
  , Proc (..)
  , ProcHandle (..)
  , SvcURI
  , manyNamed
  , startupAll
  , toPinged
  , (&:)
  , (&:&)
  )
import Test.NginxGateway (NginxGateway (..))


anNginxGateway :: NginxGateway
anNginxGateway =
  NginxGateway
    { ngCommonName = "localhost"
    , ngTargetPort = 80
    , ngTargetName = "http-bin-test-3"
    }


setupHandles :: IO (HandlesOf '[HttpBinTest, NginxGateway, HttpBinTest3])
setupHandles = startupAll $ HttpBinTest &: anNginxGateway &:& HttpBinTest3


-- | A data type representing a connection to a HttpBin server.
data HttpBinTest = HttpBinTest


-- | Run HttpBin as temporary process.
instance Proc HttpBinTest where
  type Image HttpBinTest = "kennethreitz/httpbin"
  type Name HttpBinTest = "http-bin-test"
  uriOf = httpUri
  runArgs = []
  reset _ = pure ()
  ping = pingHttp
  uriOfRootlessWith = httpUriRootless


{- | Another data type representing a connection to a HttpBin server.

Used in this module to allow mulitple types in test lists, to improve the
chances of detecting type-related compilationr errors.
-}
data HttpBinTest2 = HttpBinTest2


-- | Run HttpBin as temporary process.
instance Proc HttpBinTest2 where
  type Image HttpBinTest2 = "kennethreitz/httpbin"
  type Name HttpBinTest2 = "http-bin-test-2"
  uriOf = httpUri
  runArgs = []
  reset _ = pure ()
  ping = pingHttp
  uriOfRootlessWith = httpUriRootless


{- | Yet another data type representing a connection to a HttpBin server.

Used in this module to allow mulitple types in test lists, to improve the
chances of detecting type-related compilationr errors.
-}
data HttpBinTest3 = HttpBinTest3


-- | Run HttpBin as temporary process.
instance Proc HttpBinTest3 where
  type Image HttpBinTest3 = "kennethreitz/httpbin"
  type Name HttpBinTest3 = "http-bin-test-3"
  uriOf = httpUri
  runArgs = []
  reset _ = pure ()
  ping = pingHttp
  uriOfRootlessWith = httpUriRootless


-- | Make a uri to access the http-bin server.
httpUri :: HostIpAddress -> SvcURI
httpUri ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


-- | Make a rootless-aware uri for the http-bin server using the mapped host port for port 80.
httpUriRootless :: HostIpAddress -> [(Word16, Word16)] -> SvcURI
httpUriRootless addr pairs =
  case lookup 80 pairs of
    Just hp -> "http://" <> C8.pack (Text.unpack addr) <> ":" <> C8.pack (show hp) <> "/"
    Nothing -> httpUri addr


pingHttp :: ProcHandle a -> IO Pinged
pingHttp handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- httpGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


-- | Determine the status from a GET request, using the handle's URI as the base.
httpGet :: ProcHandle a -> Text -> IO Int
httpGet handle urlPath = do
  let theUri = C8.unpack (hUri handle) <> Text.unpack (Text.dropWhile (== '/') urlPath)
  manager <- HC.newManager HC.defaultManagerSettings
  getReq <- HC.parseRequest theUri
  statusCode . HC.responseStatus <$> HC.httpLbs getReq manager


-- | Verify that the compile time type computations related to 'manyNamed' are ok.
typeLevelCheck1 :: IO (HandlesOf '[HttpBinTest3])
typeLevelCheck1 = do
  allHandles <- setupHandles
  pure $ manyNamed @'["http-bin-test-3"] Proxy allHandles


typeLevelCheck2 :: IO (HandlesOf '[HttpBinTest, HttpBinTest3])
typeLevelCheck2 = do
  allHandles <- setupHandles
  pure $ manyNamed @'["http-bin-test", "http-bin-test-3"] Proxy allHandles


typeLevelCheck3 :: IO (HandlesOf '[HttpBinTest3, HttpBinTest])
typeLevelCheck3 = do
  allHandles <- setupHandles
  pure $ manyNamed @'["http-bin-test-3", "http-bin-test"] Proxy allHandles


typeLevelCheck4 :: IO (HandlesOf '[HttpBinTest3, NginxGateway, HttpBinTest])
typeLevelCheck4 = do
  allHandles <- setupHandles
  pure $ manyNamed @'["http-bin-test-3", "nginx-test", "http-bin-test"] Proxy allHandles
