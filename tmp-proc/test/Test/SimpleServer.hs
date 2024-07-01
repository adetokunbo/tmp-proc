{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SimpleServer
  ( -- * functions
    statusOfGet
  , statusOfGet'
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Network.Connection.CPP (noCheckSettings)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp


-- | Determine the status from a Get on localhost.
statusOfGet :: Warp.Port -> Text -> IO Int
statusOfGet p urlPath = do
  let theUri = "GET http://localhost/" <> Text.dropWhile (== '/') urlPath
  manager <- HC.newManager HC.defaultManagerSettings
  getReq <- HC.parseRequest $ Text.unpack theUri
  let theReq = getReq {HC.port = p}
  statusCode . HC.responseStatus <$> HC.httpLbs theReq manager


-- | Determine the status from a secure Get on localhost.
statusOfGet' :: Warp.Port -> Text -> IO Int
statusOfGet' p urlPath = do
  let theUri = "GET https://localhost/" <> Text.dropWhile (== '/') urlPath
  manager <- HC.newManager $ mkManagerSettings noCheckSettings Nothing
  getReq <- HC.parseRequest $ Text.unpack theUri
  let theReq = getReq {HC.port = p}
  statusCode . HC.responseStatus <$> HC.httpLbs theReq manager
