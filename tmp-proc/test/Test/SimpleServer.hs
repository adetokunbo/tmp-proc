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
import qualified Network.Connection as HC
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp


-- -- | The settings used in the integration tests
-- defaultTLSSettings :: IO Warp.TLSSettings
-- defaultTLSSettings =
--   Warp.tlsSettings
--  <$> (getDataFileName "test_certs/certificate.pem")
--  <*> (getDataFileName "test_certs/key.pem")

-- | Determine the status from a Get on localhost.
statusOfGet :: Warp.Port -> Text -> IO Int
statusOfGet p urlPath = do
  let theUri = "GET http://localhost/" <> Text.dropWhile (== '/') urlPath
  manager <- HC.newManager HC.defaultManagerSettings
  getReq <- HC.parseRequest $ Text.unpack theUri
  let theReq = getReq {HC.port = p}
  statusCode . HC.responseStatus <$> HC.httpLbs theReq manager


-- | Determine the status from a Get on localhost.
statusOfGet' :: Warp.Port -> Text -> IO Int
statusOfGet' p urlPath = do
  let theUri = "GET https://localhost/" <> Text.dropWhile (== '/') urlPath
  manager <- mkSimpleTLSManager
  getReq <- HC.parseRequest $ Text.unpack theUri
  let theReq = getReq {HC.port = p}
  statusCode . HC.responseStatus <$> HC.httpLbs theReq manager


mkSimpleTLSManager :: IO HC.Manager
mkSimpleTLSManager =
  HC.newManager $
    mkManagerSettings (HC.TLSSettingsSimple True False False) Nothing
