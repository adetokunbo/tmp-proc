{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.SimpleServer (
  -- * functions
  statusOfGet,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HC
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

-- statusOfGet' :: Int -> Text -> IO Int
-- statusOfGet' p path = do
--   manager <- mkSimpleTLSManager
--   runReq (defaultHttpConfig { httpConfigAltManager = Just manager }) $ do
--     r <- req GET (localHttpsUrl path) NoReqBody ignoreResponse $ port p
--     return $ responseStatusCode r

-- localHttpsUrl :: Text -> Url 'Https
-- localHttpsUrl p = foldl' (/:) (https "localhost")
--   $ Text.splitOn "/" $ Text.dropWhile (== '/') p

-- mkSimpleTLSManager :: IO HC.Manager
-- mkSimpleTLSManager = HC.newManager
--   $ mkManagerSettings (HC.TLSSettingsSimple True False False) Nothing
