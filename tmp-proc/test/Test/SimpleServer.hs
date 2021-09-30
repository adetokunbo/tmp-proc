{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.SimpleServer
  ( -- * functions
    statusOfGet
  , statusOfGet'

    -- * test constants
  , defaultTLSSettings
  )
where

import           Data.List                   (foldl')

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Network.Connection          as HC
import qualified Network.HTTP.Client         as HC
import           Network.HTTP.Client.TLS     (mkManagerSettings)
import           Network.HTTP.Req
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import           Paths_tmp_proc              (getDataFileName)


-- | The settings used in the integration tests
defaultTLSSettings :: IO Warp.TLSSettings
defaultTLSSettings =
  Warp.tlsSettings
 <$> (getDataFileName "test_certs/certificate.pem")
 <*> (getDataFileName "test_certs/key.pem")


-- | Determine the status from a Get on localhost.
statusOfGet :: Warp.Port -> Text -> IO Int
statusOfGet p path = runReq defaultHttpConfig $ do
  r <- req GET (localUrl path) NoReqBody ignoreResponse $ port p
  return $ responseStatusCode r


statusOfGet' :: Int -> Text -> IO Int
statusOfGet' p path = do
  manager <- mkSimpleTLSManager
  runReq (defaultHttpConfig { httpConfigAltManager = Just manager }) $ do
    r <- req GET (localHttpsUrl path) NoReqBody ignoreResponse $ port p
    return $ responseStatusCode r


localUrl :: Text -> Url 'Http
localUrl p = foldl' (/:) (http "localhost")
  $ Text.splitOn "/" $ Text.dropWhile (== '/') p


localHttpsUrl :: Text -> Url 'Https
localHttpsUrl p = foldl' (/:) (https "localhost")
  $ Text.splitOn "/" $ Text.dropWhile (== '/') p


mkSimpleTLSManager :: IO HC.Manager
mkSimpleTLSManager = HC.newManager
  $ mkManagerSettings (HC.TLSSettingsSimple True False False) Nothing
