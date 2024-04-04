{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.HttpBin where

import qualified Data.ByteString.Char8 as C8
import Data.Default (Default (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HC
import Network.HTTP.Types.Header (hHost)
import Network.HTTP.Types.Status (statusCode)
import Network.TLS (ClientParams (..), HostName, Shared (..), Supported (..), defaultParamsClient)
import Network.TLS.Extra (ciphersuite_default)
import Paths_tmp_proc
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.ByteString (getEffectiveGroupID, getEffectiveUserID)
import System.Posix.Types (GroupID, UserID)
import System.TmpProc
  ( HandlesOf
  , HostIpAddress
  , Pinged (..)
  , Preparer (..)
  , Proc (..)
  , ProcHandle (..)
  , SvcURI
  , ToRunCmd (..)
  , manyNamed
  , startupAll
  , toPinged
  , (&:)
  , (&:&)
  )
import System.X509 (getSystemCertificateStore)
import Test.Certs.Temp (CertPaths (..), defaultConfig, generateAndStore)
import Text.Mustache
  ( ToMustache (..)
  , automaticCompile
  , object
  , substitute
  , (~>)
  )


setupHandles :: IO (HandlesOf '[HttpBinTest, NginxTest, HttpBinTest3])
setupHandles = startupAll $ HttpBinTest &: anNginxTest &:& HttpBinTest3


-- | A data type that configures a tmp proc nginx reverse proxy.
data NginxTest = NginxTest
  { ntCommonName :: !Text
  , ntTargetPort :: !Int
  , ntTargetName :: !Text
  }
  deriving (Eq, Show)


data NginxPrep = NginxPrep
  { npUserID :: !UserID
  , npGroupID :: !GroupID
  , npVolumeRoot :: !FilePath
  }
  deriving (Eq, Show)


instance ToMustache NginxPrep where
  toMustache np =
    object
      [ "targetDir" ~> npVolumeRoot np
      ]


instance ToMustache NginxTest where
  toMustache nt =
    object
      [ "commonName" ~> ntCommonName nt
      , "targetPort" ~> ntTargetPort nt
      , "targetName" ~> ntTargetName nt
      ]


anNginxTest :: NginxTest
anNginxTest =
  NginxTest
    { ntCommonName = "localhost"
    , ntTargetPort = 80
    , ntTargetName = "http-bin-test-3"
    }


-- Prepare
-- expand the template with commonName to target-dir/nginx
-- create certs with commonName to target-dir/certs
-- used fixed cert basenames (certificate.pem and key.pem)
prepare' :: [(Text, HostIpAddress)] -> NginxTest -> IO NginxPrep
prepare' addrs nt = do
  case lookup (ntTargetName nt) addrs of
    Nothing -> error $ "could not find host " <> show (ntTargetName nt)
    Just _ -> do
      templateDir <- (</> "conf") <$> getDataDir
      compiled <- automaticCompile [templateDir] templateName
      case compiled of
        Left err -> error $ "the template did not compile:" ++ show err
        Right template -> do
          npVolumeRoot <- createWorkingDirs
          npUserID <- getEffectiveUserID
          npGroupID <- getEffectiveGroupID
          let (confDir, cpDir) = toConfCertsDirs npVolumeRoot
              cp =
                CertPaths
                  { cpKey = "key.pem"
                  , cpCert = "certificate.pem"
                  , cpDir
                  }
              np = NginxPrep {npUserID, npGroupID, npVolumeRoot}
          generateAndStore cp defaultConfig
          Text.writeFile (confDir </> "nginx.conf") $ substitute template (nt, np)
          pure np


templateName :: FilePath
templateName = "nginx-test.conf.mustache"


toRunCmd' :: NginxTest -> NginxPrep -> [Text]
toRunCmd' _ np =
  -- specify user ID and group ID to fix volume mount permissions
  -- mount volume /etc/tmp-proc/certs as target-dir/certs
  -- mount volume /etc/tmp-proc/nginx as target-dir/nginx
  let (confDir, certsDir) = toConfCertsDirs $ npVolumeRoot np
      confPath = confDir </> "nginx.conf"
      envArg name v =
        [ "-e"
        , name ++ "=" ++ show v
        ]
      volumeArg actualPath hostedPath =
        [ "-v"
        , actualPath ++ ":" ++ hostedPath
        ]
      confArg = volumeArg confPath $ dockerConf ++ ":ro"
      certsArg = volumeArg certsDir dockerCertsDir
      puidArg = envArg "PUID" $ npUserID np
      guidArg = envArg "GUID" $ npGroupID np
   in Text.pack <$> confArg ++ certsArg ++ puidArg ++ guidArg


createWorkingDirs :: IO FilePath
createWorkingDirs = do
  tmpDir <- getCanonicalTemporaryDirectory
  topDir <- createTempDirectory tmpDir "nginx-test"
  let (confDir, certsDir) = toConfCertsDirs topDir
  createDirectory confDir
  createDirectory certsDir
  pure topDir


toConfCertsDirs :: FilePath -> (FilePath, FilePath)
toConfCertsDirs topDir = (topDir </> "conf", topDir </> "certs")


dockerCertsDir :: FilePath
dockerCertsDir = "/etc/tmp-proc/certs"


dockerConf :: FilePath
dockerConf = "/data/conf/nginx.conf"


-- | Run Nginx as a temporary process.
instance Proc NginxTest where
  -- use this linuxserver.io nginx as it is setup to allow easy override of
  -- config
  type Image NginxTest = "lscr.io/linuxserver/nginx"
  type Name NginxTest = "nginx-test"
  uriOf = httpUri
  runArgs = []
  reset _ = pure ()
  ping = pingHttp


instance ToRunCmd NginxTest NginxPrep where
  toRunCmd = toRunCmd'


instance Preparer NginxTest NginxPrep where
  prepare = prepare'


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


-- | Make a uri access the http-bin server.
httpUri :: HostIpAddress -> SvcURI
httpUri ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


pingHttp :: ProcHandle a -> IO Pinged
pingHttp handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- httpGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


pingHttps :: ProcHandle a -> IO Pinged
pingHttps handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- httpsGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


-- | Determine the status from a Get.
httpGet :: ProcHandle a -> Text -> IO Int
httpGet handle urlPath = do
  let theUri = "http://" <> hAddr handle <> "/" <> Text.dropWhile (== '/') urlPath
  manager <- HC.newManager HC.defaultManagerSettings
  getReq <- HC.parseRequest $ Text.unpack theUri
  statusCode . HC.responseStatus <$> HC.httpLbs getReq manager


-- | Determine the status from a secure Get to host localhost.
httpsGet :: ProcHandle a -> Text -> IO Int
httpsGet handle urlPath = do
  -- _tlsSettings <- TLSSettings <$> _mkClientParams "localHost"
  let theUri = "https://" <> hAddr handle <> "/" <> Text.dropWhile (== '/') urlPath
      -- use TLS settings that disable hostname verification. What's not
      -- currently possible is to actually specify the hostname to use for SNI
      -- that differs from the connection IP address, that's not supported by
      -- http-client-tls
      tlsSettings = TLSSettingsSimple True False False
  manager <- HC.newTlsManagerWith $ HC.mkManagerSettings tlsSettings Nothing
  getReq <- HC.parseRequest $ Text.unpack theUri
  let withHost = getReq {HC.requestHeaders = [(hHost, "localhost")]}
  statusCode . HC.responseStatus <$> HC.httpLbs withHost manager


-- currently unused, since the server specified in ClientParams for SNI is
-- overridden by Connection, which resets it to the connection hostname
_mkClientParams :: HostName -> IO ClientParams
_mkClientParams server = do
  cs <- getSystemCertificateStore
  pure $
    (defaultParamsClient server "")
      { clientSupported =
          def
            { supportedCiphers = ciphersuite_default
            }
      , clientShared = def {sharedCAStore = cs}
      , clientUseServerNameIndication = True
      }


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


typeLevelCheck4 :: IO (HandlesOf '[HttpBinTest3, NginxTest, HttpBinTest])
typeLevelCheck4 = do
  allHandles <- setupHandles
  pure $ manyNamed @'["http-bin-test-3", "nginx-test", "http-bin-test"] Proxy allHandles
