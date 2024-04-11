{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Test.NginxGateway
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Test.NginxGateway
  ( -- * data types
    NginxGateway (..)
  , NginxPrep (..)

    -- * ping via https
  , pingHttps
  )
where

import qualified Data.ByteString.Char8 as C8
import Data.Data (Proxy (..))
import Data.Default (Default (..))
import Data.List (find)
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
import Paths_tmp_proc (getDataDir)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Posix.ByteString (getEffectiveGroupID, getEffectiveUserID)
import System.Posix.Types (GroupID, UserID)
import System.TmpProc
  ( HostIpAddress
  , Pinged (..)
  , Preparer (..)
  , Proc (..)
  , ProcHandle (..)
  , SlimHandle (..)
  , SvcURI
  , ToRunCmd (..)
  , toPinged
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


-- | Run Nginx as a temporary process.
instance Proc NginxGateway where
  -- use this linuxserver.io nginx as it is setup to allow easy override of
  -- config
  type Image NginxGateway = "lscr.io/linuxserver/nginx"
  type Name NginxGateway = "nginx-test"
  uriOf = httpUri
  runArgs = []
  reset _ = pure ()
  ping = pingHttps


instance ToRunCmd NginxGateway NginxPrep where
  toRunCmd = toRunCmd'


instance Preparer NginxGateway NginxPrep where
  prepare = prepare'


{- | Configures launch of a container thats uses nginx as a gateway (a.k.a
reverse proxy).
-}
data NginxGateway = NginxGateway
  { ngCommonName :: !Text
  , ngTargetPort :: !Int
  , ngTargetName :: !Text
  }
  deriving (Eq, Show)


instance ToMustache NginxGateway where
  toMustache nt =
    object
      [ "commonName" ~> ngCommonName nt
      , "targetPort" ~> ngTargetPort nt
      , "targetName" ~> ngTargetName nt
      ]


-- | Values obtained while preparing to launch the nginx container
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


templateName :: FilePath
templateName = "nginx-test.conf.mustache"


toConfCertsDirs :: FilePath -> (FilePath, FilePath)
toConfCertsDirs topDir = (topDir </> "conf", topDir </> "certs")


dockerCertsDir :: FilePath
dockerCertsDir = "/etc/tmp-proc/certs"


dockerConf :: FilePath
dockerConf = "/data/conf/nginx.conf"


createWorkingDirs :: IO FilePath
createWorkingDirs = do
  tmpDir <- getCanonicalTemporaryDirectory
  topDir <- createTempDirectory tmpDir "nginx-test"
  let (confDir, certsDir) = toConfCertsDirs topDir
  createDirectory confDir
  createDirectory certsDir
  pure topDir


toRunCmd' :: NginxGateway -> NginxPrep -> [Text]
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


-- Prepare
-- expand the template with commonName to target-dir/nginx
-- create certs with commonName to target-dir/certs
-- used fixed cert basenames (certificate.pem and key.pem)
prepare' :: [SlimHandle] -> NginxGateway -> IO NginxPrep
prepare' views nt@NginxGateway {ngTargetName = name} = do
  case find ((== name) . shName) views of
    Nothing -> error $ "could not find host " <> show name
    Just _ -> do
      templateDir <- (</> "templates") <$> getDataDir
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


-- | Make a uri access the http-bin server.
httpUri :: HostIpAddress -> SvcURI
httpUri ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


pingHttps :: ProcHandle a -> IO Pinged
pingHttps handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- httpsGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


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
