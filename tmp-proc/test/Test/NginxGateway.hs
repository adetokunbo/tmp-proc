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

Test helper types and functions for testing with an nginx reverse-proxy container.
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
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.Connection.CPP (noCheckSettings)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as HC
import Network.HTTP.Types.Header (hHost)
import Network.HTTP.Types.Status (statusCode)
import Paths_tmp_proc (getDataDir)
import System.Directory (createDirectory, removeDirectoryRecursive)
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
  , addrPort
  , toPinged
  )
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
  uriOf = httpsUri
  runArgs = []
  reset _ = pure ()
  ping = pingHttps
  uriOfRootlessWith addr pairs =
    case lookup 443 pairs of
      Just hp -> "https://" <> addrPort addr hp <> "/"
      Nothing -> httpsUri addr


instance ToRunCmd NginxGateway NginxPrep where
  toRunCmd = toRunCmd'


instance Preparer NginxGateway NginxPrep where
  prepare = prepare'
  tidy = tidy'


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


tidy' :: NginxGateway -> NginxPrep -> IO ()
tidy' _ np = removeDirectoryRecursive $ npVolumeRoot np


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


-- | Make a uri to access the nginx gateway server.
httpsUri :: HostIpAddress -> SvcURI
httpsUri ip = "https://" <> C8.pack (Text.unpack ip) <> "/"


pingHttps :: ProcHandle a -> IO Pinged
pingHttps handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- httpsGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


-- | Determine the status from a secure GET, using the handle's URI as the base.
httpsGet :: ProcHandle a -> Text -> IO Int
httpsGet handle urlPath = do
  let theUri = C8.unpack (hUri handle) <> Text.unpack (Text.dropWhile (== '/') urlPath)
  -- TLS settings disable hostname verification: the cert is for "localhost"
  -- but the connection address may be a container IP or 127.0.0.1:<port>.
  manager <- HC.newTlsManagerWith $ HC.mkManagerSettings noCheckSettings Nothing
  getReq <- HC.parseRequest theUri
  let withHost = getReq {HC.requestHeaders = [(hHost, "localhost")]}
  statusCode . HC.responseStatus <$> HC.httpLbs withHost manager

-- currently unused, since the server specified in ClientParams for SNI is
-- overridden by Connection, which resets it to the connection hostname
-- _mkClientParams :: HostName -> IO ClientParams
-- _mkClientParams server = do
--   cs <- getSystemCertificateStore
--   pure $
--     (defaultParamsClient server "")
--       { clientSupported =
--           def
--             { supportedCiphers = ciphersuite_default
--             }
--       , clientShared = def {sharedCAStore = cs}
--       , clientUseServerNameIndication = True
--       }
