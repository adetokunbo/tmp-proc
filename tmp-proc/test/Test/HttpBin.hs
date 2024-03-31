{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.HttpBin where

import Control.Concurrent (getChanContents)
import qualified Data.ByteString.Char8 as C8
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status (statusCode)
import Paths_tmp_proc
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
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
import Text.Mustache
  ( ToMustache (..)
  , automaticCompile
  , object
  , substitute
  , (~>)
  )


setupHandles :: IO (HandlesOf '[HttpBinTest, NginxTest, HttpBinTest3])
setupHandles = startupAll $ HttpBinTest &: anNginxTest &:& HttpBinTest3


-- | A data type representing a connection to an Nginx server.
data NginxTest = NginxTest
  { ntCommonName :: !Text
  , ntTargetDir :: !FilePath
  , ntTargetPort :: !Int
  , ntTargetName :: !Text
  }
  deriving (Eq, Show)


instance ToMustache NginxTest where
  toMustache nt =
    object
      [ "commonName" ~> ntCommonName nt
      , "targetDir" ~> ntTargetDir nt
      , "targetPort" ~> ntTargetPort nt
      , "targetName" ~> ntTargetName nt
      ]


anNginxTest :: NginxTest
anNginxTest =
  NginxTest
    { ntCommonName = "localhost"
    , ntTargetDir = "/tmp"
    , ntTargetPort = 80
    , ntTargetName = "http-bin-test-3"
    }


-- Prepare
-- expand the template with commonName to target-dir/nginx
-- create certs with commonName to target-dir/certs
-- used fixed cert basenames (certificate.pem and key.pem)
prepare' :: [(Text, HostIpAddress)] -> NginxTest -> IO NginxTest
prepare' addrs nt = do
  confDir <- (</> "conf") <$> getDataDir
  compiled <- automaticCompile [confDir] templateName
  let templateName = "nginx-test.conf.mustache"
  case compiled of
    Left err -> error $ "the template did not compile:" ++ show err
    Right template -> do
      (workingDir, _confDir, certsDir) <- createTmpNginxDirs
      let nt' = nt {ntTargetDir = workingDir}
      Text.writeFile (confDir </> "nginx.conf") $ substitute template nt'
      Text.writeFile (certsDir </> "nginx.conf") $ substitute template nt'
      print $ "the template compiled to:" ++ show confDir
      print addrs
      pure nt'


createTmpNginxDirs :: IO (FilePath, FilePath, FilePath)
createTmpNginxDirs = do
  tmpDir <- getCanonicalTemporaryDirectory
  workingDir <- createTempDirectory "nginx-test" tmpDir
  let (confDir, certsDir) = certDirsOf workingDir
  createDirectory confDir
  createDirectory certsDir
  pure (workingDir, confDir, certsDir)


certDirsOf :: FilePath -> (FilePath, FilePath)
certDirsOf workingDir = (workingDir </> "conf", workingDir </> "certs")


--
-- ToRunCmd
-- mount volume /etc/tmp-proc/certs as target-dir/certs
-- mount volume /etc/tmp-proc/nginx as target-dir/nginx

-- | Run Nginx as a temporary process.
instance Proc NginxTest where
  type Image NginxTest = "nginx:1.25.1"
  type Name NginxTest = "nginx-test"
  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


instance ToRunCmd NginxTest where
  toRunCmd _ = []


instance Preparer NginxTest where
  prepare = prepare'


-- | A data type representing a connection to a HttpBin server.
data HttpBinTest = HttpBinTest


-- | Run HttpBin as temporary process.
instance Proc HttpBinTest where
  type Image HttpBinTest = "kennethreitz/httpbin"
  type Name HttpBinTest = "http-bin-test"
  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


{- | Another data type representing a connection to a HttpBin server.

Used in this module to allow mulitple types in test lists, to improve the
chances of detecting type-related compilationr errors.
-}
data HttpBinTest2 = HttpBinTest2


-- | Run HttpBin as temporary process.
instance Proc HttpBinTest2 where
  type Image HttpBinTest2 = "kennethreitz/httpbin"
  type Name HttpBinTest2 = "http-bin-test-2"
  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


{- | Yet another data type representing a connection to a HttpBin server.

Used in this module to allow mulitple types in test lists, to improve the
chances of detecting type-related compilationr errors.
-}
data HttpBinTest3 = HttpBinTest3


-- | Run HttpBin as temporary process.
instance Proc HttpBinTest3 where
  type Image HttpBinTest3 = "kennethreitz/httpbin"
  type Name HttpBinTest3 = "http-bin-test-3"
  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


-- | Make a uri access the http-bin server.
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


ping' :: ProcHandle a -> IO Pinged
ping' handle = toPinged @HC.HttpException Proxy $ do
  gotStatus <- handleGet handle "/status/200"
  if gotStatus == 200 then pure OK else pure NotOK


-- | Determine the status from a Get on localhost.
handleGet :: ProcHandle a -> Text -> IO Int
handleGet handle urlPath = do
  let theUri = "http://" <> hAddr handle <> "/" <> Text.dropWhile (== '/') urlPath
  manager <- HC.newManager HC.defaultManagerSettings
  getReq <- HC.parseRequest $ Text.unpack theUri
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


typeLevelCheck4 :: IO (HandlesOf '[HttpBinTest3, NginxTest, HttpBinTest])
typeLevelCheck4 = do
  allHandles <- setupHandles
  pure $ manyNamed @'["http-bin-test-3", "nginx-test", "http-bin-test"] Proxy allHandles
