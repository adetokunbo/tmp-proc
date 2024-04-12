{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.System.TmpProc.WarpSpec where

import Control.Exception (catch)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (status200, status400)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.WarpTLS (tlsSettings)
import qualified Network.Wai.Handler.WarpTLS as Warp
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.TmpProc.Docker
  ( HList (..)
  , HandlesOf
  , Pinged (..)
  , ProcHandle
  , handleOf
  , ixPing
  , only
  )
import System.TmpProc.Warp
  ( ServerHandle
  , handles
  , runServer
  , runTLSServer
  , serverPort
  , shutdown
  , testWithApplication
  , testWithTLSApplication
  )
import Test.Certs.Temp (CertPaths (..), certificatePath, defaultConfig, generateAndStore, keyPath, withCertPathsInTmp')
import Test.Hspec
import Test.Hspec.TmpProc (tdescribe)
import Test.HttpBin
import Test.NginxGateway (NginxGateway (..))
import Test.SimpleServer (statusOfGet, statusOfGet')


spec :: Spec
spec = tdescribe "Tmp.Proc: Warp server with Tmp.Proc dependency" $ do
  checkBeforeAll prefixHttp setupBeforeAll statusOfGet
  checkBeforeAll prefixHttps setupBeforeAllTls statusOfGet'
  httpSpec prefixHttp setupAround statusOfGet
  httpSpec prefixHttps setupAroundTls statusOfGet'


type TmpProcs = '[HttpBinTest]


testProcs :: HList TmpProcs
testProcs = only HttpBinTest


theGateway :: NginxGateway
theGateway =
  NginxGateway
    { ngCommonName = "localhost"
    , ngTargetPort = 80
    , ngTargetName = "http-bin-test"
    }


testApp :: HandlesOf TmpProcs -> IO Application
testApp hs =
  mkTestApp'
    (pingOrFail $ handleOf @"http-bin-test" Proxy hs)
    (pingOrFail $ handleOf @"http-bin-test" Proxy hs)


setupBeforeAll :: IO (ServerHandle TmpProcs)
setupBeforeAll = runServer testProcs testApp


setupBeforeAllTls :: IO (ServerHandle TmpProcs)
setupBeforeAllTls = do
  cp <- genCertPaths
  let tls = tlsSettings (certificatePath cp) (keyPath cp)
  runTLSServer tls testProcs testApp


genCertPaths :: IO CertPaths
genCertPaths = do
  tmpDir <- getCanonicalTemporaryDirectory
  cpDir <- createTempDirectory tmpDir "tmp-proc-warp-spec"
  let cp =
        CertPaths
          { cpKey = "key.pem"
          , cpCert = "certificate.pem"
          , cpDir
          }
  generateAndStore cp defaultConfig
  pure cp


suffixAround, suffixBeforeAll, prefixHttp, prefixHttps :: String
suffixAround = " when the server is restarted for each test"
suffixBeforeAll = " when the server starts beforeAll tests"
prefixHttp = "Warp+HTTP:"
prefixHttps = "Warp+HTTPS:"


checkBeforeAll ::
  String ->
  IO (ServerHandle TmpProcs) ->
  (Int -> Text -> IO Int) ->
  Spec
checkBeforeAll descPrefix setup getter = beforeAll setup $ afterAll shutdown $ do
  describe (descPrefix ++ suffixBeforeAll) $ do
    it "should ping the proc handle" $ \sh ->
      ixPing @"http-bin-test" Proxy (handles sh) `shouldReturn` OK

    it "should invoke the warp server via its port" $ \sh ->
      getter (serverPort sh) "test" `shouldReturn` 200


setupAround :: ((HandlesOf TmpProcs, Int) -> IO a) -> IO a
setupAround = testWithApplication testProcs testApp


setupAroundTls :: ((HandlesOf TmpProcs, Int) -> IO a) -> IO a
setupAroundTls cont = withCertPathsInTmp' $ \cp -> do
  let tls = Warp.tlsSettings (certificatePath cp) (keyPath cp)
  testWithTLSApplication tls testProcs testApp cont


httpSpec ::
  String ->
  (ActionWith (HandlesOf TmpProcs, Int) -> IO ()) ->
  (Int -> Text -> IO Int) ->
  Spec
httpSpec prefix setup getter = around setup $ do
  describe (prefix ++ suffixAround) $ do
    it "should ping the proc handle" $ \(h, _) ->
      ixPing @"http-bin-test" Proxy h `shouldReturn` OK

    it "should invoke the warp server via its port" $ \(_, p) ->
      getter p "test" `shouldReturn` 200


{- | A WAI app that triggers an action on a TmpProc dependency on /test, and
responds to health checks on /health.
-}
mkTestApp' ::
  IO () ->
  IO () ->
  IO Application
mkTestApp' onStart onTest = onStart >> pure app
  where
    app rq respond
      | isHealthReq rq = respond $ responseLBS status200 [] "ok"
    app rq respond
      | isTestReq rq = onTest >> respond (responseLBS status200 [] "ok")
    app _ respond = respond $ responseLBS status400 [] "Incorrect request"

    isHealthReq = isReqPathsEq ["health"]
    isTestReq = isReqPathsEq ["test"]
    isReqPathsEq x rq = x == pathInfo rq


pingOrFail :: ProcHandle a -> IO ()
pingOrFail handle = do
  let catchHttp x =
        x
          `catch` ( \(_ :: HC.HttpException) ->
                      fail "tmp proc:httpbin:ping failed"
                  )
  catchHttp $ do
    gotStatus <- httpGet handle "/status/200"
    if gotStatus == 200
      then pure ()
      else fail "tmp proc:httpbin:incorrect ping status"
