{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.System.TmpProc.WarpSpec where

import           Test.Hspec

import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import           Network.HTTP.Types        (status200, status400)
import           Network.Wai               (Application, pathInfo, responseLBS)

import           System.TmpProc.Docker     (HList (..), ProcHandle, ixPing)
import           System.TmpProc.Warp       (ServerHandle, handles, runServer,
                                            runTLSServer, serverPort, shutdown,
                                            testWithApplication,
                                            testWithTLSApplication)
import           Test.HttpBin
import           Test.SimpleServer         (defaultTLSSettings, statusOfGet,
                                            statusOfGet')
import           Test.System.TmpProc.Hspec (noDockerSpec)

spec :: Bool -> Spec
spec noDocker = do
  let desc = "Tmp.Proc: run a warp server with a Tmp.Proc dependency"
  if noDocker then noDockerSpec desc else beforeAllSpec >> aroundSpec


testProcs :: HList '[HttpBinTest]
testProcs = HttpBinTest `HCons` HNil


testApp :: HList '[ProcHandle HttpBinTest] -> IO Application
testApp hs = mkTestApp' (ixPing @"http-bin-test" Proxy hs) (ixPing @"http-bin-test" Proxy hs)


setupBeforeAll :: IO (ServerHandle '[ProcHandle HttpBinTest])
setupBeforeAll = runServer testProcs testApp

setupBeforeAllTls :: IO (ServerHandle '[ProcHandle HttpBinTest])
setupBeforeAllTls = runTLSServer defaultTLSSettings testProcs testApp


suffixAround, suffixBeforeAll, prefixHttp, prefixHttps :: String
suffixAround = " when the server is restarted for each test"
suffixBeforeAll = " when the server starts beforeAll tests"
prefixHttp = "System.TmpProc.Warp:HTTP:"
prefixHttps = "System.TmpProc.Warp:HTTPS:"


beforeAllSpec :: Spec
beforeAllSpec = do
  checkBeforeAll prefixHttp setupBeforeAll statusOfGet
  checkBeforeAll prefixHttps setupBeforeAllTls statusOfGet'


checkBeforeAll
  :: String
  -> IO (ServerHandle '[ProcHandle HttpBinTest])
  -> (Int -> Text -> IO Int) -> Spec
checkBeforeAll descPrefix setup getter =  beforeAll setup $ afterAll shutdown $ do
  describe (descPrefix ++ suffixBeforeAll) $ do

    context "handle" $ do
      it "should ping the process ok" $ \sh ->
        ixPing @"http-bin-test" Proxy (handles sh) `shouldReturn` ()

    context "serverPort" $ do
      it "should invoke the server via the warp ok" $ \sh ->
        getter (serverPort sh) "test" `shouldReturn` 200


setupAround :: ((HList '[ProcHandle HttpBinTest], Int) -> IO a) -> IO a
setupAround = testWithApplication testProcs testApp


setupAroundTls :: ((HList '[ProcHandle HttpBinTest], Int) -> IO a) -> IO a
setupAroundTls = testWithTLSApplication defaultTLSSettings testProcs testApp


aroundSpec :: Spec
aroundSpec = do
  checkEachTest prefixHttp setupAround statusOfGet
  checkEachTest prefixHttps setupAroundTls statusOfGet'


checkEachTest
  :: String
  -> (ActionWith (HList '[ProcHandle HttpBinTest], Int) -> IO ())
  -> (Int -> Text -> IO Int)
  -> Spec
checkEachTest descPrefix setup getter = around setup $ do
  describe (descPrefix ++ suffixAround) $ do

    context "handle" $ do
      it "should ping the process ok" $ \(h, _) ->
        ixPing @"http-bin-test" Proxy h `shouldReturn` ()

    context "serverPort" $ do
      it "should invoke the server via the warp ok" $ \(_, p) ->
        getter p "test" `shouldReturn` 200


-- | A WAI app that triggers an action on a TmpProc dependency on /test, and
-- responds to health checks on /health.
mkTestApp'
  :: IO ()
  -> IO ()
  -> IO Application
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
