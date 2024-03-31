{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import Test.Hspec
import Test.Hspec.TmpProc (tdescribe)
import Test.HttpBin
import Test.SimpleServer (statusOfGet)


spec :: Spec
spec = tdescribe "Tmp.Proc: Warp server with Tmp.Proc dependency" $ do
  beforeAllSpec >> aroundSpec


testProcs :: HList '[HttpBinTest]
testProcs = only HttpBinTest


testApp :: HandlesOf '[HttpBinTest] -> IO Application
testApp hs =
  mkTestApp'
    (pingOrFail $ handleOf @"http-bin-test" Proxy hs)
    (pingOrFail $ handleOf @"http-bin-test" Proxy hs)


setupBeforeAll :: IO (ServerHandle '[HttpBinTest])
setupBeforeAll = runServer testProcs testApp


-- setupBeforeAllTls :: IO (ServerHandle '[HttpBinTest])
-- setupBeforeAllTls = do
--   tls <- defaultTLSSettings
--   runTLSServer tls testProcs testApp

suffixAround, suffixBeforeAll, prefixHttp, prefixHttps :: String
suffixAround = " when the server is restarted for each test"
suffixBeforeAll = " when the server starts beforeAll tests"
prefixHttp = "Warp+HTTP:"
prefixHttps = "Warp+HTTPS:"


beforeAllSpec :: Spec
beforeAllSpec = do
  checkBeforeAll prefixHttp setupBeforeAll statusOfGet


-- checkBeforeAll prefixHttps setupBeforeAllTls statusOfGet'

checkBeforeAll ::
  String ->
  IO (ServerHandle '[HttpBinTest]) ->
  (Int -> Text -> IO Int) ->
  Spec
checkBeforeAll descPrefix setup getter = beforeAll setup $ afterAll shutdown $ do
  describe (descPrefix ++ suffixBeforeAll) $ do
    it "should ping the proc handle" $ \sh ->
      ixPing @"http-bin-test" Proxy (handles sh) `shouldReturn` OK

    it "should invoke the warp server via its port" $ \sh ->
      getter (serverPort sh) "test" `shouldReturn` 200


setupAround :: ((HandlesOf '[HttpBinTest], Int) -> IO a) -> IO a
setupAround = testWithApplication testProcs testApp


-- setupAroundTls :: ((HandlesOf '[HttpBinTest], Int) -> IO a) -> IO a
-- setupAroundTls cont = do
--   tls <- defaultTLSSettings
--   testWithTLSApplication tls testProcs testApp cont

aroundSpec :: Spec
aroundSpec = do
  checkEachTest prefixHttp setupAround statusOfGet


-- checkEachTest prefixHttps setupAroundTls statusOfGet'

checkEachTest ::
  String ->
  (ActionWith (HandlesOf '[HttpBinTest], Int) -> IO ()) ->
  (Int -> Text -> IO Int) ->
  Spec
checkEachTest descPrefix setup getter = around setup $ do
  describe (descPrefix ++ suffixAround) $ do
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
