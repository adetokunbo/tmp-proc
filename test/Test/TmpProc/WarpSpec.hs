{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.TmpProc.WarpSpec where

import           Test.Hspec

import           Data.Either                (isRight)
import qualified Data.Text                  as Text
import           Network.Wai                (Application)
import           System.Docker.TmpProc
import           System.Docker.TmpProc.Warp (handle, runServer, runTLSServer,
                                             serverPort, shutdown,
                                             testWithApplication,
                                             testWithTLSApplication)

import           Test.SimpleServer          (defaultTLSSettings, statusOfGet,
                                             statusOfGet')
import           Test.TmpProc.Hspec         (noDockerSpec)


mkSpec :: Bool -> TmpProc -> (Handle -> IO Application) -> Spec
mkSpec noDocker tp mkApp = do
  let name = procImageName tp
      desc = "ServerHandle: a server using " ++ (Text.unpack name)

  if noDocker then noDockerSpec desc else do
    singleSharedServerSpec tp mkApp
    singleSharedTLSServerSpec tp mkApp
    serverPerTestSpec tp mkApp
    tlsServerPerTestSpec tp mkApp


singleSharedServerSpec :: TmpProc -> (Handle -> IO Application) -> Spec
singleSharedServerSpec tp mkApp = do
  let name = procImageName tp
      desc = "ServerHandle: (one server for all tests) using " ++ (Text.unpack name)

  beforeAll (runServer [tp] mkApp) $ afterAll shutdown $ do
    describe desc $ do
      context "handle" $ do
        it "should obtain the process' URI" $ \sh -> do
          (isRight $ procURI name $ handle sh) `shouldBe` True

        it "should reset the process ok" $ \sh -> do
          reset name (handle sh) `shouldReturn` ()

      context "serverPort" $ do
        it "should allow successful invocation of the test api" $ \sh -> do
          statusOfGet (serverPort sh) "test" `shouldReturn` 200


singleSharedTLSServerSpec :: TmpProc -> (Handle -> IO Application) -> Spec
singleSharedTLSServerSpec tp mkApp = do
  let name = procImageName tp
      desc = "TLS ServerHandle: (one server for all tests) using " ++ (Text.unpack name)

  beforeAll (runTLSServer defaultTLSSettings [tp] mkApp) $ afterAll shutdown $ do
    describe desc $ do
      context "handle" $ do
        it "should obtain the process' URI" $ \sh -> do
          (isRight $ procURI name $ handle sh) `shouldBe` True

        it "should reset the process ok" $ \sh -> do
          reset name (handle sh) `shouldReturn` ()

      context "port" $ do
        it "should reset the process ok" $ \sh -> do
          statusOfGet' (serverPort sh) "test" `shouldReturn` 200


serverPerTestSpec :: TmpProc -> (Handle -> IO Application) -> Spec
serverPerTestSpec tp mkApp = do
  let name = procImageName tp
      desc = "CPS style: (one server per-test) using " ++ (Text.unpack name)

  around (testWithApplication [tp] mkApp) $ do
    describe desc $ do
      context "with the handle" $ do
        it "should obtain the process' URI" $ \(h, _) -> do
          (isRight $ procURI name h) `shouldBe` True

        it "should reset the process ok" $ \(h, _) -> do
          reset name h `shouldReturn` ()

      context "with the port" $ do
        it "should reset the process via the api" $ \(_, p) -> do
          statusOfGet p "test" `shouldReturn` 200


tlsServerPerTestSpec :: TmpProc -> (Handle -> IO Application) -> Spec
tlsServerPerTestSpec tp mkApp = do
  let name = procImageName tp
      desc = "CPS style: (one TLS server per-test) using " ++ (Text.unpack name)

  around (testWithTLSApplication defaultTLSSettings [tp] mkApp) $ do
    describe desc $ do
      context "with the handle" $ do
        it "should obtain the process' URI" $ \(h, _) -> do
          (isRight $ procURI name h) `shouldBe` True

        it "should reset the process ok" $ \(h, _) -> do
          reset name h `shouldReturn` ()

      context "with the port" $ do
        it "should reset the process via the api" $ \(_, p) -> do
          statusOfGet' p "test" `shouldReturn` 200
