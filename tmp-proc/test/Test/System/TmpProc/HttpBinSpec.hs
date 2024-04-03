{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.System.TmpProc.HttpBinSpec where

import Data.Proxy (Proxy (Proxy))
import System.TmpProc
  ( Pinged (..)
  , ixPing
  , ixReset
  , terminateAll
  )
import System.TmpProc.Docker (ProcHandle, hOf)
import Test.Hspec
import Test.Hspec.TmpProc (tdescribe)
import Test.HttpBin


spec :: Spec
spec = tdescribe "Tmp.Proc: ping and reset" $ do
  beforeAll setupHandles $ afterAll terminateAll $ do
    context "When accessing the services in the list of test tmp procs" $ do
      context "ixPing" $ do
        it "should succeed when accessing the nginx proc by name" $ \hs ->
          pingHttps (hOf @(ProcHandle NginxTest) Proxy hs) `shouldReturn` OK

        it "should succeed when accessing the http-bin proc by name" $ \hs ->
          ixPing @"http-bin-test" Proxy hs `shouldReturn` OK

        it "should succeed when accessing a Proc by type" $ \hs ->
          ixPing @HttpBinTest Proxy hs `shouldReturn` OK

      context "ixReset" $ do
        it "should succeed when accessing a Proc by name" $ \hs ->
          ixReset @"http-bin-test" Proxy hs `shouldReturn` ()

        it "should succeed when accessing a different Proc by name" $ \hs ->
          ixReset @"http-bin-test-3" Proxy hs `shouldReturn` ()

        it "should succeed when accessing a http-bin proc by type" $ \hs ->
          ixReset @HttpBinTest Proxy hs `shouldReturn` ()

        it "should succeed when accessing the nginx proc by type" $ \hs ->
          ixReset @NginxTest Proxy hs `shouldReturn` ()
