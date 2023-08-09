{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TmpProc.Docker.ZipkinSpec where

import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import System.TmpProc.Docker.Zipkin
import Test.Hspec
import Test.Hspec.TmpProc


spec :: Spec
spec = tdescribe desc $ do
  beforeAll (startupAll aProc) $ afterAll terminateAll $ do
    context "when using the Proc from the HList by its 'Name'" $ do
      context "ixPing" $ do
        it "should succeed" $ \hs ->
          ixPing @"a-zipkin-server" Proxy hs `shouldReturn` OK


desc :: String
desc = "Tmp.Proc:Zipkin:" ++ Text.unpack (nameOf $ hHead aProc)
