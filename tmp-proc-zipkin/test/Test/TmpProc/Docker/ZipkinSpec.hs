{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Test.TmpProc.Docker.ZipkinSpec where

import           Test.Hspec
import           Test.Hspec.TmpProc

import           Data.Proxy                     (Proxy (..))
import qualified Data.Text                      as Text

import           System.TmpProc.Docker.Zipkin

spec :: Spec
spec = tdescribe desc $ do
  beforeAll setupHandles $ afterAll terminateAll $ do
    context "when using the Proc from the HList by its 'Name'" $ do

      context "ixPing" $ do

        it "should succeed" $ \hs
          -> ixPing @"a-zipkin-server" Proxy hs `shouldReturn`()


setupHandles :: IO (HList '[ProcHandle TmpZipkin])
setupHandles = startupAll $ testProc `HCons` HNil


testProc :: TmpZipkin
testProc = TmpZipkin


desc :: String
desc = "Tmp.Proc:Zipkin:" ++ Text.unpack (nameOf testProc)
