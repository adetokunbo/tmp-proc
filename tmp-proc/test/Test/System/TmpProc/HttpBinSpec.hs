{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.System.TmpProc.HttpBinSpec where

import           Test.Hspec

import           Data.Proxy         (Proxy (Proxy))
import qualified Data.Text          as Text

import           System.TmpProc     (Pinged (..), ixPing, ixReset, nameOf,
                                     terminateAll)
import           Test.Hspec.TmpProc (tdescribe)
import           Test.HttpBin


spec :: Spec
spec = tdescribe ("Tmp.Proc: " ++ Text.unpack (nameOf HttpBinTest)) $ do
  beforeAll setupHandles $ afterAll terminateAll $ do
    context "When accessing the services in the list of test tmp procs" $ do

      context "ixPing" $ do

        it "should succeed when accessing a Proc by name" $ \hs
          -> ixPing @"http-bin-test" Proxy hs `shouldReturn` OK

        it "should succeed when accessing a Proc by type" $ \hs
          -> ixPing @HttpBinTest Proxy hs `shouldReturn` OK

      context "ixReset" $ do

        it "should succeed when accessing a Proc by name" $ \hs
          -> ixReset @"http-bin-test" Proxy hs `shouldReturn`()

        it "should succeed when accessing a different Proc by name" $ \hs
          -> ixReset @"http-bin-test-3" Proxy hs `shouldReturn`()

        it "should succeed when accessing a Proc by type" $ \hs
          -> ixReset @HttpBinTest Proxy hs `shouldReturn`()

        it "should succeed when accessing a different Proc by type" $ \hs
          -> ixReset @HttpBinTest2 Proxy hs `shouldReturn`()
