{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.System.TmpProc.HttpBinSpec where

import           Test.Hspec

import           Data.Proxy                (Proxy (Proxy))
import qualified Data.Text                 as Text

import           System.TmpProc.Docker     (ixPing, ixReset, nameOf,
                                            terminateAll)
import           Test.HttpBin
import           Test.System.TmpProc.Hspec (noDockerSpec)


spec :: Bool -> Spec
spec noDocker = do
  let desc = "Tmp.Proc: " ++ Text.unpack (nameOf HttpBinTest)
  if noDocker then noDockerSpec desc else checkHttpBin desc


checkHttpBin :: String -> Spec
checkHttpBin desc =  beforeAll setupHandles $ afterAll terminateAll $ do
  describe desc $ do
    context "when using the Proc from the HList by Name" $ do

      context "ixPing" $ do

        it "should succeed" $ \hs
          -> ixPing @"http-bin-test" Proxy hs `shouldReturn`()

      context "ixReset" $ do

        it "should succeed" $ \hs
          -> ixReset @"http-bin-test" Proxy hs `shouldReturn`()

      context "ixReset" $ do

        it "should succeed" $ \hs
          -> ixReset @"http-bin-test-3" Proxy hs `shouldReturn`()
