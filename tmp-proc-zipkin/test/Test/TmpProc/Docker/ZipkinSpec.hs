{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Test.TmpProc.Docker.ZipkinSpec where

import           Test.Hspec

import           Data.Proxy                     (Proxy (..))
import qualified Data.Text                      as Text

import           System.TmpProc.Docker
import           System.TmpProc.Docker.Zipkin


-- | Used as pending alternative when docker is unavailable.
noDockerSpec :: String -> Spec
noDockerSpec desc = describe desc $
  it "cannot run as docker is unavailable" pending


spec :: Bool -> Spec
spec noDocker = do
  let desc = "Tmp.Proc:Zipkin:" ++ Text.unpack (nameOf testProc)
  if noDocker then noDockerSpec desc else checkZipkin desc


checkZipkin :: String -> Spec
checkZipkin desc =  beforeAll setupHandles $ afterAll terminateAll $ do
  describe desc $ do
    context "when using the Proc from the HList by its 'Name'" $ do

      context "ixPing" $ do

        it "should succeed" $ \hs
          -> ixPing @"a-zipkin-server" Proxy hs `shouldReturn`()



setupHandles :: IO (HList '[ProcHandle TmpZipkin])
setupHandles = do
  handles <- startupAll $ testProc `HCons` HNil
  pure handles


testProc :: TmpZipkin
testProc = TmpZipkin
