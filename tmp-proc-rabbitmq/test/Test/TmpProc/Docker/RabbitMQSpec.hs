{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Test.TmpProc.Docker.RabbitMQSpec where

import           Test.Hspec

import           Data.Proxy                     (Proxy (..))
import qualified Data.Text                      as Text

import           System.TmpProc.Docker
import           System.TmpProc.Docker.RabbitMQ


-- | Used as pending alternative when docker is unavailable.
noDockerSpec :: String -> Spec
noDockerSpec desc = describe desc $
  it "cannot run as docker is unavailable" pending


spec :: Bool -> Spec
spec noDocker = do
  let desc = "Tmp.Proc:RabbitMQ:" ++ Text.unpack (nameOf testProc)
  if noDocker then noDockerSpec desc else checkRabbitMQ desc


checkRabbitMQ :: String -> Spec
checkRabbitMQ desc =  beforeAll setupHandles $ afterAll terminateAll $ do
  describe desc $ do
    context "when using the Proc from the HList by its 'Name'" $ do

      context "ixPing" $ do

        it "should succeed" $ \hs
          -> ixPing @"a-rabbitmq-server" Proxy hs `shouldReturn`()

      context "ixReset" $ do

        it "should succeed" $ \hs
          -> ixReset @"a-rabbitmq-server" Proxy hs `shouldReturn`()


setupHandles :: IO (HList '[ProcHandle TmpRabbitMQ])
setupHandles = do
  handles <- startupAll $ testProc `HCons` HNil
  pure handles


testProc :: TmpRabbitMQ
testProc = TmpRabbitMQ
