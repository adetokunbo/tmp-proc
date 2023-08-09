{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.TmpProc.Docker.RabbitMQSpec where

import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import System.TmpProc.Docker.RabbitMQ
import Test.Hspec
import Test.Hspec.TmpProc


spec :: Spec
spec = tdescribe desc $ do
  beforeAll (startupAll aProc) $ afterAll terminateAll $ do
    context "when using the Proc from the HList by its 'Name'" $ do
      context "ixPing" $ do
        it "should succeed" $ \hs ->
          ixPing @"a-rabbitmq-server" Proxy hs `shouldReturn` OK

      context "ixReset" $ do
        it "should succeed" $ \hs ->
          ixReset @"a-rabbitmq-server" Proxy hs `shouldReturn` ()


desc :: String
desc = "Tmp.Proc:RabbitMQ:" ++ Text.unpack (nameOf $ hHead aProc)
