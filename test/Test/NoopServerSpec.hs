{-# LANGUAGE ScopedTypeVariables #-}
module Test.NoopServerSpec where

import           Test.Hspec

import           Data.Either                    (isRight)
import qualified Data.Text                      as Text

import           System.Docker.TmpProc

import           Test.NoopServer                (noopSetup)
import           Test.TmpProc.Hspec             (noDockerSpec)


noopSpec :: Bool -> TmpProc -> Spec
noopSpec noDocker tp = do
  let name = procImageName tp
      desc = "with image " ++ (Text.unpack name) ++ " and a noop server"

  if noDocker then noDockerSpec desc else do
    beforeAll (noopSetup tp) $ afterAll cleanup $ do
      describe desc $ do
        context "invoking reset" $ do
          it "should not fail" $ \oh -> do
            reset name oh `shouldReturn` ()

        context "obtaining the process' URI" $ do
          it "should succeed" $ \oh -> do
            (isRight $ procURI name oh) `shouldBe` True
