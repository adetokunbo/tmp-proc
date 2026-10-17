{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Test.System.TmpProc.DockerSpec
Copyright   : (c) 2026 Tim Emiola
Maintainer  : adetokunbo@users.noreply.github.com
SPDX-License-Identifier: BSD3

Tests for exception handling during docker proc startup, and for rootless docker
detection.
-}
module Test.System.TmpProc.DockerSpec (spec) where

import Control.Exception (AsyncException (..), SomeAsyncException, throwIO, try)
import System.Process (readProcess)
import System.TmpProc.Docker
  ( Pinged (..)
  , Proc (..)
  , isRootless
  , startup
  )
import Test.Hspec


spec :: Spec
spec = do
  describe "Rootless docker detection" $ do
    it "returns a Bool without throwing in standard docker mode" $ do
      result <- isRootless
      result `shouldBe` False

  describe "Proc typeclass defaults" $ do
    it "containerPort defaults to 0" $
      containerPort @AlwaysFailPing `shouldBe` 0
    it "uriOfRootless defaults to uriOf ignoring the host port" $
      uriOfRootless @AlwaysFailPing "127.0.0.1" 5432
        `shouldBe` uriOf @AlwaysFailPing "127.0.0.1"
    it "uriOfRootlessWith delegates to uriOfRootless using the first pair's host port" $
      uriOfRootlessWith @AlwaysFailPing "127.0.0.1" [(6379, 49153)]
        `shouldBe` uriOfRootless @AlwaysFailPing "127.0.0.1" 49153
    it "uriOfRootlessWith passes 0 to uriOfRootless when the pairs list is empty" $
      uriOfRootlessWith @AlwaysFailPing "127.0.0.1" []
        `shouldBe` uriOfRootless @AlwaysFailPing "127.0.0.1" 0

  describe "Exception handling during startup" $ do
    it "re-throws UserInterrupt during ping and cleans up the container" $ do
      result <- try @SomeAsyncException (startup InterruptOnPing)
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected UserInterrupt to be re-thrown"
      out <- readProcess "docker" ["ps", "-a", "--filter", "name=interrupt-on-ping", "--format", "{{.Names}}"] ""
      out `shouldNotContain` "interrupt-on-ping"

    it "cleans up the container when ping always fails" $ do
      startup AlwaysFailPing `shouldThrow` anyIOException
      out <- readProcess "docker" ["ps", "-a", "--filter", "name=always-fail-ping", "--format", "{{.Names}}"] ""
      out `shouldNotContain` "always-fail-ping"


-- | A 'Proc' whose 'ping' always throws 'UserInterrupt'.
data InterruptOnPing = InterruptOnPing


instance Proc InterruptOnPing where
  type Image InterruptOnPing = "alpine"
  type Name InterruptOnPing = "interrupt-on-ping"
  uriOf _ = ""
  reset _ = pure ()
  ping _ = throwIO UserInterrupt
  pingCount = 1


-- | A 'Proc' whose 'ping' always returns 'NotOK'.
data AlwaysFailPing = AlwaysFailPing


instance Proc AlwaysFailPing where
  type Image AlwaysFailPing = "alpine"
  type Name AlwaysFailPing = "always-fail-ping"
  uriOf _ = ""
  reset _ = pure ()
  ping _ = pure NotOK
  pingCount = 1
