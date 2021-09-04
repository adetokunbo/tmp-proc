{-# LANGUAGE OverloadedStrings #-}

module Test.TmpProc.Hspec (noDockerSpec) where

import           Test.Hspec


-- | Used as pending alternative when docker is unavailable.
noDockerSpec :: String -> Spec
noDockerSpec desc = describe desc $
  it "cannot run as docker is unavailable" pending
