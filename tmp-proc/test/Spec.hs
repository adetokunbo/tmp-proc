{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.System.TmpProc.HttpBinSpec as HB
import qualified Test.System.TmpProc.WarpSpec    as W3

import           System.TmpProc.Docker     (hasDocker)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  noDocker <- not <$> hasDocker
  hspec $ do
    HB.spec noDocker
    W3.spec noDocker
