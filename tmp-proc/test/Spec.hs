{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.System.TmpProc.HttpBinSpec as HB
import qualified Test.System.TmpProc.WarpSpec    as W3

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hspec $ do
    HB.spec
    W3.spec
