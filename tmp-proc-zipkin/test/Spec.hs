module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.TmpProc.Docker.ZipkinSpec as ZK

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hspec ZK.spec
