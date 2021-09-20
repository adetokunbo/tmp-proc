module Main (main) where

import           Test.Hspec

import           System.IO
import           System.TmpProc.Docker            (hasDocker)
import qualified Test.TmpProc.Docker.ZipkinSpec as ZK

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  noDocker <- not <$> hasDocker
  hspec $ ZK.spec noDocker
