module Main (main) where

import           Test.Hspec

import           System.IO
import           System.TmpProc.Docker         (hasDocker)
import qualified Test.TmpProc.Docker.RedisSpec as RD

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  noDocker <- not <$> hasDocker
  hspec $ RD.spec noDocker
