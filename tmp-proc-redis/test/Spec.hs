module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.TmpProc.Docker.RedisSpec as RD

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hspec RD.spec
