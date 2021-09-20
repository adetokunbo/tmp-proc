module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.TmpProc.Docker.RabbitMQSpec as RMQ

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hspec RMQ.spec
