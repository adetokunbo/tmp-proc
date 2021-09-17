module Main (main) where

import           Test.Hspec

import           System.IO
import           System.TmpProc.Docker            (hasDocker)
import qualified Test.TmpProc.Docker.RabbitMQSpec as RMQ

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  noDocker <- not <$> hasDocker
  hspec $ RMQ.spec noDocker
