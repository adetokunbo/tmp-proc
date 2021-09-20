module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.TmpProc.Docker.PostgresSpec as PG

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hspec PG.spec
