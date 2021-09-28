module Main (main) where

import           Test.Hspec

import           System.IO
import qualified TmpProc.Example2.IntegrationSpec as Ex2

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hspec $ do
    Ex2.spec
