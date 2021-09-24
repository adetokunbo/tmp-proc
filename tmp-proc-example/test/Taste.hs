module Main (main) where

import           System.IO
import qualified TmpProc.Example1.IntegrationTaste as Ex1

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  Ex1.main
