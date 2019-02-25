{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.TmpProc.Postgres as Postgres
import qualified Test.TmpProc.Redis    as Redis
import qualified Test.TmpProc.WarpSpec as Warp

import           System.Docker.TmpProc (hasDocker)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  noDocker <- not <$> hasDocker
  hspec $ do
    Postgres.spec noDocker
    Redis.spec noDocker
    Warp.mkSpec noDocker Postgres.testTmpProc Postgres.testWaiApp
    Warp.mkSpec noDocker Redis.testTmpProc Redis.testWaiApp
