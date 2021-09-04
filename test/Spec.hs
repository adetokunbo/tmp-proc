{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.System.TmpProc.HttpBinSpec as HB
import qualified Test.System.TmpProc.WarpSpec    as W3
import qualified Test.TmpProc.PostgresSpec as Postgres
import qualified Test.TmpProc.RedisSpec    as Redis
import qualified Test.TmpProc.WarpSpec     as Warp

import           System.Docker.TmpProc     (hasDocker)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  noDocker <- not <$> hasDocker
  hspec $ do
    Postgres.spec noDocker
    HB.spec noDocker
    W3.spec noDocker
    Redis.spec noDocker
    Warp.mkSpec noDocker Postgres.testTmpProc Postgres.testWaiApp
    Warp.mkSpec noDocker Redis.testTmpProc Redis.testWaiApp
