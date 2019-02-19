{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Test.Hspec

import           System.IO
import qualified Test.NoopServerSpec            as Noop
import qualified Test.TmpProc.Postgres          as Postgres
import qualified Test.TmpProc.Redis             as Redis

import           System.Docker.TmpProc.Postgres as Postgres
import           System.Docker.TmpProc.Redis    as Redis

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  noDocker <- not <$> hasDocker
  hspec $ do
    let noReset = [Postgres.mkNoResetProc, Redis.mkNoResetProc]
    mapM_ (Noop.noopSpec noDocker) noReset
    Postgres.spec noDocker
    Redis.spec noDocker
