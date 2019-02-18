module Test.NoopServer (noopSetup, noopPort) where

import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forever)

import           System.Docker.TmpProc


-- | A fake server, ignores the port and 'OwnerHandle'.
noopServer :: OwnerHandle -> Port -> IO () -> IO ()
noopServer _oh _port ready = do
  let loopPeriod = 5000000
  ready
  forever $ threadDelay loopPeriod


noopOwner :: Owner IOError
noopOwner = Owner  noopServer $ const $ pure $ Right ()


noopPort :: Port
noopPort = 1


noopSetup :: TmpProc -> IO OwnerHandle
noopSetup tp = setup noopOwner [tp] noopPort
