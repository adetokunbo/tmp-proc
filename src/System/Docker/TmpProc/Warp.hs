{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module System.Docker.TmpProc.Warp
  ( -- * functions with test server continuations
    testWithApplication
  , testWithReadyApplication

    -- * ServerHandle and related functions
  , ServerHandle
  , runReadyServer
  , runServer
  , port
  , ownerHandle
  , shutdown
  )

where

import           Control.Concurrent       (newEmptyMVar, putMVar, readMVar,
                                           takeMVar)
import           Control.Monad            (void)
import           Control.Monad.Cont       (cont, runCont)
import           Network.Socket           (Socket, close)
import           Network.Wai              (Application)
import qualified Network.Wai.Handler.Warp as Warp

import           System.Docker.TmpProc    (OwnerHandle, TmpProc, cleanup,
                                           setupProcs, withTmpProcs)
import           UnliftIO                 (Async, async, bracket, cancel,
                                           onException, waitEither)


-- | Represents a started Warp application and it's 'TmpProc' dependencies.
data ServerHandle = ServerHandle
  { shServer :: Async ()
  , shPort   :: Warp.Port
  , shSocket :: Socket
  , shHandle :: OwnerHandle
  }


-- | Runs an 'Application' with 'TmpProc' dependencies on a free port, with a
-- @ready@ action that checks if the server is up.
runReadyServer
  :: (Warp.Port -> IO ())       --  ^ throws an exception if the client is not ready
  -> [TmpProc]                  --  ^ defines the dependent @TmpProc@s
  -> (OwnerHandle -> IO Application)
  -> IO ServerHandle
runReadyServer check procs mkApp = do
  h <- setupProcs procs
  (p, sock) <- Warp.openFreePort
  signal <- newEmptyMVar
  let settings = Warp.setPort p $ readySettings(putMVar signal ())
  s <- async (mkApp h >>= Warp.runSettings settings)
  aConfirm <- async (takeMVar signal)
  let result = ServerHandle s p sock h
  waitEither s aConfirm >>= \case
    Left _ -> do
      shutdown result
      error "setup: server thread stopped unexpectedly"
    Right _ -> do
      check p `onException` shutdown result
      pure result


-- | Runs an 'Application' with 'TmpProc' dependencies on a free port.
runServer
  :: [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> IO ServerHandle
runServer = runReadyServer (const $ pure ())


-- | Shuts down the 'ServerHandle' server and its 'TmpProc' dependencies.
shutdown :: ServerHandle -> IO ()
shutdown h = do
  let ServerHandle { shServer, shSocket, shHandle } = h
  cleanup shHandle
  cancel shServer
  close shSocket


-- | The 'OwnerHandle' for interacting with a 'ServerHandle's 'TmpProc' dependencies.
ownerHandle :: ServerHandle -> OwnerHandle
ownerHandle = shHandle


-- | The 'Warp.Port' on the 'ServerHandle's server is running.
port :: ServerHandle -> Warp.Port
port = shPort


-- | Runs an 'Application' on a free port, after setting up some 'TmpProc's and
-- allowing the app to configure itself using them. Also provides the
-- continuation with access to the handle.
--
-- The tmp process are shut down when the application shut down.
testWithApplication
  :: [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> ((OwnerHandle, Warp.Port) -> IO a)
  -> IO a
testWithApplication procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ mkApp oh
  pure (oh, p)


-- | Runs an 'Application' on a free port, after setting up some 'TmpProc's and
-- allowing the app to configure itself using them.
--
-- Also allows a @ready@ action to determine if the application started
-- correctly.
--
-- The tmp process are shut down when the application shut down.
testWithReadyApplication
  :: (Warp.Port -> IO ())
  -> [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> ((OwnerHandle, Warp.Port) -> IO a)
  -> IO a
testWithReadyApplication check procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  w <- cont $ bracket (mkWaiter check) (const pure ())
  p <- cont $ Warp.testWithApplicationSettings (waiterSettings w) $ mkApp oh
  _ <- cont $ bracket (waitFor w p) (const pure ())
  return (oh, p)


-- | A 'Warp.Settings' configured with a ready action.
--
-- The ready action is used to check if a server is healthy.
readySettings :: IO () -> Warp.Settings
readySettings ready = Warp.setBeforeMainLoop ready Warp.defaultSettings



-- | A 'Warp.Settings' configured with a ready action.
--
-- The ready action is used to check if a server is healthy.
waiterSettings :: PortWaiter () -> Warp.Settings
waiterSettings w = Warp.setBeforeMainLoop (notify w ()) Warp.defaultSettings


-- | Simplifies creation of a ready action.
data PortWaiter a =
  PortWaiter
  { notify  :: a -> IO ()
  , waitFor :: Warp.Port -> IO a
  }


-- | Simplifies creation of a ready action.
mkWaiter :: (Warp.Port -> IO a) -> IO (PortWaiter a)
mkWaiter check = do
  mvar <- newEmptyMVar
  let waitFor p = do
        res <- readMVar mvar
        void $ check p
        pure res
  pure PortWaiter
    { notify = putMVar mvar
    , waitFor
    }
