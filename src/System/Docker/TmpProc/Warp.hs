{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module System.Docker.TmpProc.Warp
  ( -- * test functions in continuation-passing style
    testWithApplication
  , testWithTLSApplication
  , testWithReadyApplication
  , testWithReadyTLSApplication

    -- * ServerHandle and related functions
  , ServerHandle
  , runReadyServer
  , runReadyTLSServer
  , runServer
  , runTLSServer
  , serverPort
  , handle
  , shutdown

    -- * health check support
  , checkHealth
  )

where

import           Control.Concurrent          (newEmptyMVar, putMVar, readMVar,
                                              takeMVar, threadDelay, myThreadId, throwTo)
import           Control.Exception           (ErrorCall (..))
import           Control.Monad               (void, when)
import           Control.Monad.Cont          (cont, runCont)
import           Network.Socket              (Socket, close)
import           Network.Wai                 (Application)
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import           System.Docker.TmpProc       (Handle, TmpProc, cleanup,
                                              setupProcs, withTmpProcs)
import           UnliftIO                    (Async, async, bracket, cancel,
                                              onException, race, throwIO,
                                              waitEither, catch)


-- | Represents a started Warp application and it's 'TmpProc' dependencies.
data ServerHandle = ServerHandle
  { shServer :: Async ()
  , shPort   :: Warp.Port
  , shSocket :: Socket
  , shHandle :: Handle
  }


-- | Runs an 'Application' with 'TmpProc' dependencies on a free port, with a
-- @ready@ action that checks if the server is up.
runReadyServer
  :: (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> [TmpProc]                  --  ^ defines the dependent @TmpProc@s
  -> (Handle -> IO Application)
  -> IO ServerHandle
runReadyServer = runReadyServer' Warp.runSettingsSocket


-- | Runs an 'Application' with 'TmpProc' dependencies on a free port.
runServer
  :: [TmpProc]
  -> (Handle -> IO Application)
  -> IO ServerHandle
runServer = runReadyServer doNothing


-- | Like 'runReadyServer', but the port is secured using the provided
-- 'Warp.TLSSettings'.
runReadyTLSServer
  :: Warp.TLSSettings
  -> (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> [TmpProc]                  --  ^ defines the dependent @TmpProc@s
  -> (Handle -> IO Application)
  -> IO ServerHandle
runReadyTLSServer tlsSettings = runReadyServer' (Warp.runTLSSocket tlsSettings)


-- | Runs an 'Application' with 'TmpProc' dependencies on a free port.
runTLSServer
  :: Warp.TLSSettings
  -> [TmpProc]
  -> (Handle -> IO Application)
  -> IO ServerHandle
runTLSServer tlsSettings = runReadyServer' (Warp.runTLSSocket tlsSettings)  doNothing


-- | Used to implement 'runReadyServer'
runReadyServer'
  :: (Warp.Settings -> Socket -> Application -> IO ())
  -> (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> [TmpProc]                  --  ^ defines the dependent @TmpProc@s
  -> (Handle -> IO Application)
  -> IO ServerHandle
runReadyServer' runApp check procs mkApp = do
  callingThread <- myThreadId
  h <- setupProcs procs
  (p, sock) <- Warp.openFreePort
  signal <- newEmptyMVar
  let settings = readySettings(putMVar signal ())
  app <- mkApp h
  let wrappedApp request respond =
        app request respond `catch` \ e -> do
          when
            (Warp.defaultShouldDisplayException e)
            (throwTo callingThread e)
          throwIO e
  s <- async (pure wrappedApp >>= runApp settings sock)
  aConfirm <- async (takeMVar signal)
  let result = ServerHandle s p sock h
  waitEither s aConfirm >>= \case
    Left _ -> do
      shutdown result
      error "setup: server thread stopped unexpectedly"
    Right _ -> do
      check p `onException` shutdown result
      pure result


-- | Shuts down the 'ServerHandle' server and its 'TmpProc' dependencies.
shutdown :: ServerHandle -> IO ()
shutdown h = do
  let ServerHandle { shServer, shSocket, shHandle } = h
  cleanup shHandle
  cancel shServer
  close shSocket


-- | The 'Handle' for interacting with a 'ServerHandle's 'TmpProc' dependencies.
handle :: ServerHandle -> Handle
handle = shHandle


-- | The 'Warp.Port' on the 'ServerHandle's server is running.
serverPort :: ServerHandle -> Warp.Port
serverPort = shPort


-- | Runs an 'Application' on a free port, after setting up some 'TmpProc's and
-- allowing the app to configure itself using them. Also provides the
-- continuation with access to the handle.
--
-- The tmp process are shut down when the application shut down.
testWithApplication
  :: [TmpProc]
  -> (Handle -> IO Application)
  -> ((Handle, Warp.Port) -> IO a)
  -> IO a
testWithApplication procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ mkApp oh
  pure (oh, p)


-- | Like 'testWithApplication', but the port is secured using the provided
-- 'Warp.TLSSettings'.
testWithTLSApplication
  :: Warp.TLSSettings
  -> [TmpProc]
  -> (Handle -> IO Application)
  -> ((Handle, Warp.Port) -> IO a)
  -> IO a
testWithTLSApplication tlsSettings procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ withTLSApplicationSettings tlsSettings Warp.defaultSettings $ mkApp oh
  pure (oh, p)


-- | Runs an 'Application' on a free port after setting up some 'TmpProc's and
-- allowing the app to configure itself using them.
--
-- Also runs a @ready@ action that can determine f the application started
-- correctly.
--
-- The tmp process are shut down when the application shut down.
testWithReadyApplication
  :: (Warp.Port -> IO ()) -- throws an exception if the server is not ready
  -> [TmpProc]
  -> (Handle -> IO Application)
  -> ((Handle, Warp.Port) -> IO a)
  -> IO a
testWithReadyApplication check procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  w <- cont $ bracket (mkWaiter check) doNothing
  p <- cont $ Warp.testWithApplicationSettings (waiterSettings w) $ mkApp oh
  _ <- cont $ bracket (waitFor w p) doNothing
  return (oh, p)


-- | Like 'testWithReadyApplication' but the port is secured using the provided
-- 'Warp.TLSSettings'.
testWithReadyTLSApplication
  :: Warp.TLSSettings
  -> (Warp.Port -> IO ()) -- throws an exception if the server is not ready
  -> [TmpProc]
  -> (Handle -> IO Application)
  -> ((Handle, Warp.Port) -> IO a)
  -> IO a
testWithReadyTLSApplication tlsSettings check procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  w <- cont $ bracket (mkWaiter check) doNothing
  p <- cont $ withTLSApplicationSettings tlsSettings (waiterSettings w) $ mkApp oh
  _ <- cont $ bracket (waitFor w p) doNothing
  return (oh, p)


-- | Simplifies writing the health checks used by @ready@ variants of this module.
checkHealth :: Int -> IO (Either a b) -> IO ()
checkHealth tries h = go tries
  where
    go 0 = error "healthy: server isn't healthy"
    go n = h >>= \case
      Left  _ -> threadDelay pingPeriod >> go (n - 1)
      Right _ -> pure ()



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


-- | Gap between service pings in milliseconds.
pingPeriod :: Int
pingPeriod = 1000000


-- | Like 'Warp.testWithApplicationSettings' , but the port is secured using the
-- provided 'Warp.TLSSettings'.
withTLSApplicationSettings
  :: Warp.TLSSettings
  -> Warp.Settings
  -> IO Application
  -> (Warp.Port -> IO a)
  -> IO a
withTLSApplicationSettings tlsSettings settings mkApp action = do
  app <- mkApp
  withFreePort $ \ (p, sock) -> do
    started <- mkWaiter doNothing
    let settings' = Warp.setBeforeMainLoop (notify started ()) settings
    result <- race
      (Warp.runTLSSocket tlsSettings settings' sock app)
      (waitFor started p >> action p)
    case result of
      Left () -> throwIO $ ErrorCall "Unexpected: runSettingsSocket exited"
      Right x -> return x


-- | Like 'openFreePort' but closes the socket before exiting.
withFreePort :: ((Warp.Port, Socket) -> IO a) -> IO a
withFreePort = bracket Warp.openFreePort (close . snd)


-- | Improves readability...
doNothing :: b -> IO ()
doNothing = const $ pure ()
