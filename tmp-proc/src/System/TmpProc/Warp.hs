{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_HADDOCK prune not-home #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Provides functions to conveniently run WAI applications in integration tests
with temporary processes as dependencies.

These aim to simplify setup and teardown code in integration tests that use
Warp.

-}
module System.TmpProc.Warp
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
  , handles
  , shutdown

    -- * health check support
  , checkHealth
  )

where

import           Control.Concurrent          (myThreadId, newEmptyMVar, putMVar,
                                              readMVar, takeMVar, threadDelay,
                                              throwTo)
import           Control.Exception           (ErrorCall (..))
import           Control.Monad               (void, when)
import           Control.Monad.Cont          (cont, runCont)
import           Network.Socket              (Socket, close)
import           Network.Wai                 (Application)
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import           UnliftIO                    (Async, async, bracket, cancel,
                                              catch, onException, race, throwIO,
                                              waitEither)

import           System.TmpProc.Docker       (AreProcs, HList (..), Proc2Handle,
                                              startupAll, terminateAll,
                                              withTmpProcs)


-- | Represents a started Warp application and any 'AreProcs' dependencies.
data ServerHandle as = ServerHandle
  { shServer  :: !(Async ())
  , shPort    :: !Warp.Port
  , shSocket  :: !Socket
  , shHandles :: !(HList as)
  }

-- | Runs an 'Application' with 'System.TmpProc.ProcHandle' dependencies on a free port.
runServer
  :: AreProcs as
  => HList as
  -> (HList (Proc2Handle as) -> IO Application)
  -> IO (ServerHandle (Proc2Handle as))
runServer = runReadyServer doNothing


{-| Like 'runServer'; with an additional @ready@ that determines if the server is ready.'. -}
runReadyServer
  :: AreProcs as
  => (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> HList as                   --  ^ defines the dependent @Proc@s
  -> (HList (Proc2Handle as) -> IO Application)
  -> IO (ServerHandle (Proc2Handle as))
runReadyServer = runReadyServer' Warp.runSettingsSocket


{-| Like 'runServer'; the port is secured with 'Warp.TLSSettings'. -}
runTLSServer
  :: AreProcs as
  => Warp.TLSSettings
  -> HList as
  -> (HList (Proc2Handle as) -> IO Application)
  -> IO (ServerHandle (Proc2Handle as))
runTLSServer tlsSettings = runReadyServer' (Warp.runTLSSocket tlsSettings)  doNothing


{-| Like 'runReadyServer'; the port is secured with 'Warp.TLSSettings'. -}
runReadyTLSServer
  :: AreProcs as
  => Warp.TLSSettings
  -> (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> HList as                   --  ^ defines the dependent @Proc@s
  -> (HList (Proc2Handle as) -> IO Application)
  -> IO (ServerHandle (Proc2Handle as))
runReadyTLSServer tlsSettings = runReadyServer' (Warp.runTLSSocket tlsSettings)


-- | Used to implement 'runReadyServer'
runReadyServer'
  :: AreProcs as
  => (Warp.Settings -> Socket -> Application -> IO ())
  -> (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> HList as                   --  ^ defines the dependent @Proc@s
  -> (HList (Proc2Handle as) -> IO Application)
  -> IO (ServerHandle (Proc2Handle as))
runReadyServer' runApp check procs mkApp = do
  callingThread <- myThreadId
  h <- startupAll procs
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


-- | Shuts down the 'ServerHandle' server and its @tmp proc@ dependencies.
shutdown :: AreProcs as => ServerHandle (Proc2Handle as) -> IO ()
shutdown h = do
  let ServerHandle { shServer, shSocket, shHandles } = h
  terminateAll shHandles
  cancel shServer
  close shSocket


-- | The @'ServerHandle's@  @"System.TmpProc.ProcHandle"@.
handles :: AreProcs as => ServerHandle (Proc2Handle as) -> HList (Proc2Handle as)
handles = shHandles


-- | The 'Warp.Port' on which the 'ServerHandle's server is running.
serverPort :: ServerHandle as -> Warp.Port
serverPort = shPort


{-| Set up some @"System.TmpProc.ProcHandle"s@ then run an 'Application' that uses
   them on a free port.

Allows the app to configure itself using the @tmp procs@, then provides a
callback with access to the handles.

The @tmp procs@ are shut down when the application is shut down.
-}
testWithApplication
  :: AreProcs as
  => HList as
  -> (HList (Proc2Handle as) -> IO Application)
  -> ((HList (Proc2Handle as), Warp.Port) -> IO a)
  -> IO a
testWithApplication procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ mkApp oh
  pure (oh, p)


{-| Like 'testWithApplication', but the port is secured using a 'Warp.TLSSettings. '-}
testWithTLSApplication
  :: AreProcs as
  =>  Warp.TLSSettings
  -> HList as
  -> (HList (Proc2Handle as) -> IO Application)
  -> ((HList (Proc2Handle as), Warp.Port) -> IO a)
  -> IO a
testWithTLSApplication tlsSettings procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ withTLSApplicationSettings tlsSettings Warp.defaultSettings $ mkApp oh
  pure (oh, p)


{-| Set up some @"System.TmpProc.ProcHandle"s@ then run an 'Application' that uses
   them on a free port.

Allows the app to configure itself using the @tmp procs@, then provides a
callback with access to the handles.

Also runs a @ready@ action that to determine if the application started
correctly.

The @tmp procs@ are shut down when the application is shut down.
-}
testWithReadyApplication
  :: AreProcs as
  => (Warp.Port -> IO ()) -- throws an exception if the server is not ready
  -> HList as
  -> (HList (Proc2Handle as) -> IO Application)
  -> ((HList (Proc2Handle as), Warp.Port) -> IO a)
  -> IO a
testWithReadyApplication check procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  w <- cont $ bracket (mkWaiter check) doNothing
  p <- cont $ Warp.testWithApplicationSettings (waiterSettings w) $ mkApp oh
  _ <- cont $ bracket (waitFor w p) doNothing
  return (oh, p)


{-| Like 'testWithReadyApplication'; the port is secured with 'Warp.TLSSettings'. -}
testWithReadyTLSApplication
  :: AreProcs as
  => Warp.TLSSettings
  -> (Warp.Port -> IO ()) -- throws an exception if the server is not ready
  -> HList as
  -> (HList (Proc2Handle as) -> IO Application)
  -> ((HList (Proc2Handle as), Warp.Port) -> IO a)
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


-- | Like "Network.Wai.Handler.Warp.openFreePort" but closes the socket before exiting.
withFreePort :: ((Warp.Port, Socket) -> IO a) -> IO a
withFreePort = bracket Warp.openFreePort (close . snd)


-- | Improves readability...
doNothing :: b -> IO ()
doNothing = const $ pure ()
