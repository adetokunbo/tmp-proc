{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Provides functions that make it easy to run /'Application's/
 that access services running as @tmp@ @procs@ in integration tests.
-}
module System.TmpProc.Warp
  ( -- * Continuation-style setup
    testWithApplication
  , testWithReadyApplication
  , testWithTLSApplication
  , testWithReadyTLSApplication

    -- * ServerHandle
  , ServerHandle
  , serverPort
  , handles
  , shutdown
  , runServer
  , runReadyServer
  , runTLSServer
  , runReadyTLSServer

    -- * Health check support
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

import           System.TmpProc.Docker       (AreProcs, HList (..), HandlesOf,
                                              startupAll, terminateAll,
                                              withTmpProcs)


-- | Represents a started Warp application and any 'AreProcs' dependencies.
data ServerHandle procs = ServerHandle
  { shServer  :: !(Async ())
  , shPort    :: !Warp.Port
  , shSocket  :: !Socket
  , shHandles :: !(HandlesOf procs)
  }

-- | Runs an 'Application' with @ProcHandle@ dependencies on a free port.
runServer
  :: AreProcs procs
  => HList procs
  -> (HandlesOf procs -> IO Application)
  -> IO (ServerHandle procs)
runServer = runReadyServer doNothing


{-| Like 'runServer'; with an additional @ready@ that determines if the server is ready.'. -}
runReadyServer
  :: AreProcs procs
  => (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> HList procs                --  ^ defines the dependent @Proc@s
  -> (HandlesOf procs -> IO Application)
  -> IO (ServerHandle procs)
runReadyServer = runReadyServer' Warp.runSettingsSocket


{-| Like 'runServer'; the port is secured with 'Warp.TLSSettings'. -}
runTLSServer
  :: AreProcs procs
  => Warp.TLSSettings
  -> HList procs
  -> (HandlesOf procs -> IO Application)
  -> IO (ServerHandle procs)
runTLSServer tlsSettings = runReadyServer' (Warp.runTLSSocket tlsSettings)  doNothing


{-| Like 'runReadyServer'; the port is secured with 'Warp.TLSSettings'. -}
runReadyTLSServer
  :: AreProcs procs
  => Warp.TLSSettings
  -> (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> HList procs                --  ^ defines the dependent @Proc@s
  -> (HandlesOf procs -> IO Application)
  -> IO (ServerHandle procs)
runReadyTLSServer tlsSettings = runReadyServer' (Warp.runTLSSocket tlsSettings)


-- | Used to implement 'runReadyServer'
runReadyServer'
  :: AreProcs procs
  => (Warp.Settings -> Socket -> Application -> IO ())
  -> (Warp.Port -> IO ())       --  ^ throws an exception if the server is not ready
  -> HList procs                   --  ^ defines the dependent @Proc@s
  -> (HandlesOf procs -> IO Application)
  -> IO (ServerHandle procs)
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
shutdown :: AreProcs procs => ServerHandle procs -> IO ()
shutdown h = do
  let ServerHandle { shServer, shSocket, shHandles } = h
  terminateAll shHandles
  cancel shServer
  close shSocket


-- | The @'ServerHandle's@  @ProcHandles@.
handles :: AreProcs procs => ServerHandle procs -> HandlesOf procs
handles = shHandles


-- | The 'Warp.Port' on which the 'ServerHandle's server is running.
serverPort :: ServerHandle procs -> Warp.Port
serverPort = shPort


{- | Set up some @ProcHandles@ then run an 'Application' that uses them on a free
   port.

Allows the app to configure itself using the @tmp procs@, then provides a
callback with access to the handles.

The @tmp procs@ are shut down when the application is shut down.
-}
testWithApplication ::
  (AreProcs procs) =>
  HList procs ->
  (HandlesOf procs -> IO Application) ->
  ((HandlesOf procs, Warp.Port) -> IO a) ->
  IO a
testWithApplication procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ mkApp oh
  pure (oh, p)


-- | Like 'testWithApplication', but the port is secured using a 'Warp.TLSSettings. '
testWithTLSApplication ::
  (AreProcs procs) =>
  Warp.TLSSettings ->
  HList procs ->
  (HandlesOf procs -> IO Application) ->
  ((HandlesOf procs, Warp.Port) -> IO a) ->
  IO a
testWithTLSApplication tlsSettings procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ withTLSApplicationSettings tlsSettings Warp.defaultSettings $ mkApp oh
  pure (oh, p)


{- | Set up some @ProcHandles@ then run an 'Application' that uses them on a free
   port.

Allows the app to configure itself using the @tmp procs@, then provides a
callback with access to the handles.

Also runs a @ready@ action that to determine if the application started
correctly.

The @tmp procs@ are shut down when the application is shut down.
-}
testWithReadyApplication ::
  (AreProcs procs) =>
  (Warp.Port -> IO ()) -> -- throws an exception if the server is not ready
  HList procs ->
  (HandlesOf procs -> IO Application) ->
  ((HandlesOf procs, Warp.Port) -> IO a) ->
  IO a
testWithReadyApplication check procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  w <- cont $ bracket (mkWaiter check) doNothing
  p <- cont $ Warp.testWithApplicationSettings (waiterSettings w) $ mkApp oh
  _ <- cont $ bracket (waitFor w p) doNothing
  return (oh, p)


-- | Like 'testWithReadyApplication'; the port is secured with 'Warp.TLSSettings'.
testWithReadyTLSApplication ::
  (AreProcs procs) =>
  Warp.TLSSettings ->
  (Warp.Port -> IO ()) -> -- throws an exception if the server is not ready
  HList procs ->
  (HandlesOf procs -> IO Application) ->
  ((HandlesOf procs, Warp.Port) -> IO a) ->
  IO a
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
    go n =
      h >>= \case
        Left _ -> threadDelay pingPeriod >> go (n - 1)
        Right _ -> pure ()


{- | A 'Warp.Settings' configured with a ready action.

The ready action is used to check if a server is healthy.
-}
readySettings :: IO () -> Warp.Settings
readySettings ready = Warp.setBeforeMainLoop ready Warp.defaultSettings


{- | A 'Warp.Settings' configured with a ready action.

The ready action is used to check if a server is healthy.
-}
waiterSettings :: PortWaiter () -> Warp.Settings
waiterSettings w = Warp.setBeforeMainLoop (notify w ()) Warp.defaultSettings


-- | Simplifies creation of a ready action.
data PortWaiter a = PortWaiter
  { notify :: a -> IO ()
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
  pure
    PortWaiter
      { notify = putMVar mvar
      , waitFor
      }


-- | Gap between service pings in milliseconds.
pingPeriod :: Int
pingPeriod = 1000000


{- | Like 'Warp.testWithApplicationSettings' , but the port is secured using the
provided 'Warp.TLSSettings'.
-}
withTLSApplicationSettings ::
  Warp.TLSSettings ->
  Warp.Settings ->
  IO Application ->
  (Warp.Port -> IO a) ->
  IO a
withTLSApplicationSettings tlsSettings settings mkApp action = do
  app <- mkApp
  withFreePort $ \(p, sock) -> do
    started <- mkWaiter doNothing
    let settings' = Warp.setBeforeMainLoop (notify started ()) settings
    result <-
      race
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
