module System.Docker.TmpProc.Warp
  ( -- * functions
    testWithApplication
  , testWithApplication'
  , testWithReadyApplication
  , runReadyCancellableApp
  , runCancellableApp

  -- * type aliases
  , TmpWarpHandle
  )

where

import           Control.Concurrent.Async
import           Control.Monad.Cont                (cont, runCont)
import           Network.Socket                    (close)
import           Network.Wai                       (Application)
import qualified Network.Wai.Handler.Warp          as Warp

import           System.Docker.TmpProc             (OwnerHandle, TmpProc,
                                                    cleanup, setupProcs,
                                                    withTmpProcs)


-- | TmpWarpHandle is convenience type; it combines the 'OwnerHandle' and
-- 'Warp.Port' returned several functions in this module.
type TmpWarpHandle = (OwnerHandle, Warp.Port)


-- | Runs an 'Application' on a free port, after setting up some 'TmpProc's and
-- allowing the app to configure itself using them.
--
-- The tmp process are shut down when the application shut down.
testWithApplication
  :: [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> (Warp.Port -> IO a)
  -> IO a
testWithApplication procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ mkApp oh
  pure p


-- | Runs an 'Application' with 'TmpProc' dependencies on a free port.
--
-- The result is a tuple ('OwnerHandle', 'Warp.Port'), designed for easy use
-- with Tasty's withResource function.
runCancellableApp
  :: [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> IO (IO TmpWarpHandle, (TmpWarpHandle -> IO()))
runCancellableApp procs mkApp = do
  oh <- setupProcs procs
  (port, socket) <- Warp.openFreePort
  let settings = Warp.setPort port $ Warp.defaultSettings
  server <- async (mkApp oh >>= Warp.runSettings settings)
  let cleanup' = do cleanup oh
                    cancel server
                    close socket
  pure (pure (oh, port), const cleanup')


-- | Runs an 'Application' with 'TmpProc' dependencies on a free port.
--
-- The result is a tuple ('OwnerHandle', 'Warp.Port'), designed for easy use
-- with Tasty's withResource function.
runReadyCancellableApp
  :: (IO (), Warp.Port -> IO ())
  -> [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> IO (IO TmpWarpHandle, (TmpWarpHandle -> IO()))
runReadyCancellableApp (signal, check) procs mkApp = do
  oh <- setupProcs procs
  (port, socket) <- Warp.openFreePort
  let settings = Warp.setPort port
        $ Warp.setBeforeMainLoop signal
        $ Warp.defaultSettings
  server <- async (mkApp oh >>= Warp.runSettings settings)
  let cleanup' = do cleanup oh
                    cancel server
                    close socket
  pure (check port >> pure (oh, port), const cleanup')


-- | Runs an 'Application' on a free port, after setting up some 'TmpProc's and
-- allowing the app to configure itself using them. Also provides the
-- continuation with access to the handle.
--
-- The tmp process are shut down when the application shut down.
testWithApplication'
  :: [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> ((OwnerHandle, Warp.Port) -> IO a)
  -> IO a
testWithApplication' procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ mkApp oh
  pure (oh, p)


-- | Same as 'testWithApplication', but provides actions for checking the
-- application is running correctly.
--
-- The types for the items in the tuple need to be improved.
--
-- They are supposed to be a pair of actions, one which puts an item in an MVar
-- to signal that the other can proceed, and the other waits the waits on the
-- MVar and then performs a check that will throw an exception if the server is
-- not ready.
--
-- TODO: improve the types to constrain what these actions do.
--
-- The tmp process are shut down when the application shut down.
testWithReadyApplication
  :: (IO (), Warp.Port -> IO ())
  -> [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> (Warp.Port -> IO a)
  -> IO a
testWithReadyApplication (signal, check) procs mkApp = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplicationSettings (readySettings signal) $ mkApp oh
  _ <- pure $ check p
  return p


-- | A 'Warp.Settings' configured with a ready action
--
-- During tests, ready can be used to check if a server is healthy.
readySettings :: IO () -> Warp.Settings
readySettings ready = Warp.setBeforeMainLoop ready Warp.defaultSettings
