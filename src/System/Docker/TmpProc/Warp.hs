module System.Docker.TmpProc.Warp
  ( -- * functions
    testWithApplication
  , testWithApplication'
  , testWithReadyApplication
  )

where

import           Control.Monad.Cont       (cont, runCont)
import           Network.Wai              (Application)
import qualified Network.Wai.Handler.Warp as Warp

import           System.Docker.TmpProc    (OwnerHandle, TmpProc, withTmpProcs)


-- | Runs an 'Application' on a free port, after setting up some 'TmpProc's and
-- allowing the app to configure itself using them.
--
-- The tmp process are shut down when the application shut down.
testWithApplication
  :: [TmpProc]
  -> (OwnerHandle -> IO Application)
  -> (Warp.Port -> IO a)
  -> IO a
testWithApplication procs action = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ action oh
  pure p


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
testWithApplication' procs action = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplication $ action oh
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
testWithReadyApplication (signal, check) procs action = runCont $ do
  oh <- cont $ withTmpProcs procs
  p <- cont $ Warp.testWithApplicationSettings (readySettings signal) $ action oh
  _ <- pure $ check p
  return p


-- | A 'Warp.Settings' configured with a ready action
--
-- During tests, ready can be used to check if a server is healthy.
readySettings :: IO () -> Warp.Settings
readySettings ready = Warp.setBeforeMainLoop ready Warp.defaultSettings
