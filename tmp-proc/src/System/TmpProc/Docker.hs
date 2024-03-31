{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Provides the core data types and combinators used to launch temporary /(tmp)/
processes /(procs)/ using docker.

@tmp-proc@ aims to simplify integration tests that use dockerizable services.

* Basically, @tmp-proc@ helps launch services used in integration test on docker

* While it's possible to write integration tests that use services hosted on
  docker /without/ @tmp-proc@, @tmp-proc@ aims to make writing those kind of
  tests easier, by providing types and combinators that take care of

    * launching services on docker
    * obtaining references to the launched service
    * cleaning up docker once the tests are finished

This module does all that via its data types:

* A /'Proc'/ specifies a docker image that provides a service and other details
  related to its use in tests.

* A /'ProcHandle'/ is created whenever a service specifed by a /'Proc'/ is
started, and is used to access and eventually terminate the running service.

* Some @'Proc's@ will also be /'Connectable'/; these specify how access the
service via some /'Conn'-ection/ type.
-}
module System.TmpProc.Docker
  ( -- * @'Proc'@
    Proc (..)
  , Pinged (..)
  , AreProcs
  , nameOf
  , startup
  , toPinged
  , uriOf'
  , runArgs'

    -- * customize docker startup
  , ToRunCmd (..)
  , Preparer (..)

    -- * @'ProcHandle'@
  , ProcHandle (..)
  , Proc2Handle
  , HandlesOf
  , NetworkHandlesOf
  , startupAll
  , startupAll'
  , terminateAll
  , netwTerminateAll
  , netwStartupAll
  , withTmpProcs
  , manyNamed
  , genNetworkName
  , handleOf
  , ixReset
  , ixPing
  , ixUriOf
  , HasHandle
  , HasNamedHandle
  , SomeNamedHandles

    -- * @'Connectable'@
  , Connectable (..)
  , Connectables
  , withTmpConn
  , withConnOf
  , openAll
  , closeAll
  , withConns
  , withKnownConns
  , withNamedConns

    -- * Docker status
  , hasDocker

    -- * Aliases
  , HostIpAddress
  , SvcURI

    -- * Re-exports
  , module System.TmpProc.TypeLevel
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( Exception
  , SomeException
  , bracket
  , catch
  , onException
  )
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as C8
import Data.Kind (Type)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word16)
import Fmt ((+|), (|+))
import GHC.TypeLits
  ( CmpSymbol
  , KnownSymbol
  , Nat
  , Symbol
  , symbolVal
  )
import Numeric.Natural (Natural)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (..), openBinaryFile, stderr)
import System.Process
  ( CreateProcess
  , StdStream (..)
  , proc
  , readCreateProcess
  , readProcess
  , std_err
  , std_out
  , waitForProcess
  , withCreateProcess
  )
import System.Random (randomIO)
import System.TmpProc.TypeLevel
  ( Drop
  , HList (..)
  , HalfOf
  , IsAbsent
  , IsInProof
  , KV (..)
  , LengthOf
  , ManyMemberKV
  , MemberKV
  , ReorderH (..)
  , SortSymbols
  , Take
  , both
  , hHead
  , hOf
  , only
  , select
  , selectMany
  , (&:)
  , (&:&)
  )


-- | Determines if the docker daemon is accessible.
hasDocker :: IO Bool
hasDocker = do
  let rawSystemNoStdout cmd args =
        withCreateProcess
          (proc cmd args) {std_out = CreatePipe, std_err = CreatePipe}
          (\_ _ _ -> waitForProcess)
          `catch` (\(_ :: IOError) -> return (ExitFailure 127))
      succeeds ExitSuccess = True
      succeeds _ = False

  succeeds <$> rawSystemNoStdout "docker" ["ps"]


-- | Set up some @'Proc's@, run an action that uses them, then terminate them.
withTmpProcs ::
  (AreProcs procs) =>
  HList procs ->
  (HandlesOf procs -> IO b) ->
  IO b
withTmpProcs procs action =
  let wrapAction f (_, ps) = f ps
   in bracket (netwStartupAll procs) netwTerminateAll $ wrapAction action


-- | Provides access to a 'Proc' that has been started.
data ProcHandle a = ProcHandle
  { hProc :: !a
  , hPid :: !String
  , hUri :: !SvcURI
  , hAddr :: !HostIpAddress
  }


-- | Start up processes for each 'Proc' type.
startupAll :: (AreProcs procs) => HList procs -> IO (HandlesOf procs)
startupAll ps = snd <$> startupAll' Nothing ps


-- | Like 'startupAll' but creates a new docker network and that the processes use
netwStartupAll :: (AreProcs procs) => HList procs -> IO (NetworkHandlesOf procs)
netwStartupAll ps = do
  netwName <- genNetworkName
  startupAll' (Just netwName) ps


addresses :: (AreProcs procs) => HandlesOf procs -> [(Text, HostIpAddress)]
addresses = go procProof
  where
    go :: SomeProcs as -> HandlesOf as -> [(Text, HostIpAddress)]
    go SomeProcsNil HNil = []
    go (SomeProcsCons cons) (x `HCons` y) = (nameOf $ hProc x, hAddr x) : go cons y


-- | Start up processes for each 'Proc' type.
startupAll' :: (AreProcs procs) => Maybe Text -> HList procs -> IO (NetworkHandlesOf procs)
startupAll' ntwkMb ps =
  let
    mayCreateNetwork = case ntwkMb of
      Nothing -> pure ()
      Just name -> void $ readProcess "docker" (createNetworkArgs $ Text.unpack name) ""
    wrap x = (fromMaybe "" ntwkMb, x)

    go :: SomeProcs as -> HList as -> IO (HandlesOf as)
    go SomeProcsNil HNil = pure HNil
    go (SomeProcsCons cons) (x `HCons` y) = do
      others <- go cons y
      let addrs = addresses others
      h <- startup' ntwkMb addrs x `onException` terminateAll others
      pure $ h `HCons` others
   in
    do
      mayCreateNetwork
      wrap <$> go procProof ps


-- | Terminate all processes owned by some @'ProcHandle's@.
terminateAll :: (AreProcs procs) => HandlesOf procs -> IO ()
terminateAll = go $ p2h procProof
  where
    go :: SomeHandles as -> HList as -> IO ()
    go SomeHandlesNil HNil = pure ()
    go (SomeHandlesCons cons) (x `HCons` y) = do
      terminate x
      go cons y


{- | Like 'terminateAll', but also removes the docker network connecting the
processes.
-}
netwTerminateAll :: (AreProcs procs) => NetworkHandlesOf procs -> IO ()
netwTerminateAll (ntwk, ps) = do
  let name' = Text.unpack ntwk
  terminateAll ps
  void $ readProcess "docker" (removeNetworkArgs name') ""


-- | Terminate the process owned by a @'ProcHandle's@.
terminate :: ProcHandle p -> IO ()
terminate handle = do
  let pid = hPid handle
  void $ readProcess "docker" ["stop", pid] ""
  void $ readProcess "docker" ["rm", pid] ""


{- | Allow customization of the docker command that launches a @'Proc'@
|
| The full command is
|   `docker run -d <optional-args> $(imageText a)`
|
| A fallback instance is provided that works for any instance of @Proc a@
| Specify a new instance of @ToRunCmd@ to control <optional-args>
-}
class ToRunCmd a where
  -- * Generate args that follow the initial ['docker', 'run', '-d']
  toRunCmd :: a -> [Text]


instance {-# OVERLAPPABLE #-} (Proc a) => ToRunCmd a where
  toRunCmd _ = runArgs @a


class Preparer a where
  -- * Hook to run before the docker container is started
  prepare :: [(Text, HostIpAddress)] -> a -> IO a


instance {-# OVERLAPPABLE #-} (Proc a) => Preparer a where
  prepare _ = pure


-- | Specifies how to a get a connection to a 'Proc'.
class (Proc a) => Connectable a where
  -- | The connection type.
  type Conn a = (conn :: Type) | conn -> a


  -- | Get a connection to the Proc via its 'ProcHandle'.
  openConn :: ProcHandle a -> IO (Conn a)


  -- | Close a connection to a 'Proc'.
  closeConn :: Conn a -> IO ()
  closeConn = const $ pure ()


-- | Specifies how to launch a temporary process using Docker.
class (KnownSymbol (Image a), KnownSymbol (Name a)) => Proc a where
  -- | The image name of the docker image, e.g, /postgres:10.6/
  type Image a :: Symbol


  -- | A label used to refer to running process created from this image, e.g,
  --   /a-postgres-db/
  type Name a = (labelName :: Symbol) | labelName -> a


  -- | Additional arguments to the docker command that launches the tmp proc.
  runArgs :: [Text]
  runArgs = mempty


  -- | Determines the service URI of the process, when applicable.
  uriOf :: HostIpAddress -> SvcURI


  -- | Resets some state in a tmp proc service.
  reset :: ProcHandle a -> IO ()


  -- | Checks if the tmp proc started ok.
  ping :: ProcHandle a -> IO Pinged


  -- | Maximum number of pings to perform during startup.
  pingCount :: Natural
  pingCount = 4


  -- | Number of milliseconds between pings.
  pingGap :: Natural
  pingGap = 1000000


{- | Indicates the result of pinging a 'Proc'.

If the ping succeeds, 'ping' should return 'OK'.

'ping' should catch any exceptions that are expected when the @'Proc's@ service
is not available and return 'NotOK'.

'startupAll' uses 'PingFailed' to report any unexpected exceptions that escape
'ping'.
-}
data Pinged
  = -- | The service is running OK.
    OK
  | -- | The service is not running.
    NotOK
  | -- | Contact to the service failed unexpectedly.
    PingFailed Text
  deriving (Eq, Show)


-- | Name of a process.
nameOf :: forall a. (Proc a) => a -> Text
nameOf _ = Text.pack $ symbolVal (Proxy :: Proxy (Name a))


-- | Simplifies use of 'runArgs'.
runArgs' :: forall a. (Proc a) => a -> [Text]
runArgs' _ = runArgs @a


-- | Simplifies use of 'pingCount'.
pingCount' :: forall a. (Proc a) => a -> Natural
pingCount' _ = pingCount @a


-- | Simplifies use of 'pingGap'.
pingGap' :: forall a. (Proc a) => a -> Natural
pingGap' _ = pingGap @a


-- | Simplifies use of 'uriOf'.
uriOf' :: forall a. (Proc a) => a -> HostIpAddress -> SvcURI
uriOf' _ = uriOf @a


-- | The full args of a @docker run@ command for starting up a 'Proc'.
dockerCmdArgs :: forall a. (Proc a, ToRunCmd a) => a -> Maybe Text -> [Text]
dockerCmdArgs x ntwkMb =
  let toNetworkArgs n = ["--network", n]
      networkArg = maybe mempty toNetworkArgs ntwkMb
   in ["run", "-d"] <> nameArg @a <> networkArg <> toRunCmd x <> [imageText' @a]


imageText' :: forall a. (Proc a) => Text
imageText' = Text.pack $ symbolVal (Proxy :: Proxy (Image a))


nameArg :: forall a. (Proc a) => [Text]
nameArg = ["--name", Text.pack $ symbolVal $ Proxy @(Name a)]


-- | The IP address of the docker host.
type HostIpAddress = Text


-- | A connection string used to access the service once its running.
type SvcURI = C8.ByteString


{- | Starts a 'Proc'.

It uses 'ping' to determine if the 'Proc' started up ok, and will fail by
throwing an exception if it did not.

Returns the 'ProcHandle' used to control the 'Proc' once a ping has succeeded.
-}
startup :: (ProcPlus a) => a -> IO (ProcHandle a)
startup = startup' Nothing mempty


-- | A @Constraint@ that combines @'Proc'@  and its supporting typeclasses
type ProcPlus a = (Proc a, ToRunCmd a, Preparer a)


startup' ::
  (ProcPlus a) =>
  Maybe Text ->
  [(Text, HostIpAddress)] ->
  a ->
  IO (ProcHandle a)
startup' ntwkMb addrs x = do
  let fullArgs = map Text.unpack $ dockerCmdArgs x ntwkMb
      isGarbage = flip elem ['\'', '\n']
      trim = dropWhileEnd isGarbage . dropWhile isGarbage
  printDebug $ Text.pack $ show fullArgs
  runCmd <- createDockerCmdProcess fullArgs
  x' <- prepare addrs x
  hPid <- trim <$> readCreateProcess runCmd ""
  hAddr <-
    Text.pack . trim
      <$> readProcess
        "docker"
        [ "inspect"
        , hPid
        , "--format"
        , "'{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}'"
        ]
        ""
  let h = ProcHandle {hProc = x', hPid, hUri = uriOf' x' hAddr, hAddr}
  (nPings h `onException` terminate h) >>= \case
    OK -> pure h
    pinged -> do
      terminate h
      fail $ pingedMsg x pinged


pingedMsg :: (Proc a) => a -> Pinged -> String
pingedMsg _ OK = ""
pingedMsg p NotOK = "tmp proc:" ++ Text.unpack (nameOf p) ++ ":could not be pinged"
pingedMsg p (PingFailed err) =
  "tmp proc:"
    ++ Text.unpack (nameOf p)
    ++ ":ping failed:"
    ++ Text.unpack err


-- | Use an action that might throw an exception as a ping.
toPinged :: forall e a. (Exception e) => Proxy e -> IO a -> IO Pinged
toPinged _ action = (action >> pure OK) `catch` (\(_ :: e) -> pure NotOK)


-- | Ping a 'ProcHandle' several times.
nPings :: (Proc a) => ProcHandle a -> IO Pinged
nPings h@ProcHandle {hProc = p} =
  let
    count = fromEnum $ pingCount' p
    gap = fromEnum $ pingGap' p

    badMsg x = "tmp.proc: could not start " <> nameOf p <> "; uncaught exception :" <> x
    badErr x = printDebug $ badMsg x

    lastMsg = "tmp.proc: could not start " <> nameOf p <> "; all pings failed"
    lastErr = printDebug lastMsg

    pingMsg i = "tmp.proc: ping #" <> Text.pack (show i) <> " failed; will retry"
    nthErr n = printDebug $ pingMsg $ count + 1 - n

    ping' x =
      ping x
        `catch` ( \(e :: SomeException) -> do
                    let errMsg = Text.pack $ show e
                    badErr errMsg
                    pure $ PingFailed errMsg
                )

    go n =
      ping' h >>= \case
        OK -> pure OK
        NotOK | n == 0 -> lastErr >> pure NotOK
        NotOK -> threadDelay gap >> nthErr n >> go (n - 1)
        err -> pure err
   in
    go count


{- | Constraint alias used to constrain types where proxy of a 'Proc' type looks up
  a value in an 'HList' of 'ProcHandle'.
-}
type HasHandle aProc procs =
  ( Proc aProc
  , AreProcs procs
  , IsInProof (ProcHandle aProc) (Proc2Handle procs)
  )


{- | Constraint alias used to constrain types where a 'Name' looks up
  a type in an 'HList' of 'ProcHandle'.
-}
type HasNamedHandle name a procs =
  ( name ~ Name a
  , Proc a
  , AreProcs procs
  , MemberKV name (ProcHandle a) (Handle2KV (Proc2Handle procs))
  )


-- | Run an action on a 'Connectable' handle as a callback on its 'Conn'
withTmpConn :: (Connectable a) => ProcHandle a -> (Conn a -> IO b) -> IO b
withTmpConn handle = bracket (openConn handle) closeConn


{- | Constraint alias when several @'Name's@ are used to find matching
 types in an 'HList' of 'ProcHandle'.
-}
type SomeNamedHandles names procs someProcs sortedProcs =
  ( names ~ Proc2Name procs
  , ManyMemberKV
      (SortSymbols names)
      (SortHandles (Proc2Handle procs))
      (Handle2KV (Proc2Handle sortedProcs))
  , ReorderH (SortHandles (Proc2Handle procs)) (Proc2Handle procs)
  , ReorderH (Proc2Handle someProcs) (Proc2Handle sortedProcs)
  , AreProcs sortedProcs
  , SortHandles (Proc2Handle someProcs) ~ Proc2Handle sortedProcs
  )


-- | Select the named @'ProcHandle's@ from an 'HList' of @'ProcHandle'@.
manyNamed ::
  (SomeNamedHandles names namedProcs someProcs sortedProcs) =>
  Proxy names ->
  HandlesOf someProcs ->
  HandlesOf namedProcs
manyNamed proxy xs = manyNamed' proxy $ toSortedKVs xs


manyNamed' ::
  forall (names :: [Symbol]) sortedNames (procs :: [Type]) (ordered :: [Type]) someProcs.
  ( names ~ Proc2Name procs
  , sortedNames ~ SortSymbols names
  , ordered ~ SortHandles (Proc2Handle procs)
  , ManyMemberKV sortedNames ordered (Handle2KV someProcs)
  , ReorderH ordered (Proc2Handle procs)
  ) =>
  Proxy names ->
  HList (Handle2KV someProcs) ->
  HandlesOf procs
manyNamed' _ kvs = unsortHandles $ selectMany @sortedNames @ordered kvs


-- | Specifies how to obtain a 'ProcHandle' that is present in an HList.
class HandleOf a procs b where
  -- | Obtain the handle matching the given type from a @'HList'@ of @'ProcHandle'@.
  handleOf :: Proxy a -> HandlesOf procs -> ProcHandle b


instance (HasHandle p procs) => HandleOf p procs p where
  handleOf _ = hOf @(ProcHandle p) Proxy


instance (HasNamedHandle name p procs) => HandleOf name procs p where
  handleOf _ xs = select @name @(ProcHandle p) $ toKVs xs


-- | Builds on 'handleOf'; gives the 'Conn' of the 'ProcHandle' to a callback.
withConnOf ::
  (HandleOf idx procs namedConn, Connectable namedConn) =>
  Proxy idx ->
  HandlesOf procs ->
  (Conn namedConn -> IO b) ->
  IO b
withConnOf proxy xs action = flip withTmpConn action $ handleOf proxy xs


-- | Specifies how to reset a 'ProcHandle' at an index in a list.
class IxReset a procs where
  -- | Resets the handle whose index is specified by the proxy type.
  ixReset :: Proxy a -> HandlesOf procs -> IO ()


instance (HasNamedHandle name a procs) => IxReset name procs where
  ixReset _ xs = reset $ select @name @(ProcHandle a) $ toKVs xs


instance (HasHandle p procs) => IxReset p procs where
  ixReset _ xs = reset $ hOf @(ProcHandle p) Proxy xs


-- | Specifies how to ping a 'ProcHandle' at an index in a list.
class IxPing a procs where
  -- | Pings the handle whose index is specified by the proxy type.
  ixPing :: Proxy a -> HandlesOf procs -> IO Pinged


instance (HasNamedHandle name a procs) => IxPing name procs where
  ixPing _ xs = ping $ select @name @(ProcHandle a) $ toKVs xs


instance (HasHandle p procs) => IxPing p procs where
  ixPing _ xs = ping $ hOf @(ProcHandle p) Proxy xs


-- | Specifies how to obtain the service URI a 'ProcHandle' at an index in a list.
class IxUriOf a procs where
  -- | Obtains the service URI of the handle whose index is specified by the proxy type.
  ixUriOf :: Proxy a -> HandlesOf procs -> SvcURI


instance (HasNamedHandle name a procs) => IxUriOf name procs where
  ixUriOf _ xs = hUri $ select @name @(ProcHandle a) $ toKVs xs


instance (HasHandle p procs) => IxUriOf p procs where
  ixUriOf _ xs = hUri $ hOf @(ProcHandle p) Proxy xs


-- | Create a 'HList' of @'KV's@ from a 'HList' of @'ProcHandle's@.
toKVs :: (handles ~ Proc2Handle xs, AreProcs xs) => HList handles -> HList (Handle2KV handles)
toKVs = go $ p2h procProof
  where
    go :: SomeHandles as -> HList as -> HList (Handle2KV as)
    go SomeHandlesNil HNil = HNil
    go (SomeHandlesCons cons) (x `HCons` y) = toKV x `HCons` go cons y


toSortedKVs ::
  ( handles ~ Proc2Handle someProcs
  , sorted ~ SortHandles handles
  , ReorderH handles sorted
  , AreProcs sortedProcs
  , Proc2Handle sortedProcs ~ sorted
  ) =>
  HList handles ->
  HList (Handle2KV sorted)
toSortedKVs procHandles = toKVs $ sortHandles procHandles


-- | Convert a 'ProcHandle' to a 'KV'.
toKV :: (Proc a) => ProcHandle a -> KV (Name a) (ProcHandle a)
toKV = V


-- | Converts list of types to the corresponding @'ProcHandle'@ types.
type family Proc2Handle (as :: [Type]) = (handleTys :: [Type]) | handleTys -> as where
  Proc2Handle '[] = '[]
  Proc2Handle (a ': as) = ProcHandle a ': Proc2Handle as


-- | A list of @'ProcHandle'@ values.
type HandlesOf procs = HList (Proc2Handle procs)


-- | A list of @'ProcHandle'@ values with the docker network of their processes
type NetworkHandlesOf procs = (Text, HandlesOf procs)


-- | Converts list of 'Proc' the corresponding @'Name'@ symbols.
type family Proc2Name (as :: [Type]) = (nameTys :: [Symbol]) | nameTys -> as where
  Proc2Name '[] = '[]
  Proc2Name (a ': as) = Name a ': Proc2Name as


-- | Convert list of 'ProcHandle' types to corresponding @'KV'@ types.
type family Handle2KV (ts :: [Type]) = (kvTys :: [Type]) | kvTys -> ts where
  Handle2KV '[] = '[]
  Handle2KV (ProcHandle t ': ts) = KV (Name t) (ProcHandle t) ': Handle2KV ts


-- | Declares a proof that a list of types only contains @'Proc's@.
class AreProcs as where
  procProof :: SomeProcs as


instance AreProcs '[] where
  procProof = SomeProcsNil


instance
  ( ProcPlus a
  , AreProcs as
  , IsAbsent a as
  ) =>
  AreProcs (a ': as)
  where
  procProof = SomeProcsCons procProof


-- | Used to prove a list of types just contains @'ProcHandle's@.
data SomeHandles (as :: [Type]) where
  SomeHandlesNil :: SomeHandles '[]
  SomeHandlesCons :: (ProcPlus a) => SomeHandles as -> SomeHandles (ProcHandle a ': as)


p2h :: SomeProcs as -> SomeHandles (Proc2Handle as)
p2h SomeProcsNil = SomeHandlesNil
p2h (SomeProcsCons cons) = SomeHandlesCons (p2h cons)


-- | Used to prove a list of types just contains @'Proc's@.
data SomeProcs (as :: [Type]) where
  SomeProcsNil :: SomeProcs '[]
  SomeProcsCons :: (ProcPlus a, AreProcs as, IsAbsent a as) => SomeProcs as -> SomeProcs (a ': as)


-- | Declares a proof that a list of types only contains @'Connectable's@.
class Connectables as where
  connProof :: Uniquely Connectable Connectables as


instance Connectables '[] where
  connProof = UniquelyNil


instance (Connectable a, Connectables as, IsAbsent a as) => Connectables (a ': as) where
  connProof = UniquelyCons connProof


-- | Convert list of 'Connectable' types to corresponding 'Conn' types.
type family ConnsOf (cs :: [Type]) = (conns :: [Type]) | conns -> cs where
  ConnsOf '[] = '[]
  ConnsOf (c ': cs) = Conn c ': ConnsOf cs


-- | Open all the 'Connectable' types to corresponding 'Conn' types.
openAll :: (Connectables xs) => HandlesOf xs -> IO (HList (ConnsOf xs))
openAll = go connProof
  where
    go :: Uniquely Connectable Connectables as -> HandlesOf as -> IO (HList (ConnsOf as))
    go UniquelyNil HNil = pure HNil
    go (UniquelyCons cons) (x `HCons` y) = do
      c <- openConn x
      others <- go cons y `onException` closeConn c
      pure $ c `HCons` others


-- | Close some 'Connectable' types.
closeAll :: (Connectables procs) => HList (ConnsOf procs) -> IO ()
closeAll = go connProof
  where
    go :: Uniquely Connectable Connectables as -> HList (ConnsOf as) -> IO ()
    go UniquelyNil HNil = pure ()
    go (UniquelyCons cons) (x `HCons` y) = closeConn x >> go cons y


-- | Open some connections, use them in an action; close them.
withConns ::
  (Connectables procs) =>
  HandlesOf procs ->
  (HList (ConnsOf procs) -> IO b) ->
  IO b
withConns handles = bracket (openAll handles) closeAll


-- | Open all known connections; use them in an action; close them.
withKnownConns ::
  ( AreProcs someProcs
  , Connectables conns
  , ReorderH (Proc2Handle someProcs) (Proc2Handle conns)
  ) =>
  HandlesOf someProcs ->
  (HList (ConnsOf conns) -> IO b) ->
  IO b
withKnownConns = withConns . hReorder


-- | Open the named connections; use them in an action; close them.
withNamedConns ::
  ( SomeNamedHandles names namedConns someProcs sortedProcs
  , Connectables namedConns
  ) =>
  Proxy names ->
  HandlesOf someProcs ->
  (HList (ConnsOf namedConns) -> IO b) ->
  IO b
withNamedConns proxy = withConns . manyNamed proxy


{- | Used to support type classes that prove a list of types is constrained to
unique instances of another type class.
-}
data Uniquely f fs (as :: [Type]) where
  UniquelyNil :: Uniquely f fs '[]
  UniquelyCons :: (IsAbsent a as, f a, fs as) => Uniquely f fs as -> Uniquely f fs (a ': as)


sortHandles ::
  ( handles ~ Proc2Handle ps
  , sorted ~ SortHandles handles
  , ReorderH handles sorted
  ) =>
  HList handles ->
  HList sorted
sortHandles = hReorder


unsortHandles ::
  ( sorted ~ SortHandles handles
  , handles ~ Proc2Handle ps
  , ReorderH sorted handles
  ) =>
  HList sorted ->
  HList handles
unsortHandles = hReorder


-- | Sort lists of @'ProcHandle'@ types.
type family SortHandles (xs :: [Type]) :: [Type] where
  SortHandles '[] = '[]
  SortHandles '[x] = '[x]
  SortHandles '[x, y] = MergeHandles '[x] '[y] -- just an optimization, not required
  SortHandles xs = SortHandlesStep xs (HalfOf (LengthOf xs))


type family SortHandlesStep (xs :: [Type]) (halfLen :: Nat) :: [Type] where
  SortHandlesStep xs halfLen = MergeHandles (SortHandles (Take xs halfLen)) (SortHandles (Drop xs halfLen))


type family MergeHandles (xs :: [Type]) (ys :: [Type]) :: [Type] where
  MergeHandles xs '[] = xs
  MergeHandles '[] ys = ys
  MergeHandles (ProcHandle x ': xs) (ProcHandle y ': ys) =
    MergeHandlesImpl (ProcHandle x ': xs) (ProcHandle y ': ys) (CmpSymbol (Name x) (Name y))


type family MergeHandlesImpl (xs :: [Type]) (ys :: [Type]) (o :: Ordering) :: [Type] where
  MergeHandlesImpl (ProcHandle x ': xs) (ProcHandle y ': ys) 'GT =
    ProcHandle y ': MergeHandles (ProcHandle x ': xs) ys
  MergeHandlesImpl (ProcHandle x ': xs) (ProcHandle y ': ys) leq =
    ProcHandle x ': MergeHandles xs (ProcHandle y ': ys)


devNull :: IO Handle
devNull = openBinaryFile "/dev/null" WriteMode


createDockerCmdProcess :: [String] -> IO CreateProcess
createDockerCmdProcess args = do
  devNull' <- devNull
  pure $ (proc "docker" args) {std_err = UseHandle devNull'}


showDebug :: IO Bool
showDebug = isJust <$> lookupEnv debugEnv


debugEnv :: String
debugEnv = "TMP_PROC_DEBUG"


printDebug :: Text -> IO ()
printDebug t = do
  canPrint <- showDebug
  when canPrint $ Text.hPutStrLn stderr t


-- | generate a random network name
genNetworkName :: IO Text
genNetworkName = networkNameOf <$> randomIO


networkNameOf :: Word16 -> Text
networkNameOf suffix = "tmp-proc-" +| suffix |+ ""


createNetworkArgs :: String -> [String]
createNetworkArgs name = ["network", "create", "-d", "bridge", name]


removeNetworkArgs :: String -> [String]
removeNetworkArgs name = ["network", "remove", "-f", name]
