{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Provides the core data types and combinators used to launch temporary /(tmp)/
processes /(procs)/ using docker.

@tmp-proc@ aims to simplify integration tests that use dockerizable services.

* Basically, @tmp-proc@ helps launch services used in integration test on docker

* Obviously, it's possible to write integration tests that use services hosted
  on docker /without/ @tmp-proc@

* However, @tmp-proc@ aims to make writing those kind of tests easier, by providing
  types and combinators to take care of

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
    Proc(..)
  , Pinged(..)
  , AreProcs
  , SomeProcs(..)
  , nameOf
  , startup
  , toPinged
  , uriOf'
  , runArgs'

    -- * @'ProcHandle'@
  , ProcHandle(..)
  , Proc2Handle
  , HandlesOf
  , startupAll
  , terminateAll
  , withTmpProcs
  , manyNamed
  , handleOf
  , ixReset
  , ixPing
  , ixUriOf
  , HasNamedHandle
  , SomeNamedHandles

    -- * @'Connectable'@
  , Connectable(..)
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

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException, bracket, catch,
                                           onException, Exception)
import           Control.Monad            (void)
import qualified Data.ByteString.Char8    as C8
import           Data.Kind                (Type)
import           Data.List                (dropWhileEnd)
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           GHC.TypeLits             (CmpSymbol, KnownSymbol, Nat, Symbol,
                                           symbolVal)
import           Numeric.Natural          (Natural)
import           System.Exit              (ExitCode (..))
import           System.IO                (stderr)
import           System.Process           (StdStream (..), proc, readProcess,
                                           std_err, std_out, waitForProcess,
                                           withCreateProcess)

import           System.TmpProc.TypeLevel (Drop, HList (..), HalfOf, IsAbsent,
                                           IsInProof, KV (..), LengthOf,
                                           ManyMemberKV, MemberKV,
                                           ReorderH (..), SortSymbols, Take,
                                           hOf, select, selectMany, (%:))


{-| Determines if the docker daemon is accessible. -}
hasDocker :: IO Bool
hasDocker = do
  let rawSystemNoStdout cmd args = withCreateProcess
        (proc cmd args) { std_out = CreatePipe , std_err = CreatePipe }
        (\_ _ _ -> waitForProcess)
        `catch`
        (\(_ :: IOError) -> return (ExitFailure 127))
      succeeds ExitSuccess = True
      succeeds _           = False

  succeeds <$> rawSystemNoStdout "docker" ["ps"]


{-| Set up some @'Proc's@, run an action that uses them, then terminate them. -}
withTmpProcs
  :: AreProcs procs
  => HList procs
  -> (HandlesOf procs -> IO b)
  -> IO b
withTmpProcs procs = bracket (startupAll procs) terminateAll


{-| Provides access to a 'Proc' that has been started. -}
data ProcHandle a = ProcHandle
  { hProc :: !a
  , hPid  :: !String
  , hUri  :: !SvcURI
  , hAddr :: !HostIpAddress
  }


{-| Start up processes for each 'Proc' type. -}
startupAll :: AreProcs procs => HList procs -> IO (HandlesOf procs)
startupAll = go procProof
  where
    go :: SomeProcs as -> HList as -> IO (HandlesOf as)
    go SomeProcsNil HNil = pure HNil
    go (SomeProcsCons cons) (x `HCons` y) = do
      h <- startup x
      others <- go cons y `onException` terminate h
      pure $ h `HCons` others


{-| Terminate all processes owned by some @'ProcHandle's@. -}
terminateAll :: AreProcs procs => HandlesOf procs -> IO ()
terminateAll = go $ p2h procProof
  where
    go :: SomeHandles as -> HList as -> IO ()
    go SomeHandlesNil HNil = pure ()
    go (SomeHandlesCons cons) (x `HCons` y) = do
      terminate x
      go cons y


{-| Terminate the process owned by a @'ProcHandle's@. -}
terminate :: ProcHandle p -> IO ()
terminate handle = do
  let pid = hPid handle
  void $ readProcess "docker" [ "stop", pid ] ""
  void $ readProcess "docker" [ "rm", pid ] ""


{-| Specifies how to a get a connection to a 'Proc'. -}
class Proc a => Connectable a where
  {-| The connection type. -}
  type Conn a = (conn :: *) | conn -> a

  {-| Get a connection to the Proc via its 'ProcHandle', -}
  openConn :: ProcHandle a -> IO (Conn a)

  {-| Close a connection to a 'Proc', -}
  closeConn :: Conn a -> IO ()
  closeConn = const $ pure ()


{-| Specifies how to launch a temporary process using Docker. -}
class (KnownSymbol (Image a), KnownSymbol (Name a)) => Proc a where
  {-| The image name of the docker image, e.g, /postgres:10.6/ -}
  type Image a :: Symbol

  {-| A label used to refer to running process created from this image, e.g,
  /a-postgres-db/ -}
  type Name a = (labelName :: Symbol) | labelName -> a

  {-| Additional arguments to the docker command that launches the tmp proc. -}
  runArgs :: [Text]
  runArgs = mempty

  {-| Determines the service URI of the process, when applicable. -}
  uriOf :: HostIpAddress -> SvcURI

  {-| Resets some state in a tmp proc service. -}
  reset :: ProcHandle a -> IO ()

  {-| Checks if the tmp proc started ok.  -}
  ping :: ProcHandle a -> IO Pinged

  {-| Maximum number of pings to perform during startup. -}
  pingCount :: Natural
  pingCount = 4

  {-| Number of milliseconds between pings. -}
  pingGap :: Natural
  pingGap = 1000000


{-| Indicates the result of pinging a 'Proc'.

If the ping succeeds, 'ping2' should return 'OK'.

'ping2' should catch any exceptions that are expected when the @'Proc's@ service
is not available and return 'NotOK'.

'startupAll' uses 'PingFailed' to report any unexpected exceptions that escape
'ping2'.

-}
data Pinged =
  {-| The service is running OK. -}
  OK

  {-| The service is not running. -}
  | NotOK

  {-| Contact to the service failed unexpectedly. -}
  | PingFailed Text

  deriving (Eq, Show)

{-| Name of a process. -}
nameOf :: forall a . (Proc a) => a -> Text
nameOf _  = Text.pack $ symbolVal (Proxy :: Proxy (Name a))


{-| Simplifies use of 'runArgs'. -}
runArgs' :: forall a . (Proc a) => a -> [Text]
runArgs' _  = runArgs @a


{-| Simplifies use of 'pingCount'. -}
pingCount' :: forall a . (Proc a) => a -> Natural
pingCount' _  = pingCount @a


{-| Simplifies use of 'pingGap'. -}
pingGap' :: forall a . (Proc a) => a -> Natural
pingGap' _  = pingGap @a


{-| Simplifies use of 'uriOf'. -}
uriOf' :: forall a . (Proc a) => a -> HostIpAddress -> SvcURI
uriOf' _ addr  = uriOf @a addr


{-| The full args of a @docker run@ command for starting up a 'Proc'. -}
dockerCmdArgs :: forall a . (Proc a) => [Text]
dockerCmdArgs = [
  "run"
  , "-d"
  ]
  <> runArgs @a
  <> [imageText' @a]


imageText' :: forall a . (Proc a) => Text
imageText' = Text.pack $ symbolVal (Proxy :: Proxy (Image a))


-- | The IP address of the docker host.
type HostIpAddress = Text


-- | A connection string used to access the service once its running.
type SvcURI = C8.ByteString


{-| Starts a 'Proc'.

It uses 'ping' to determine if the 'Proc' started up ok, and will fail by
throwing an exception if it did not.

Returns the 'ProcHandle' used to control the 'Proc' once a ping has succeeded.

-}
startup :: forall a . Proc a => a -> IO (ProcHandle a)
startup x = do
  let fullArgs = dockerCmdArgs @a
      isGarbage = flip elem ['\'', '\n']
      trim = dropWhileEnd isGarbage . dropWhile isGarbage
  hPid <- trim <$> readProcess "docker" (map Text.unpack fullArgs) ""
  hAddr <- (Text.pack . trim) <$> readProcess "docker"
    [ "inspect"
    , hPid
    , "--format"
    , "'{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}'"
    ] ""
  let hUri = uriOf @a hAddr
      h = ProcHandle {hProc=x, hPid, hUri, hAddr }
  (nPings h `onException` terminate h) >>= \case
    OK     -> pure h
    pinged -> fail $ pingedMsg x pinged


pingedMsg :: Proc a => a -> Pinged -> String
pingedMsg _ OK = ""
pingedMsg p NotOK = "tmp proc:" ++ (Text.unpack $ nameOf p) ++ ":could not be pinged"
pingedMsg p (PingFailed err) = "tmp proc:"
  ++ (Text.unpack $ nameOf p)
  ++ ":ping failed"
  ++ (Text.unpack err)


{-| Use an action that might throw an exception as a ping. -}
toPinged :: forall e a . Exception e => Proxy e -> IO a -> IO Pinged
toPinged _ action = (action >> pure OK) `catch` (\(_ :: e) -> pure NotOK)


{-| Ping a 'ProcHandle' several times. -}
nPings :: Proc a => ProcHandle a -> IO Pinged
nPings h@ProcHandle{hProc = p} =
  let
    count  = fromEnum $ pingCount' p
    gap    = fromEnum $ pingGap' p

    badMsg x = "tmp.proc: could not start " <> nameOf p <> "; uncaught exception :" <> x
    badErr x = Text.hPutStrLn stderr $ badMsg x

    lastMsg = "tmp.proc: could not start " <> nameOf p <> "; all pings failed"
    lastErr  = Text.hPutStrLn stderr lastMsg

    pingMsg i = "tmp.proc: ping #" <> (Text.pack $ show i) <> " failed; will retry"
    nthErr n  = Text.hPutStrLn stderr $ pingMsg $ count + 1 - n

    ping' x  = ping x `catch` (\(e :: SomeException) -> do
                                    let errMsg = Text.pack $ show e
                                    badErr errMsg
                                    pure $ PingFailed errMsg)

    go n = ping' h >>= \case
      OK             -> pure OK
      NotOK | n == 0 -> lastErr >> pure NotOK
      NotOK          -> threadDelay gap >> nthErr n >> go (n - 1)
      err            -> pure err

  in
    go count


{-| Constraint alias used to constrain types where proxy of a 'Proc' type looks up
  a value in an 'HList' of 'ProcHandle'.
-}
type HasHandle aProc procs =
  ( Proc aProc
  , IsInProof (ProcHandle aProc) (Proc2Handle procs)
  )


{-| Constraint alias used to constrain types where a 'Name' looks up
  a type in an 'HList' of 'ProcHandle'.
-}
type HasNamedHandle name a procs =
  ( name ~ Name a
  , Proc a
  , AreProcs procs
  , MemberKV name (ProcHandle a) (Handle2KV (Proc2Handle procs))
  )


{-| Run an action on a 'Connectable' handle as a callback on its 'Conn' -}
withTmpConn :: Connectable a => ProcHandle a -> (Conn a -> IO b) -> IO b
withTmpConn handle action = bracket (openConn handle) closeConn action


{-| Constraint alias when several @'Name's@ are used to find matching
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


{-| Select the named @'ProcHandle's@ from an 'HList' of @'ProcHandle'@. -}
manyNamed
  :: SomeNamedHandles names namedProcs someProcs sortedProcs
  => Proxy names -> HandlesOf someProcs -> HandlesOf namedProcs
manyNamed proxy xs = manyNamed' proxy $ toSortedKVs xs


manyNamed'
  :: forall (names :: [Symbol]) sortedNames (procs :: [*]) (ordered :: [*]) someProcs.
     ( names ~ Proc2Name procs
     , sortedNames ~ SortSymbols names
     , ordered ~ SortHandles (Proc2Handle procs)
     , ManyMemberKV sortedNames ordered (Handle2KV someProcs)
     , ReorderH ordered (Proc2Handle procs)
     )
  => Proxy names -> HList (Handle2KV someProcs) -> HandlesOf procs
manyNamed' _ kvs = unsortHandles $ selectMany @sortedNames @ordered kvs


{-| Specifies how to obtain a 'ProcHandle' that is present in an HList.  -}
class HandleOf a procs b where

  {-| Obtain the handle matching the given type from a @'HList'@ of @'ProcHandle'@. -}
  handleOf :: Proxy a -> HandlesOf procs -> ProcHandle b

instance (HasHandle p procs) => HandleOf p procs p where
  handleOf _ procs = hOf @(ProcHandle p) Proxy procs

instance (HasNamedHandle name p procs) => HandleOf name procs p where
  handleOf _ xs = select @name @(ProcHandle p) $ toKVs xs


{-| Builds on 'handleOf'; gives the 'Conn' of the 'ProcHandle' to a callback. -}
withConnOf
  :: (HandleOf idx procs namedConn, Connectable namedConn)
  => Proxy idx -> HandlesOf procs -> (Conn namedConn -> IO b) -> IO b
withConnOf proxy xs action = flip withTmpConn action $ handleOf proxy xs


{-| Specifies how to reset a 'ProcHandle' at an index in a list.  -}
class IxReset a procs where

  {-| Resets the handle whose index is specified by the proxy type. -}
  ixReset :: Proxy a -> HandlesOf procs -> IO ()

instance (HasNamedHandle name a procs) => IxReset name procs where
  ixReset _  xs = reset $ select @name @(ProcHandle a) $ toKVs xs

instance (HasHandle p procs) => IxReset p procs where
  ixReset _ xs = reset $ hOf @(ProcHandle p) Proxy xs


{-| Specifies how to ping a 'ProcHandle' at an index in a list.  -}
class IxPing a procs where

  {-| Pings the handle whose index is specified by the proxy type. -}
  ixPing :: Proxy a -> HandlesOf procs -> IO Pinged

instance (HasNamedHandle name a procs) => IxPing name procs where
  ixPing _  xs = ping $ select @name @(ProcHandle a) $ toKVs xs

instance (HasHandle p procs) => IxPing p procs where
  ixPing _ xs = ping $ hOf @(ProcHandle p) Proxy xs


{-| Specifies how to obtain the service URI a 'ProcHandle' at an index in a list.  -}
class IxUriOf a procs where

  {-| Obtains the service URI of the handle whose index is specified by the proxy type. -}
  ixUriOf :: Proxy a -> HandlesOf procs -> SvcURI

instance (HasNamedHandle name a procs) => IxUriOf name procs where
  ixUriOf _  xs = hUri $ select @name @(ProcHandle a) $ toKVs xs

instance (HasHandle p procs) => IxUriOf p procs where
  ixUriOf _ xs = hUri $ hOf @(ProcHandle p) Proxy xs


{-| Create a 'HList' of @'KV's@ from a 'HList' of @'ProcHandle's@. -}
toKVs :: (handles ~ Proc2Handle xs, AreProcs xs) => HList handles -> HList (Handle2KV handles)
toKVs = go $ p2h procProof
  where
    go :: SomeHandles as -> HList as -> HList (Handle2KV as)
    go SomeHandlesNil         HNil          = HNil
    go (SomeHandlesCons cons) (x `HCons` y) = toKV x `HCons` go cons y


toSortedKVs
  :: ( handles ~ Proc2Handle someProcs
     , sorted ~ SortHandles handles
     , ReorderH handles sorted
     , AreProcs sortedProcs
     , Proc2Handle sortedProcs ~ sorted
     )
  => HList handles -> HList (Handle2KV sorted)
toSortedKVs procHandles = toKVs $ sortHandles procHandles


{-| Convert a 'ProcHandle' to a 'KV'. -}
toKV :: Proc a => ProcHandle a -> KV (Name a) (ProcHandle a)
toKV h = V h


{-| Converts list of types to the corresponding @'ProcHandle'@ types. -}
type family Proc2Handle (as :: [*]) = (handleTys :: [*]) | handleTys -> as where
  Proc2Handle '[]        = '[]
  Proc2Handle (a ':  as) = ProcHandle a ': Proc2Handle as


{-| A list of @'ProcHandle'@ values. -}
type HandlesOf procs = HList (Proc2Handle procs)


{-| Converts list of 'Proc' the corresponding @'Name'@ symbols. -}
type family Proc2Name (as :: [*]) = (nameTys :: [Symbol]) | nameTys -> as where
  Proc2Name '[]          = '[]
  Proc2Name (a ':  as)   = Name a ': Proc2Name as


{-| Convert list of 'ProcHandle' types to corresponding @'KV'@ types. -}
type family Handle2KV (ts :: [*]) = (kvTys :: [*]) | kvTys -> ts where
  Handle2KV '[]                   = '[]
  Handle2KV (ProcHandle t ':  ts) = KV (Name t) (ProcHandle t) ': Handle2KV ts


{-| Used by @'AreProcs'@ to prove a list of types just contains @'Proc's@. -}
data SomeProcs (as :: [*]) where
  SomeProcsNil  :: SomeProcs '[]
  SomeProcsCons :: (Proc a, IsAbsent a as) => SomeProcs as -> SomeProcs (a ': as)


{-| Declares a proof that a list of types only contains @'Proc's@. -}
class AreProcs as where
  procProof :: SomeProcs as

instance AreProcs '[] where
  procProof = SomeProcsNil

instance (Proc a, AreProcs as, IsAbsent a as) => AreProcs (a ': as) where
  procProof = SomeProcsCons procProof


{-| Used to prove a list of types just contains @'ProcHandle's@. -}
data SomeHandles (as :: [*]) where
  SomeHandlesNil  :: SomeHandles '[]
  SomeHandlesCons :: Proc a => SomeHandles as -> SomeHandles (ProcHandle a ': as)


p2h :: SomeProcs as -> SomeHandles (Proc2Handle as)
p2h SomeProcsNil         = SomeHandlesNil
p2h (SomeProcsCons cons) = SomeHandlesCons (p2h cons)


{-| Used by @'Connectables'@ to prove a list of types just contains @'Connectable's@. -}
data SomeConns (as :: [*]) where
  SomeConnsNil  :: SomeConns '[]
  SomeConnsCons :: (Connectable a, IsAbsent a as) => SomeConns as -> SomeConns (a ': as)


{-| Declares a proof that a list of types only contains @'Connectable's@. -}
class Connectables as where
  connProof :: SomeConns as

instance Connectables '[] where
  connProof = SomeConnsNil

instance (Connectable a, Connectables as, IsAbsent a as) => Connectables (a ': as) where
  connProof = SomeConnsCons connProof


{-| Convert list of 'Connectable' types to corresponding 'Conn' types. -}
type family ConnsOf (cs :: [*]) = (conns :: [*]) | conns -> cs where
  ConnsOf '[]        = '[]
  ConnsOf (c ':  cs) = Conn c ': ConnsOf cs


{-| Open all the 'Connectable' types to corresponding 'Conn' types. -}
openAll :: Connectables xs => HandlesOf xs -> IO (HList (ConnsOf xs))
openAll =  go connProof
  where
    go :: SomeConns as -> HandlesOf as -> IO (HList (ConnsOf as))
    go SomeConnsNil HNil = pure HNil
    go (SomeConnsCons cons) (x `HCons` y) = do
      c <- openConn x
      others <- go cons y `onException` closeConn c
      pure $ c `HCons` others


{-| Close some 'Connectable' types. -}
closeAll :: Connectables procs => HList (ConnsOf procs) -> IO ()
closeAll = go connProof
  where
    go :: SomeConns as -> HList (ConnsOf as) -> IO ()
    go SomeConnsNil HNil                  = pure ()
    go (SomeConnsCons cons) (x `HCons` y) = closeConn x >> go cons y


{-| Open some connections, use them in an action; close them. -}
withConns
  :: Connectables procs
  => HandlesOf procs
  -> (HList (ConnsOf procs) -> IO b)
  -> IO b
withConns handles = bracket (openAll handles) closeAll


{-| Open all known connections; use them in an action; close them. -}
withKnownConns
  :: (AreProcs someProcs,
      Connectables conns,
      ReorderH (Proc2Handle someProcs) (Proc2Handle conns)
     )
  => HandlesOf someProcs
  -> (HList (ConnsOf conns) -> IO b)
  -> IO b
withKnownConns = withConns . hReorder


{-| Open the named connections; use them in an action; close them. -}
withNamedConns
  :: ( SomeNamedHandles names namedConns someProcs sortedProcs
     , Connectables namedConns
     )
  => Proxy names
  -> HandlesOf someProcs
  -> (HList (ConnsOf namedConns) -> IO b)
  -> IO b
withNamedConns proxy = withConns . manyNamed proxy


sortHandles
  :: ( handles ~ Proc2Handle ps
     , sorted ~ SortHandles (handles)
     , ReorderH handles sorted
     )
  => HList handles -> HList sorted
sortHandles = hReorder


unsortHandles
  :: ( sorted ~ SortHandles (handles)
     , handles ~ Proc2Handle ps
     , ReorderH sorted handles
     )
  => HList sorted -> HList handles
unsortHandles = hReorder


{-| Sort lists of @'ProcHandle'@ types. -}
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
