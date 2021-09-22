{-# OPTIONS_HADDOCK prune not-home #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com >

Simplify use of @tmp-proc@ with @hspec@.

-}
module Test.Hspec.TmpProc
  ( -- * Combinators
    tdescribe

    -- * Re-export
  , module System.TmpProc
  )
where

import           System.TmpProc
import           Test.Hspec


{-| Like 'describe', but makes the specs @pending@ if @docker@ is unavailable. -}
tdescribe :: HasCallStack => String -> SpecWith a -> SpecWith a
tdescribe label action = do
  noDocker <- not <$> runIO hasDocker
  if noDocker then tmpPending label action else describe label action


tmpPending :: HasCallStack => String -> SpecWith a -> SpecWith a
tmpPending label spec = before_ (pendingWith noDockerMessage) $ describe label spec


noDockerMessage :: String
noDockerMessage = "docker could not be detected"
