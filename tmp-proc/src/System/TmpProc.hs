{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Exports all tmp-proc behaviour.

@tmp-proc@ is a package that aims to simplify writing integration tests that use
dockerizable services.

The package has the following structure:

* __"System.TmpProc.Docker":__ definition of core data types, @Proc@ and @ProcHandle@, and their combinators.
* __"System.TmpProc.Warp":__  functions that make it easy to test a WAI @application@ that accesses  @tmp@ @procs@

-}
module System.TmpProc
  ( module System.TmpProc.Docker
  , module System.TmpProc.Warp
  ) where

import System.TmpProc.Docker
import System.TmpProc.Warp
