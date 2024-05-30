{-# LANGUAGE CPP #-}

{- |
Module      : Network.Connection.CPP
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Network.Connection.CPP (noCheckSettings) where

import Network.Connection (TLSSettings (..))


#if MIN_VERSION_crypton_connection(0,4,0)
import Data.Default (def)
#endif

#if MIN_VERSION_crypton_connection(0,4,0)
noCheckSettings :: TLSSettings
noCheckSettings = TLSSettingsSimple True False False def
#else
noCheckSettings :: TLSSettings
noCheckSettings = TLSSettingsSimple True False False
#endif
