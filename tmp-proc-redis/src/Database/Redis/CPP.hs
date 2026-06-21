{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Network.Connection.CPP
Copyright   : (c) 2026 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Database.Redis.CPP (del) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as C8
import qualified Database.Redis as Redis


#if MIN_VERSION_hedis(0,16,0)
import Data.List.NonEmpty (NonEmpty ((:|)))
#endif

#if MIN_VERSION_hedis(0,16,0)
del :: Redis.RedisCtx m f => [C8.ByteString] -> m ()
del [] = pure ()
del (x : xs) = void $ Redis.del (x :| xs)
#else
del :: Redis.RedisCtx m f => [C8.ByteString] -> m ()
del [] = pure ()
del xs = void $ Redis.del xs
#endif
