{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Docker.TmpProc.Redis
  ( -- * functions
    clearKeys
  , mkTmpProc
  , mkNoResetProc
  , withConnectionFrom

    -- * type aliases
  , KeyName

  -- * re-exports
  , module System.Docker.TmpProc
  ) where


import           Control.Exception     (bracket)
import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C8
import           Data.Text             (Text)
import           Database.Redis        (Connection, checkedConnect, del,
                                        disconnect, parseConnectInfo, runRedis)
import           System.Docker.TmpProc


-- | The name of a key in redis.
type KeyName = C8.ByteString


-- | A cleanup function that clears all fields with the given key names.
clearKeys :: [KeyName] -> ProcURI -> IO ()
clearKeys keys rdsUri = withConnectionFrom rdsUri $ \c ->
  runRedis c $ void $ del keys


-- | A @TmpProc@ that runs the default redis version with a reset action that
-- deletes some keys.
mkTmpProc :: [KeyName] -> TmpProc
mkTmpProc keys = mkNoResetProc { procReset = clearKeys keys }


-- | A @TmpProc@ that runs the default redis version without any configured
-- reset action.
mkNoResetProc :: TmpProc
mkNoResetProc = TmpProc
  { procImageName = rdsImageName
  , procRunArgs = []
  , procMkUri = rdsMkUri
  , procReset = doNothing
  , procPing = flip withConnectionFrom (const $ pure ())
  }


withConnectionFrom :: ProcURI -> (Connection -> IO()) -> IO()
withConnectionFrom uri action = case parseConnectInfo $ C8.unpack uri of
  Left e  -> error e
  Right x -> bracket (checkedConnect x) disconnect action


-- | The default image to use when launching the redis @TmpProc@.
rdsImageName :: Text
rdsImageName = "redis:5.0"


-- | Makes redis connection uri for connecting to the redis @TmpProc@.
rdsMkUri :: DockerIpAddress -> ProcURI
rdsMkUri ip = "redis://" <> C8.pack ip <> "/"
