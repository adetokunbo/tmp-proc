{-# LANGUAGE LambdaCase #-}
module TmpProc.Example2.Cache
  ( -- * Cache services
    deleteContact
  , loadContact
  , saveContact
  , runRedisAction

    -- * Redis Connection
  , Connection
  , defaultConn
  )
where

import           Control.Monad (void)
import           Data.ByteString.Char8 (pack, unpack, ByteString)
import           Database.Redis

import           TmpProc.Example2.Schema

{-| A default for local development .-}
defaultConn :: IO Connection
defaultConn = connect defaultConnectInfo

runRedisAction :: Connection -> Redis a -> IO a
runRedisAction loc action = runRedis loc action

saveContact :: Connection -> ContactID -> Contact -> IO ()
saveContact loc cid contact = runRedisAction loc $ void $ setex (toKey cid) 3600 (pack . show $ contact)

loadContact :: Connection -> ContactID -> IO (Maybe Contact)
loadContact loc cid = runRedisAction loc $ do
  (get $ toKey cid) >>= \case
    Right (Just contactString) -> return $ Just (read . unpack $ contactString)
    _ -> return Nothing

deleteContact :: Connection -> ContactID -> IO ()
deleteContact loc cid = do
  runRedis loc $ do
    _ <- del [pack . show $ cid]
    return ()


toKey :: ContactID -> ByteString
toKey = pack . show
