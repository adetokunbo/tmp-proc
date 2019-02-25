{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.SimpleServer (mkTestApp, statusOfGet) where

import           Data.List                (foldl')

import           Data.Default             (def)
import           Data.Text                (Text)
import qualified Data.Text                as Text

import           Network.HTTP.Req
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import           System.Docker.TmpProc


-- | A WAI app that triggers an action on a TmpProc dependency on /test, and
-- responds to health checks on /health.
mkTestApp
  :: (OwnerHandle -> IO())
  -> (OwnerHandle -> IO())
  -> OwnerHandle
  -> IO Application
mkTestApp onStart onTest h = onStart h >> pure app
  where
    app rq respond
      | isHealthReq rq = (respond $ responseLBS status200 [] "ok")
    app rq respond
      | isTestReq rq = onTest h >> (respond $ responseLBS status200 [] "ok")
    app _ respond = respond $ responseLBS status400 [] "Incorrect request"

    isHealthReq = isReqPathsEq ["health"]
    isTestReq = isReqPathsEq ["test"]
    isReqPathsEq x rq = x == pathInfo rq


-- | Determine the status from a Get on localhost.
statusOfGet :: Warp.Port -> Text -> IO Int
statusOfGet p path = runReq def $ do
  r <- req GET (localUrl path) NoReqBody ignoreResponse $ port p
  return $ responseStatusCode r


localUrl :: Text -> Url 'Http
localUrl p = foldl' (/:) (http "localhost")
  $ Text.splitOn "/" $ Text.dropWhile (== '/') p
