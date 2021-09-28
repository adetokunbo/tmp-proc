{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TmpProc.Example1.Client
  ( -- * Client combinators
    fetch
  , create
  ) where

import           Servant.API             ((:<|>) (..))
import           Servant.Client          (ClientM, client)

import           TmpProc.Example1.Routes (contactsAPI)
import           TmpProc.Example1.Schema (Contact, ContactID)

{-| Fetch a client via the API, -}
fetch :: ContactID -> ClientM Contact

{-| Create a client via the API, -}
create :: Contact -> ClientM ContactID
(fetch :<|> create) = client contactsAPI
