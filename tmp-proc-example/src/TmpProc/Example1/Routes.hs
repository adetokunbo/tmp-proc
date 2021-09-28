{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Specifies the API provided by the demo service.

-}
module TmpProc.Example1.Routes
  ( -- * API route definition
    ContactsAPI
  , contactsAPI
  )
where

import           Data.Proxy (Proxy(..))
import           Servant.API

import           TmpProc.Example1.Schema


{-| API allowing 'Contact' creation and retrieval. -}
type ContactsAPI =
       "contacts" :> Capture "contactid" ContactID :> Get '[JSON] Contact
  :<|> "contacts" :> ReqBody '[JSON] Contact :> Post '[JSON] ContactID


{-| For convenience in "Servant" combinators where a proxy is required. -}
contactsAPI :: Proxy ContactsAPI
contactsAPI = Proxy :: Proxy ContactsAPI
