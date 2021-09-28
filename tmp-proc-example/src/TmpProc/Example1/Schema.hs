{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-|
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

Specifies the schema of the data accessed by the demo service.

-}
module TmpProc.Example1.Schema

where

import           Data.Int (Int64)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text           (Text)
import qualified Database.Persist.TH as P

type ContactID = Int64

P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [P.persistLowerCase|
  Contact sql=contacts
    email Text
    name Text
    age Int
    title Text
    UniqueEmail email
    deriving Show Read
|]

instance ToJSON Contact where
  toJSON Contact {contactEmail, contactName, contactAge, contactTitle } = object
    [ "email" .= contactEmail
    , "name" .= contactName
    , "age" .= contactAge
    , "title" .= contactTitle
    ]

instance FromJSON Contact where
  parseJSON = withObject "Contact" parseContact

parseContact :: Object -> Parser Contact
parseContact o = do
  contactName <- o .: "name"
  contactEmail <- o .: "email"
  contactAge <- o .: "age"
  contactTitle <- o .: "title"
  return Contact
    { contactName
    , contactEmail
    , contactAge
    , contactTitle
    }
