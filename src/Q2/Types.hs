{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Q2.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Sqlite
import qualified Database.Beam.Sqlite.Migrate as Sqlite
import Database.SQLite.Simple

data UserT f = User
  { _userId :: Columnar f Text,
    _userEmail :: Columnar f Text,
    _userFirstName :: Columnar f Text,
    _userLastName :: Columnar f Text,
    _userPassword :: Columnar f Text,
    _userName :: Columnar f (Maybe Text),
    _userIsRegistered :: Columnar f Bool,
    _userRegion :: Columnar f T.Text
  }
  deriving (Generic, Beamable)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserId . _userId

data Q2UserDb f = Q2UserDb
  { _q2User :: f (TableEntity UserT)
  }
  deriving (Generic, Database Sqlite)

q2UserDb :: CheckedDatabaseSettings Sqlite Q2UserDb
q2UserDb = defaultMigratableDbSettings

instance FromJSON User

instance ToJSON User

{-
instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .: "id"
    <*> v .: "email"
    <*> v .: "firstName"
    <*> v .: "lastName"
    <*> v .: "password"
    <*> v .: "name"
    <*> v .: "isRegistered"
    <*> v .: "Region"
-}
