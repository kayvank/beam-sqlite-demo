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
    _userRegion :: Columnar f T.Text

  }
  deriving (Generic, Beamable)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show User

instance Eq User where
  (User userId1 _ _ _ _ _ _) == (User userId2 _ _ _ _ _ _) = userId1 == userId2

instance Ord User where
  (User userId1 _ _ _ _ _ _) `compare` (User userId2 _ _ _ _ _ _) = userId1 `compare` userId2

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserId . _userId

data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers :: f (TableEntity UserT)
  }
  deriving (Generic, Database Sqlite)

-- shoppingCartDb :: DatabaseSettings be ShoppingCartDb
-- shoppingCartDb = defaultDbSettings

shoppingCartDb :: CheckedDatabaseSettings Sqlite ShoppingCartDb
shoppingCartDb = defaultMigratableDbSettings
