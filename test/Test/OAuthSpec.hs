{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.OAuthSpec where

import Control.Exception hiding (assert)
import Data.List hiding (insert)
import Data.Ord
import qualified Data.Text as T
import Data.Time
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import qualified Database.Beam.Sqlite.Migrate as Sqlite
import Database.SQLite.Simple
import GHC.Generics
import Generic.Random
import Q2.Types
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Text.Printf
import Test.Utils


withTestDb :: (Connection -> IO a) -> IO a
withTestDb = handle asFailure . bracket (open ":memory:") close
  where
    asFailure (e :: SomeException) = assertFailure $ show e

testTree :: TestTree
testTree =
  testGroup
    "Simple beam generative test "
    [testSimpleInserts]

-- testInsertOAuth = QC.testProperty "genUser v1"  genUsersHere
testSimpleInserts = QC.testProperty "Inserts users in sqlite" insertUsersQC
-- testQuery = QC.testProperty "Query for pereviously inserted users" genUsersHere2

genUsersHere :: User -> User -> Property
genUsersHere u1 u2 = u1 /= u2 ==> _userEmail u1 /= _userEmail u2

insertUsersQC :: [User] -> Property
insertUsersQC users = ioProperty $ do
  b <- bulkInsert users
  pure (b === True)



bulkInsert :: [User] -> IO Bool
bulkInsert = \users -> withTestDb $ \conn ->
  runBeamSqliteDebug putStrLn {- for debug output -} conn $ do
    liftIO $ runBeamSqliteDebug putStrLn conn $
      autoMigrate Sqlite.migrationBackend shoppingCartDb
    inserted <- runInsertReturningList
        $ insert (_shoppingCartUsers $ unCheckDatabase shoppingCartDb) $
          insertValues users
    liftIO $ pure (length users == length inserted) --inserted)
