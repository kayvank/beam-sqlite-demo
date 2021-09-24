{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Q2.Specifications where

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
import Data.Aeson
import Data.Functor ( (<&>) )
import Data.Maybe (isJust)

withTestDb :: (Connection -> IO a) -> IO a
withTestDb = handle asFailure . bracket (open ":memory:") close
  where
    asFailure (e :: SomeException) = assertFailure $ show e

testTree :: TestTree
testTree =
  testGroup
    "Simple generative test for bean and aeson"
    [testSimpleInserts, testJsonEncodeDecode]

testSimpleInserts = QC.testProperty "Insert and retrieve users in sqlite" insertUsersQC
testJsonEncodeDecode = QC.testProperty "JSON encode/devode" jsonEncodeDecode

insertUsersQC :: [User] -> Property
insertUsersQC users = ioProperty $ do
  b <- bulkInsert users
  pure (b === True)

bulkInsert :: [User] -> IO Bool
bulkInsert = \users -> withTestDb $ \conn ->
  runBeamSqliteDebug putStrLn {- for debug output -} conn $ do
    liftIO $ runBeamSqliteDebug putStrLn conn $
      autoMigrate Sqlite.migrationBackend q2UserDb
    inserted <- runInsertReturningList
        $ insert (_q2User $ unCheckDatabase q2UserDb) $
          insertValues users
    liftIO $ pure (length users == length inserted)

jsonEncodeDecode :: [User] ->  Bool
jsonEncodeDecode = \users ->
   isJust $ ( traverse decode (users <&> encode )::Maybe[User])
