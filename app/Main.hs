{-# LANGUAGE OverloadedStrings #-}

module Main where

import BeamT1
import Data.UUID
import Data.UUID.V4
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate.Simple
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Sqlite
import qualified Database.Beam.Sqlite.Migrate as Sqlite
import Database.SQLite.Simple
import Q2.Types

main :: IO ()
main = do
  conn <- open "/tmp/shoppingcart1.db"
  [id1, id2, id3, id4] <- traverse (\_ -> toText <$> nextRandom) [0, 1, 2, 3]
  runBeamSqliteDebug putStrLn {- for debug output -} conn $ do
    autoMigrate Sqlite.migrationBackend shoppingCartDb
    runInsert $
      insert (_shoppingCartUsers $ unCheckDatabase shoppingCartDb) $
        insertValues
          [ User
              id1
              "james@example.com"
              "James"
              "Smith"
              "b4cc344d25a2efe540adbf2678e2304c"
              (Just "JS")
            "us-west-1",
            User
              id2
              "betty@example.com"
              "Betty"
              "Jones"
              "82b054bd83ffad9b6cf8bdb98ce3cc2f"
              (Just "BJ")
            "us-west-1",
            User
              id3
              "sam@example.com"
              "Sam"
              "Taylor"
              "332532dcfaa1cbf61e2a266bd723612c"
              Nothing
            "us-west-1",
            User
              id4
              "sam2@example.com"
              "Sam2"
              "Taylor2"
              "332532dcfaa1cbf61e2a266bd723612c"
              (Just "123")
            "us-west-1"
          ]
  print $
    "Hello from " ++ doBeamT1 ++ "!"
