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
import qualified Q2.Web.Api as Web.Api
import MocData
import Network.Wai.Handler.Warp


main, main2 :: IO ()
main =  run 9000 Web.Api.userApp


main2 = do
  let users = MocData.mocUsers
  conn <- open "/tmp/q2users.db"
  runBeamSqlite conn $ do
    users <- liftIO mocUsers
    autoMigrate Sqlite.migrationBackend q2UserDb
    runInsert $
      insert (_q2User $ unCheckDatabase q2UserDb) $
        insertValues users

  print $
    "Hello from " ++ doBeamT1 ++ "!"
