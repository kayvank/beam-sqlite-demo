{-# LANGUAGE OverloadedStrings #-}

module MocData where

import Data.UUID
import Data.UUID.V4
import Database.Beam
import qualified Q2.Types as Q2
import qualified Data.Text as T

mocUsers :: IO [Q2.User]
mocUsers = do
  ids <- traverse (\_ ->  toText <$> nextRandom) [1 .. 4]
  pure $ buildUsers ids

buildUsers :: [T.Text]-> [Q2.User]
buildUsers ids = (\id ->
   Q2.User
      id
      "id@example.com"
      ("name-id" <> id)
      ("lastname-id" <> id)
       id
      (Just "JS")
      True
      "us-west-1") <$> ids
