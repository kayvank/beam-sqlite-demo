{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Test.Utils where
import qualified Data.Text                      as T
import           Data.UUID
import           Data.UUID.V4
import           Q2.Types
import           Test.QuickCheck.Instances.UUID ()
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

newtype NonNilUUID = NonNilUUID { unNonNilUUID :: UUID } deriving (Eq, Show)
instance Arbitrary NonNilUUID where
  arbitrary = NonNilUUID <$> arbitrary  `suchThat` (/= nil)

newtype DbKey = DbKey {unKey :: T.Text} deriving (Eq, Show)
instance Arbitrary DbKey where
  arbitrary = do
    (NonNilUUID uid) <- arbitrary
    pure $ DbKey $ toText uid

instance Arbitrary User where
  arbitrary = do
    _userId   <- unKey <$> arbitrary
    _userEmail <- T.pack . getUnicodeString <$> arbitrary
    _userFirstName <- T.pack . getUnicodeString <$> arbitrary
    _userLastName <- T.pack . getUnicodeString <$> arbitrary
    _userPassword <- T.pack . getUnicodeString <$> arbitrary
    _userName <- elements [Just "app_dev", Just "app_dev2", Nothing]
    _userIsRegistered <- choose (True, False)
    _userRegion <- elements (["us-west-1", "us-west-2", "us-east-1", "us-east-2", "central-europ", "asia"] :: [T.Text])
    return $ User {..}
