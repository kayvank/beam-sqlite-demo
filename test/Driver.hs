{-# LANGUAGE TemplateHaskell #-}

module Main where
import Test.Tasty
import Test.Tasty
import qualified Test.OAuthSpec

main :: IO ()
main = defaultMain  =<< beamTestTree

beamTestTree :: IO TestTree
beamTestTree = do
  pure $
    testGroup
     "beam"
     [
       Test.OAuthSpec.testTree
     ]
