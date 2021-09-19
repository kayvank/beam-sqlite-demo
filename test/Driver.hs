{-# LANGUAGE TemplateHaskell #-}

module Main where
import Test.Tasty
import Test.Tasty
import qualified Test.Q2.Specifications

main :: IO ()
main = defaultMain  =<< beamTestTree

beamTestTree :: IO TestTree
beamTestTree = do
  pure $
    testGroup
     "beam"
     [
       Test.Q2.Specifications.testTree
     ]
