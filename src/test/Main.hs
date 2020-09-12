{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Cod
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified System.Exit as Exit
import qualified Test.HUnit as Test

main :: IO ()
main = do
  counts <- Test.runTestTT tests
  Monad.when (Test.errors counts > 0 || Test.failures counts > 0)
    Exit.exitFailure

tests :: Test.Test
tests = Test.TestList
  [ Test.TestLabel "handles invalid UTF-8 without throwing" $
    expectLeft "\x80"
  , Test.TestLabel "fails to parse an invalid module" $
    expectLeft "module invalid where"
  , Test.TestLabel "parses a valid module" $
    expectRight "module Valid where"
  ]

expectLeft :: ByteString.ByteString -> Test.Test
expectLeft contents = Test.TestCase $ do
  result <- Cod.parse "" contents
  Test.assertBool "expected Left but got Right" $ Either.isLeft result

expectRight :: ByteString.ByteString -> Test.Test
expectRight contents = Test.TestCase $ do
  result <- Cod.parse "" contents
  Test.assertBool "expected Right but got Left" $ Either.isRight result
