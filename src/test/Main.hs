{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Cod
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified GHC.LanguageExtensions.Type as X
import qualified System.Exit as Exit
import qualified Test.HUnit as Test

main :: IO ()
main = do
  counts <- Test.runTestTT tests
  Monad.when (Test.errors counts > 0 || Test.failures counts > 0)
    Exit.exitFailure

tests :: Test.Test
tests = Test.TestList
  [ Test.TestLabel "handles invalid UTF-8 without throwing" $ expectLeft
    []
    "\x80"
  , Test.TestLabel "fails to parse an invalid module" $ expectLeft
    []
    "module invalid where"
  , Test.TestLabel "parses a valid module" $ expectRight
    []
    "module Valid where"
  , Test.TestLabel "fails to parse without required extension" $ expectLeft
    []
    "module WithoutBlockArguments where\n\
    \zero = id do 0"
  , Test.TestLabel "parses with required extension" $ expectRight
    []
    "{-# language BlockArguments #-}\n\
    \module WithBlockArguments where\n\
    \zero = id do 0"
  , Test.TestLabel "parses with required extension passed in" $ expectRight
    [(True, X.BlockArguments)]
    "module WithBlockArguments where\n\
    \zero = id do 0"
  , Test.TestLabel "handles implied extensions" $ expectRight
    [(True, X.RankNTypes)]
    "module WithExplicitForAll where \n\
    \identity :: forall a . a -> a\n\
    \identity x = x"
  ]

expectLeft :: [(Bool, X.Extension)] -> ByteString.ByteString -> Test.Test
expectLeft extensions contents = Test.TestCase $ do
  result <- Cod.parse extensions "" contents
  Test.assertBool "expected Left but got Right" $ Either.isLeft result

expectRight :: [(Bool, X.Extension)] -> ByteString.ByteString -> Test.Test
expectRight extensions contents = Test.TestCase $ do
  result <- Cod.parse extensions "" contents
  Test.assertBool "expected Right but got Left" $ Either.isRight result
