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
    ""
    "\x80"
  , Test.TestLabel "fails to parse an invalid module" $ expectLeft
    []
    ""
    "module invalid where"
  , Test.TestLabel "parses a valid module" $ expectRight
    []
    ""
    "module Valid where"
  , Test.TestLabel "fails to parse without required extension" $ expectLeft
    []
    ""
    "module WithoutBlockArguments where\n\
    \zero = id do 0"
  , Test.TestLabel "parses with required extension" $ expectRight
    []
    ""
    "{-# language BlockArguments #-}\n\
    \module WithBlockArguments where\n\
    \zero = id do 0"
  , Test.TestLabel "parses with required extension as GHC option" $ expectRight
    []
    ""
    "{-# options_ghc -XBlockArguments #-}\n\
    \module WithBlockArguments where\n\
    \zero = id do 0"
  , Test.TestLabel "parses with required extension passed in" $ expectRight
    [(True, X.BlockArguments)]
    ""
    "module WithBlockArguments where\n\
    \zero = id do 0"
  , Test.TestLabel "handles implied extensions" $ expectRight
    [(True, X.RankNTypes)]
    ""
    "module WithExplicitForAll where \n\
    \identity :: forall a . a -> a\n\
    \identity x = x"
  , Test.TestLabel "handles CPP" $ expectRight
    [(True, X.Cpp)]
    ""
    "module WithCpp where\n\
    \#"
  , Test.TestLabel "handles CPP errors without throwing" $ expectLeft
    [(True, X.Cpp)]
    ""
    "module WithCpp where\n\
    \#error"
  , Test.TestLabel "does not unlit without extension" $ expectLeft
    []
    ""
    "> module WithoutLiterateHaskell where"
  , Test.TestLabel "unlits with extension" $ expectRight
    []
    ".lhs"
    "> module WithLiterateHaskell where"
  , Test.TestLabel "handles shebang lines" $ expectRight
    []
    ""
    "#! /bin/true\n\
    \module WithShebang where"
  , it "extracts a value declaration" $ do
    Right module_ <- Cod.parse [] "" "unit = ()"
    Test.assertEqual "" ["unit"] $ Cod.extract module_
  , it "extracts a type declaration" $ do
    Right module_ <- Cod.parse [] "" "unit :: ()"
    Test.assertEqual "" ["unit"] $ Cod.extract module_
  ]

it :: String -> Test.Assertion -> Test.Test
it label = Test.TestLabel label . Test.TestCase

expectLeft :: [(Bool, X.Extension)] -> FilePath -> ByteString.ByteString -> Test.Test
expectLeft extensions filePath contents = Test.TestCase $ do
  result <- Cod.parse extensions filePath contents
  Test.assertBool "expected Left but got Right" $ Either.isLeft result

expectRight :: [(Bool, X.Extension)] -> FilePath -> ByteString.ByteString -> Test.Test
expectRight extensions filePath contents = Test.TestCase $ do
  result <- Cod.parse extensions filePath contents
  Test.assertBool "expected Right but got Left" $ Either.isRight result
