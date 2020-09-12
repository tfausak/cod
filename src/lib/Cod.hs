module Cod ( parse ) where

import qualified Bag
import qualified Data.ByteString
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified DynFlags
import qualified ErrUtils
import qualified FastString
import qualified GHC
import qualified GHC.Hs
import qualified GHC.LanguageExtensions.Type
import qualified GHC.Paths
import qualified HeaderInfo
import qualified Language.Preprocessor.Cpphs
import qualified Lexer
import qualified Parser
import qualified SrcLoc
import qualified StringBuffer

type Errors = Bag.Bag ErrUtils.ErrMsg

type Module = SrcLoc.Located (GHC.Hs.HsModule GHC.Hs.GhcPs)

parse
  :: [(Bool, GHC.LanguageExtensions.Type.Extension)]
  -> FilePath
  -> Data.ByteString.ByteString
  -> IO (Either Errors Module)
parse extensions filePath byteString = do
  dynFlags1 <- getDynFlags

  let onDecodeError = Data.Text.Encoding.Error.lenientDecode
  let text = Data.Text.Encoding.decodeUtf8With onDecodeError byteString
  let string1 = Data.Text.unpack text
  let stringBuffer1 = StringBuffer.stringToStringBuffer string1

  let fastString = FastString.mkFastString filePath
  let realSrcLoc = SrcLoc.mkRealSrcLoc fastString 1 1

  let dynFlags2 = foldr toggleExtension dynFlags1 extensions

  let locatedStrings = HeaderInfo.getOptions dynFlags2 stringBuffer1 filePath
  (dynFlags3, _, _) <- DynFlags.parseDynamicFilePragma dynFlags2 locatedStrings

  let
    cpphsOptions = Language.Preprocessor.Cpphs.defaultCpphsOptions
      { Language.Preprocessor.Cpphs.boolopts =
        Language.Preprocessor.Cpphs.defaultBoolOptions
          { Language.Preprocessor.Cpphs.warnings = False
          }
      }
  string2 <- Language.Preprocessor.Cpphs.runCpphs cpphsOptions filePath string1
  let stringBuffer2 = StringBuffer.stringToStringBuffer string2

  let pState1 = Lexer.mkPState dynFlags3 stringBuffer2 realSrcLoc

  pure $ case Lexer.unP Parser.parseModule pState1 of
    Lexer.PFailed pState2 -> Left . snd $ Lexer.getMessages pState2 dynFlags3
    Lexer.POk pState2 locatedHsModuleGhcPs ->
      let bagErrMsg = snd $ Lexer.getMessages pState2 dynFlags3
      in if null bagErrMsg
        then Right locatedHsModuleGhcPs
        else Left bagErrMsg

getDynFlags :: IO DynFlags.DynFlags
getDynFlags = do
  let libdir = Just GHC.Paths.libdir
  GHC.runGhc libdir GHC.getSessionDynFlags

toggleExtension
  :: (Bool, GHC.LanguageExtensions.Type.Extension)
  -> DynFlags.DynFlags
  -> DynFlags.DynFlags
toggleExtension (bool, extension) =
  if bool then enableExtension extension else disableExtension extension

enableExtension
  :: GHC.LanguageExtensions.Type.Extension
  -> DynFlags.DynFlags
  -> DynFlags.DynFlags
enableExtension extension dynFlags = foldr
  toggleExtension
  (DynFlags.xopt_set dynFlags extension)
  (impliedExtensions extension)

disableExtension
  :: GHC.LanguageExtensions.Type.Extension
  -> DynFlags.DynFlags
  -> DynFlags.DynFlags
disableExtension = flip DynFlags.xopt_unset

impliedExtensions
  :: GHC.LanguageExtensions.Type.Extension
  -> [(Bool, GHC.LanguageExtensions.Type.Extension)]
impliedExtensions extension = case extension of
  GHC.LanguageExtensions.Type.AutoDeriveTypeable ->
    [ (True, GHC.LanguageExtensions.Type.DeriveDataTypeable) ]
  GHC.LanguageExtensions.Type.DeriveTraversable ->
    [ (True, GHC.LanguageExtensions.Type.DeriveFoldable)
    , (True, GHC.LanguageExtensions.Type.DeriveFunctor) ]
  GHC.LanguageExtensions.Type.DerivingVia ->
    [ (True, GHC.LanguageExtensions.Type.DerivingStrategies) ]
  GHC.LanguageExtensions.Type.DuplicateRecordFields ->
    [ (True, GHC.LanguageExtensions.Type.DisambiguateRecordFields) ]
  GHC.LanguageExtensions.Type.ExistentialQuantification ->
    [ (True, GHC.LanguageExtensions.Type.ExplicitForAll) ]
  GHC.LanguageExtensions.Type.FlexibleInstances ->
    [ (True, GHC.LanguageExtensions.Type.TypeSynonymInstances) ]
  GHC.LanguageExtensions.Type.FunctionalDependencies ->
    [ (True, GHC.LanguageExtensions.Type.MultiParamTypeClasses) ]
  GHC.LanguageExtensions.Type.GADTs ->
    [ (True, GHC.LanguageExtensions.Type.GADTSyntax)
    , (True, GHC.LanguageExtensions.Type.MonoLocalBinds) ]
  GHC.LanguageExtensions.Type.ImpredicativeTypes ->
    [ (True, GHC.LanguageExtensions.Type.RankNTypes) ]
  GHC.LanguageExtensions.Type.JavaScriptFFI ->
    [ (True, GHC.LanguageExtensions.Type.InterruptibleFFI) ]
  GHC.LanguageExtensions.Type.LiberalTypeSynonyms ->
    [ (True, GHC.LanguageExtensions.Type.ExplicitForAll) ]
  GHC.LanguageExtensions.Type.MultiParamTypeClasses ->
    [ (True, GHC.LanguageExtensions.Type.ConstrainedClassMethods) ]
  GHC.LanguageExtensions.Type.ParallelArrays ->
    [ (True, GHC.LanguageExtensions.Type.ParallelListComp) ]
  GHC.LanguageExtensions.Type.PolyKinds ->
    [ (True, GHC.LanguageExtensions.Type.KindSignatures) ]
  GHC.LanguageExtensions.Type.QuantifiedConstraints ->
    [ (True, GHC.LanguageExtensions.Type.ExplicitForAll) ]
  GHC.LanguageExtensions.Type.RankNTypes ->
    [ (True, GHC.LanguageExtensions.Type.ExplicitForAll) ]
  GHC.LanguageExtensions.Type.RebindableSyntax ->
    [ (False, GHC.LanguageExtensions.Type.ImplicitPrelude) ]
  GHC.LanguageExtensions.Type.RecordWildCards ->
    [ (True, GHC.LanguageExtensions.Type.DisambiguateRecordFields) ]
  GHC.LanguageExtensions.Type.ScopedTypeVariables ->
    [ (True, GHC.LanguageExtensions.Type.ExplicitForAll) ]
  GHC.LanguageExtensions.Type.StandaloneKindSignatures ->
    [ (False, GHC.LanguageExtensions.Type.CUSKs) ]
  GHC.LanguageExtensions.Type.Strict ->
    [ (True, GHC.LanguageExtensions.Type.StrictData) ]
  GHC.LanguageExtensions.Type.TemplateHaskell ->
    [ (True, GHC.LanguageExtensions.Type.TemplateHaskellQuotes) ]
  GHC.LanguageExtensions.Type.TypeFamilies ->
    [ (True, GHC.LanguageExtensions.Type.ExplicitNamespaces)
    , (True, GHC.LanguageExtensions.Type.KindSignatures)
    , (True, GHC.LanguageExtensions.Type.MonoLocalBinds) ]
  GHC.LanguageExtensions.Type.TypeFamilyDependencies ->
    [ (True, GHC.LanguageExtensions.Type.TypeFamilies) ]
  GHC.LanguageExtensions.Type.TypeInType ->
    [ (True, GHC.LanguageExtensions.Type.DataKinds)
    , (True, GHC.LanguageExtensions.Type.KindSignatures)
    , (True, GHC.LanguageExtensions.Type.PolyKinds) ]
  GHC.LanguageExtensions.Type.TypeOperators ->
    [ (True, GHC.LanguageExtensions.Type.ExplicitNamespaces) ]
  _ -> []
