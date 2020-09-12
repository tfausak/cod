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
import qualified Lexer
import qualified Parser
import qualified SrcLoc
import qualified StringBuffer

type Errors = Bag.Bag ErrUtils.ErrMsg

type Module = SrcLoc.Located (GHC.Hs.HsModule GHC.Hs.GhcPs)

parse
  :: [GHC.LanguageExtensions.Type.Extension]
  -> FilePath
  -> Data.ByteString.ByteString
  -> IO (Either Errors Module)
parse extensions filePath byteString = do
  let libdir = Just GHC.Paths.libdir
  dynFlags1 <- GHC.runGhc libdir GHC.getSessionDynFlags

  let onDecodeError = Data.Text.Encoding.Error.lenientDecode
  let text = Data.Text.Encoding.decodeUtf8With onDecodeError byteString
  let string = Data.Text.unpack text
  let stringBuffer = StringBuffer.stringToStringBuffer string

  let fastString = FastString.mkFastString filePath
  let realSrcLoc = SrcLoc.mkRealSrcLoc fastString 1 1

  let dynFlags2 = foldr (flip DynFlags.xopt_set) dynFlags1 extensions

  let locatedStrings = HeaderInfo.getOptions dynFlags2 stringBuffer filePath
  (dynFlags3, _, _) <- DynFlags.parseDynamicFilePragma dynFlags2 locatedStrings

  let pState1 = Lexer.mkPState dynFlags3 stringBuffer realSrcLoc

  pure $ case Lexer.unP Parser.parseModule pState1 of
    Lexer.PFailed pState2 -> Left . snd $ Lexer.getMessages pState2 dynFlags3
    Lexer.POk pState2 locatedHsModuleGhcPs ->
      let bagErrMsg = snd $ Lexer.getMessages pState2 dynFlags3
      in if null bagErrMsg
        then Right locatedHsModuleGhcPs
        else Left bagErrMsg
