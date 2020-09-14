module Cod ( parseFile, parse, extract ) where

import qualified Bag
import qualified Data.List
import qualified DynFlags
import qualified EnumSet
import qualified ErrUtils
import qualified FastString
import qualified GHC
import qualified GHC.LanguageExtensions.Type
import qualified Lexer
import qualified OccName
import qualified Outputable
import qualified Parser
import qualified SrcLoc
import qualified StringBuffer
import qualified ToolSettings

parseFile :: FilePath -> IO (Either Errors (Module, Comments))
parseFile filePath = do
  string <- readFile filePath
  parse filePath string

parse :: FilePath -> String -> IO (Either Errors (Module, Comments))
parse filePath string = do
  let stringBuffer = StringBuffer.stringToStringBuffer string
  let fastString = FastString.mkFastString filePath
  let realSrcLoc = SrcLoc.mkRealSrcLoc fastString 1 1
  let pState1 = Lexer.mkPState dynFlags stringBuffer realSrcLoc
  pure $ case Lexer.unP Parser.parseModule pState1 of
    Lexer.PFailed pState2 ->
      Left . Errors $ Lexer.getErrorMessages pState2 dynFlags
    Lexer.POk pState2 modul ->
      let errors = Lexer.getErrorMessages pState2 dynFlags
      in if null errors
        then Right
          . (,) (Module modul)
          . Comments
          . fmap (fmap $ Data.List.sortOn SrcLoc.getLoc)
          . Data.List.sortOn fst
          . filter (not . null . snd)
          $ (SrcLoc.noSrcSpan, Lexer.comment_q pState2)
          : Lexer.annotations_comments pState2
        else Left $ Errors errors

-- TOOD: Find associated type signatures.
-- TODO: Find associated documentation.

extract :: Module -> Comments -> [String]
extract modul _ = concatMap (fromDecl . SrcLoc.unLoc)
  . GHC.hsmodDecls
  . SrcLoc.unLoc
  $ unwrapModule modul

fromDecl :: GHC.HsDecl GHC.GhcPs -> [String]
fromDecl hsDecl = case hsDecl of
  GHC.ValD _ hsBind -> case hsBind of
    GHC.FunBind _ lRdrName _ _ _ -> case SrcLoc.unLoc lRdrName of
      GHC.Unqual occName -> [OccName.occNameString occName]
      _ -> []
    _ -> []
  _ -> []

newtype Comments = Comments
  { unwrapComments :: [(SrcLoc.SrcSpan, [SrcLoc.Located GHC.AnnotationComment])]
  -- ^ The outer location is the "group" of comments. The inner location is
  -- the actual comment itself. See this Twitter thread:
  -- <https://twitter.com/taylorfausak/status/1305209220388868097>.
  }

instance Show Comments where
  show = Outputable.showSDoc dynFlags . Outputable.ppr . unwrapComments

newtype Errors = Errors
  { unwrapErrors :: ErrUtils.ErrorMessages
  }

instance Show Errors where
  show = show . Bag.bagToList . unwrapErrors

newtype Module = Module
  { unwrapModule :: SrcLoc.Located (GHC.HsModule GHC.GhcPs)
  }

instance Show Module where
  show = Outputable.showSDoc dynFlags . Outputable.ppr . unwrapModule

dynFlags :: DynFlags.DynFlags
dynFlags = DynFlags.DynFlags
  { DynFlags.avx = error "avx"
  , DynFlags.avx2 = error "avx2"
  , DynFlags.avx512cd = error "avx512cd"
  , DynFlags.avx512er = error "avx512er"
  , DynFlags.avx512f = error "avx512f"
  , DynFlags.avx512pf = error "avx512pf"
  , DynFlags.binBlobThreshold = error "binBlobThreshold"
  , DynFlags.bmiVersion = error "bmiVersion"
  , DynFlags.buildTag = error "buildTag"
  , DynFlags.cachedPlugins = error "cachedPlugins"
  , DynFlags.canGenerateDynamicToo = error "canGenerateDynamicToo"
  , DynFlags.canUseColor = error "canUseColor"
  , DynFlags.cfgWeightInfo = error "cfgWeightInfo"
  , DynFlags.cmdlineFrameworks = error "cmdlineFrameworks"
  , DynFlags.cmmProcAlignment = error "cmmProcAlignment"
  , DynFlags.colScheme = error "colScheme"
  , DynFlags.debugLevel = error "debugLevel"
  , DynFlags.depExcludeMods = error "depExcludeMods"
  , DynFlags.depIncludeCppDeps = error "depIncludeCppDeps"
  , DynFlags.depIncludePkgDeps = error "depIncludePkgDeps"
  , DynFlags.depMakefile = error "depMakefile"
  , DynFlags.depSuffixes = error "depSuffixes"
  , DynFlags.dirsToClean = error "dirsToClean"
  , DynFlags.dumpDir = error "dumpDir"
  , DynFlags.dumpFlags = EnumSet.empty
  , DynFlags.dumpPrefix = error "dumpPrefix"
  , DynFlags.dumpPrefixForce = error "dumpPrefixForce"
  , DynFlags.dylibInstallName = error "dylibInstallName"
  , DynFlags.dynHiSuf = error "dynHiSuf"
  , DynFlags.dynLibLoader = error "dynLibLoader"
  , DynFlags.dynObjectSuf = error "dynObjectSuf"
  , DynFlags.dynOutputFile = error "dynOutputFile"
  , DynFlags.enableTimeStats = error "enableTimeStats"
  , DynFlags.extensionFlags = EnumSet.fromList extensions
  , DynFlags.extensions = error "extensions"
  , DynFlags.fatalWarningFlags = error "fatalWarningFlags"
  , DynFlags.fileSettings = fileSettings
  , DynFlags.filesToClean = error "filesToClean"
  , DynFlags.floatLamArgs = error "floatLamArgs"
  , DynFlags.flushErr = error "flushErr"
  , DynFlags.flushOut = error "flushOut"
  , DynFlags.frameworkPaths = error "frameworkPaths"
  , DynFlags.frontendPluginOpts = error "frontendPluginOpts"
  , DynFlags.generalFlags = generalFlags
  , DynFlags.generatedDumps = error "generatedDumps"
  , DynFlags.ghcHeapSize = error "ghcHeapSize"
  , DynFlags.ghciHistSize = error "ghciHistSize"
  , DynFlags.ghciScripts = error "ghciScripts"
  , DynFlags.ghcLink = error "ghcLink"
  , DynFlags.ghcMode = error "ghcMode"
  , DynFlags.ghcNameVersion = ghcNameVersion
  , DynFlags.ghcVersionFile = error "ghcVersionFile"
  , DynFlags.haddockOptions = error "haddockOptions"
  , DynFlags.hcSuf = error "hcSuf"
  , DynFlags.hiDir = error "hiDir"
  , DynFlags.hieDir = error "hieDir"
  , DynFlags.hieSuf = error "hieSuf"
  , DynFlags.historySize = error "historySize"
  , DynFlags.hiSuf = error "hiSuf"
  , DynFlags.hooks = error "hooks"
  , DynFlags.hpcDir = error "hpcDir"
  , DynFlags.hscTarget = error "hscTarget"
  , DynFlags.ignorePackageFlags = error "ignorePackageFlags"
  , DynFlags.importPaths = error "importPaths"
  , DynFlags.includePaths = error "includePaths"
  , DynFlags.incoherentOnLoc = error "incoherentOnLoc"
  , DynFlags.initialUnique = error "initialUnique"
  , DynFlags.inlineCheck = error "inlineCheck"
  , DynFlags.integerLibrary = error "integerLibrary"
  , DynFlags.interactivePrint = error "interactivePrint"
  , DynFlags.language = error "language"
  , DynFlags.ldInputs = error "ldInputs"
  , DynFlags.liberateCaseThreshold = error "liberateCaseThreshold"
  , DynFlags.libraryPaths = error "libraryPaths"
  , DynFlags.liftLamsKnown = error "liftLamsKnown"
  , DynFlags.liftLamsNonRecArgs = error "liftLamsNonRecArgs"
  , DynFlags.liftLamsRecArgs = error "liftLamsRecArgs"
  , DynFlags.llvmConfig = error "llvmConfig"
  , DynFlags.log_action = error "log_action"
  , DynFlags.mainFunIs = error "mainFunIs"
  , DynFlags.mainModIs = error "mainModIs"
  , DynFlags.maxErrors = error "maxErrors"
  , DynFlags.maxInlineAllocSize = error "maxInlineAllocSize"
  , DynFlags.maxInlineMemcpyInsns = error "maxInlineMemcpyInsns"
  , DynFlags.maxInlineMemsetInsns = error "maxInlineMemsetInsns"
  , DynFlags.maxPmCheckModels = error "maxPmCheckModels"
  , DynFlags.maxRefHoleFits = error "maxRefHoleFits"
  , DynFlags.maxRelevantBinds = error "maxRelevantBinds"
  , DynFlags.maxSimplIterations = error "maxSimplIterations"
  , DynFlags.maxUncoveredPatterns = error "maxUncoveredPatterns"
  , DynFlags.maxValidHoleFits = error "maxValidHoleFits"
  , DynFlags.maxWorkerArgs = error "maxWorkerArgs"
  , DynFlags.newDerivOnLoc = error "newDerivOnLoc"
  , DynFlags.nextTempSuffix = error "nextTempSuffix"
  , DynFlags.nextWrapperNum = error "nextWrapperNum"
  , DynFlags.objectDir = error "objectDir"
  , DynFlags.objectSuf = error "objectSuf"
  , DynFlags.optLevel = error "optLevel"
  , DynFlags.outputFile = error "outputFile"
  , DynFlags.outputHi = error "outputHi"
  , DynFlags.overlapInstLoc = error "overlapInstLoc"
  , DynFlags.packageDBFlags = error "packageDBFlags"
  , DynFlags.packageEnv = error "packageEnv"
  , DynFlags.packageFlags = error "packageFlags"
  , DynFlags.parMakeCount = error "parMakeCount"
  , DynFlags.pkgDatabase = error "pkgDatabase"
  , DynFlags.pkgState = error "pkgState"
  , DynFlags.pkgTrustOnLoc = error "pkgTrustOnLoc"
  , DynFlags.platformConstants = error "platformConstants"
  , DynFlags.platformMisc = platformMisc
  , DynFlags.pluginModNameOpts = error "pluginModNameOpts"
  , DynFlags.pluginModNames = error "pluginModNames"
  , DynFlags.pluginPackageFlags = error "pluginPackageFlags"
  , DynFlags.pprCols = 80
  , DynFlags.pprUserLength = error "pprUserLength"
  , DynFlags.profAuto = error "profAuto"
  , DynFlags.rawSettings = error "rawSettings"
  , DynFlags.reductionDepth = error "reductionDepth"
  , DynFlags.refLevelHoleFits = error "refLevelHoleFits"
  , DynFlags.reverseErrors = error "reverseErrors"
  , DynFlags.rtccInfo = error "rtccInfo"
  , DynFlags.rtldInfo = error "rtldInfo"
  , DynFlags.rtsOpts = error "rtsOpts"
  , DynFlags.rtsOptsEnabled = error "rtsOptsEnabled"
  , DynFlags.rtsOptsSuggestions = error "rtsOptsSuggestions"
  , DynFlags.ruleCheck = error "ruleCheck"
  , DynFlags.safeHaskell = DynFlags.Sf_Ignore
  , DynFlags.safeInfer = error "safeInfer"
  , DynFlags.safeInferred = error "safeInferred"
  , DynFlags.simplPhases = error "simplPhases"
  , DynFlags.simplTickFactor = error "simplTickFactor"
  , DynFlags.solverIterations = error "solverIterations"
  , DynFlags.specConstrCount = error "specConstrCount"
  , DynFlags.specConstrRecursive = error "specConstrRecursive"
  , DynFlags.specConstrThreshold = error "specConstrThreshold"
  , DynFlags.splitInfo = error "splitInfo"
  , DynFlags.sseVersion = error "sseVersion"
  , DynFlags.staticPlugins = error "staticPlugins"
  , DynFlags.strictnessBefore = error "strictnessBefore"
  , DynFlags.stubDir = error "stubDir"
  , DynFlags.targetPlatform = error "targetPlatform"
  , DynFlags.thisComponentId_ = error "thisComponentId_"
  , DynFlags.thisInstalledUnitId = error "thisInstalledUnitId"
  , DynFlags.thisUnitIdInsts_ = error "thisUnitIdInsts_"
  , DynFlags.thOnLoc = error "thOnLoc"
  , DynFlags.toolSettings = toolSettings
  , DynFlags.trustFlags = error "trustFlags"
  , DynFlags.trustworthyOnLoc = error "trustworthyOnLoc"
  , DynFlags.ufCreationThreshold = error "ufCreationThreshold"
  , DynFlags.ufDearOp = error "ufDearOp"
  , DynFlags.ufDictDiscount = error "ufDictDiscount"
  , DynFlags.ufFunAppDiscount = error "ufFunAppDiscount"
  , DynFlags.ufKeenessFactor = error "ufKeenessFactor"
  , DynFlags.ufUseThreshold = error "ufUseThreshold"
  , DynFlags.ufVeryAggressive = error "ufVeryAggressive"
  , DynFlags.uniqueIncrement = error "uniqueIncrement"
  , DynFlags.useColor = error "useColor"
  , DynFlags.useUnicode = False
  , DynFlags.verbosity = 0
  , DynFlags.warningFlags = error "warningFlags"
  , DynFlags.warnSafeOnLoc = error "warnSafeOnLoc"
  , DynFlags.warnUnsafeOnLoc = error "warnUnsafeOnLoc"
  , DynFlags.ways = error "ways"
  }

extensions :: [GHC.LanguageExtensions.Type.Extension]
extensions =
  [ GHC.LanguageExtensions.Type.TraditionalRecordSyntax
  ]

fileSettings :: DynFlags.FileSettings
fileSettings = DynFlags.FileSettings
  { DynFlags.fileSettings_ghciUsagePath = error "fileSettings_ghciUsagePath"
  , DynFlags.fileSettings_ghcUsagePath = error "fileSettings_ghcUsagePath"
  , DynFlags.fileSettings_systemPackageConfig = error "fileSettings_systemPackageConfig"
  , DynFlags.fileSettings_tmpDir = error "fileSettings_tmpDir"
  , DynFlags.fileSettings_toolDir = error "fileSettings_toolDir"
  , DynFlags.fileSettings_topDir = error "fileSettings_topDir"
  }

generalFlags :: EnumSet.EnumSet DynFlags.GeneralFlag
generalFlags = EnumSet.fromList
  [ DynFlags.Opt_Haddock
  ]

ghcNameVersion :: DynFlags.GhcNameVersion
ghcNameVersion = DynFlags.GhcNameVersion
  { DynFlags.ghcNameVersion_programName = error "ghcNameVersion_programName"
  , DynFlags.ghcNameVersion_projectVersion = error "ghcNameVersion_projectVersion"
  }

platformMisc :: DynFlags.PlatformMisc
platformMisc = DynFlags.PlatformMisc
  { DynFlags.platformMisc_ghcDebugged = error "platformMisc_ghcDebugged"
  , DynFlags.platformMisc_ghcRTSWays = error "platformMisc_ghcRTSWays"
  , DynFlags.platformMisc_ghcRtsWithLibdw = error "platformMisc_ghcRtsWithLibdw"
  , DynFlags.platformMisc_ghcThreaded = error "platformMisc_ghcThreaded"
  , DynFlags.platformMisc_ghcWithInterpreter = error "platformMisc_ghcWithInterpreter"
  , DynFlags.platformMisc_ghcWithNativeCodeGen = error "platformMisc_ghcWithNativeCodeGen"
  , DynFlags.platformMisc_ghcWithSMP = error "platformMisc_ghcWithSMP"
  , DynFlags.platformMisc_integerLibrary = error "platformMisc_integerLibrary"
  , DynFlags.platformMisc_integerLibraryType = error "platformMisc_integerLibraryType"
  , DynFlags.platformMisc_leadingUnderscore = error "platformMisc_leadingUnderscore"
  , DynFlags.platformMisc_libFFI = error "platformMisc_libFFI"
  , DynFlags.platformMisc_llvmTarget = error "platformMisc_llvmTarget"
  , DynFlags.platformMisc_tablesNextToCode = error "platformMisc_tablesNextToCode"
  , DynFlags.platformMisc_targetPlatformString = error "platformMisc_targetPlatformString"
  }

toolSettings :: ToolSettings.ToolSettings
toolSettings = ToolSettings.ToolSettings
  { ToolSettings.toolSettings_ccSupportsNoPie = error "toolSettings_ccSupportsNoPie"
  , ToolSettings.toolSettings_extraGccViaCFlags = error "toolSettings_extraGccViaCFlags"
  , ToolSettings.toolSettings_ldIsGnuLd = error "toolSettings_ldIsGnuLd"
  , ToolSettings.toolSettings_ldSupportsBuildId = error "toolSettings_ldSupportsBuildId"
  , ToolSettings.toolSettings_ldSupportsCompactUnwind = error "toolSettings_ldSupportsCompactUnwind"
  , ToolSettings.toolSettings_ldSupportsFilelist = error "toolSettings_ldSupportsFilelist"
  , ToolSettings.toolSettings_opt_a = error "toolSettings_opt_a"
  , ToolSettings.toolSettings_opt_c = error "toolSettings_opt_c"
  , ToolSettings.toolSettings_opt_cxx = error "toolSettings_opt_cxx"
  , ToolSettings.toolSettings_opt_F = error "toolSettings_opt_F"
  , ToolSettings.toolSettings_opt_i = error "toolSettings_opt_i"
  , ToolSettings.toolSettings_opt_l = error "toolSettings_opt_l"
  , ToolSettings.toolSettings_opt_L = error "toolSettings_opt_L"
  , ToolSettings.toolSettings_opt_lc = error "toolSettings_opt_lc"
  , ToolSettings.toolSettings_opt_lcc = error "toolSettings_opt_lcc"
  , ToolSettings.toolSettings_opt_lm = error "toolSettings_opt_lm"
  , ToolSettings.toolSettings_opt_lo = error "toolSettings_opt_lo"
  , ToolSettings.toolSettings_opt_P = error "toolSettings_opt_P"
  , ToolSettings.toolSettings_opt_P_fingerprint = error "toolSettings_opt_P_fingerprint"
  , ToolSettings.toolSettings_opt_windres = error "toolSettings_opt_windres"
  , ToolSettings.toolSettings_pgm_a = error "toolSettings_pgm_a"
  , ToolSettings.toolSettings_pgm_ar = error "toolSettings_pgm_ar"
  , ToolSettings.toolSettings_pgm_c = error "toolSettings_pgm_c"
  , ToolSettings.toolSettings_pgm_dll = error "toolSettings_pgm_dll"
  , ToolSettings.toolSettings_pgm_F = error "toolSettings_pgm_F"
  , ToolSettings.toolSettings_pgm_i = error "toolSettings_pgm_i"
  , ToolSettings.toolSettings_pgm_l = error "toolSettings_pgm_l"
  , ToolSettings.toolSettings_pgm_L = error "toolSettings_pgm_L"
  , ToolSettings.toolSettings_pgm_lc = error "toolSettings_pgm_lc"
  , ToolSettings.toolSettings_pgm_lcc = error "toolSettings_pgm_lcc"
  , ToolSettings.toolSettings_pgm_libtool = error "toolSettings_pgm_libtool"
  , ToolSettings.toolSettings_pgm_lm = error "toolSettings_pgm_lm"
  , ToolSettings.toolSettings_pgm_lo = error "toolSettings_pgm_lo"
  , ToolSettings.toolSettings_pgm_P = error "toolSettings_pgm_P"
  , ToolSettings.toolSettings_pgm_ranlib = error "toolSettings_pgm_ranlib"
  , ToolSettings.toolSettings_pgm_T = error "toolSettings_pgm_T"
  , ToolSettings.toolSettings_pgm_windres = error "toolSettings_pgm_windres"
  }
