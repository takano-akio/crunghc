import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Char
import qualified Data.Digest.Pure.SHA as SHA
import Data.List
import qualified Data.ByteString.Lazy as SL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

data Config = Config
  { cmdGhcFlags :: [String]
  , cmdScriptPath :: FilePath
  , cmdScriptArgs :: [String]
  }

data CachePlan = CP
  { cpCachedir :: FilePath
  , cpStrategy :: CacheStrategy
  }

data CacheStrategy
  = HashPath FilePath

main :: IO ()
main = do
  config <- fromRightIO . parseConfig =<< getArgs
  plan <- chooseCachePlan config
  needRecomp <- testRecompilationNeeded config plan
  when needRecomp $ recompile config plan
  runCached config plan

parseConfig :: [String] -> Either String Config
parseConfig args = case span ("-" `isPrefixOf`) args of
  (flags, path:scriptArgs) -> Right Config
    { cmdGhcFlags = flags
    , cmdScriptPath = path
    , cmdScriptArgs = scriptArgs
    }
  _ -> Left "No file is given"

-- | Decide where to store the cache, and which strategy to use.
chooseCachePlan :: Config -> IO CachePlan
chooseCachePlan config = do
  canonicalScriptPath <- canonicalizePath $ cmdScriptPath config
  appDir <- getAppUserDataDirectory "crunghc"
  ghcVersion <- filter (/='\n') <$> readProcess "ghc" ["--numeric-version"] ""
  return $ CP
    { cpCachedir = appDir </> ghcVersion </> hashToDirname canonicalScriptPath
    , cpStrategy = HashPath canonicalScriptPath
    }

-- | Does the script need to be recompiled?
testRecompilationNeeded :: Config -> CachePlan -> IO Bool
testRecompilationNeeded config CP{cpCachedir=cachedir, cpStrategy=strat}
    = fmap isNothing $ runMaybeT $ do
  case strat of
    HashPath canonicalScriptPath -> do
      recordedPath <- checkIOError $ readFile $ cachedir </> "path"
      guard $ canonicalScriptPath == recordedPath
  exeModTime <- checkIOError $
    getModificationTime $ cachedir </> "cached.exe"
  deps <- readDependencies $ cachedir </> "deps"
  if length deps == 1
    then do -- Single-file program
      scriptModTime <- checkIOError $ getModificationTime $
        cmdScriptPath config
      guard $ exeModTime > scriptModTime
    else do
      -- Make sure that the working directory is the same as when the cache
      -- was created.
      wdir <- liftIO getCurrentDirectory
      cachedWdir <- checkIOError $ readFile $ cachedir </> "wdir"
      guard $ wdir == cachedWdir
      -- Check for modification times of all the source files
      forM_ deps $ \srcPath -> do
        modTime <- checkIOError $ getModificationTime srcPath
        guard $ exeModTime > modTime

-- | Compile the script and populate the cache directory.
recompile :: Config -> CachePlan -> IO ()
recompile config CP{cpCachedir=cachedir, cpStrategy=strat} = do
  createDirectoryIfMissing True $ cachedir </> "build"
  runGhc $ ghcArgs ["-M", "-dep-makefile", cachedir </> "deps"]
  runGhc $ ghcArgs
    [ "-hidir", builddir, "-odir", builddir
    , "-threaded"
    , "-o", cachedir </> "cached.exe"
    ]
  wdir <- getCurrentDirectory
  writeFile (cachedir </> "wdir") wdir
  case strat of
    HashPath canonicalScriptPath ->
      writeFile (cachedir </> "path") canonicalScriptPath
  where
    ghcArgs flags = cmdGhcFlags config ++ flags ++ [cmdScriptPath config]
    builddir = cachedir </> "build"
    runGhc args = do
      (exitCode, _, err) <- readProcessWithExitCode "ghc" args ""
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure{} -> die err

-- | Run the cached executable.
runCached :: Config -> CachePlan -> IO ()
runCached config plan =
  exitWith =<< rawSystem (cpCachedir plan </> "cached.exe") (cmdScriptArgs config)

readDependencies :: FilePath -> MaybeT IO [FilePath]
readDependencies depFile = do
  lns <- checkIOError $ lines <$> readFile depFile
  return $ mapMaybe extractDependency lns

extractDependency :: String -> Maybe FilePath
extractDependency ln
  | looksLikeSourceFile = Just $ takeWhileEnd (not . isSpace) ln
  | otherwise = Nothing
  where
    looksLikeSourceFile =
      ".hs" `isSuffixOf` ln ||
      ".lhs" `isSuffixOf` ln

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

checkIOError :: IO a -> MaybeT IO a
checkIOError action = MaybeT $
  (Just <$> action) `E.catch` \e -> const (return Nothing) (e::IOError)

hashToDirname :: String -> FilePath
hashToDirname
  = SHA.showDigest . SHA.sha1 . SL.fromStrict . T.encodeUtf8 . T.pack

fromRightIO :: Either String a -> IO a
fromRightIO = either die return

die :: String -> IO a
die msg = do
  prog <- getProgName
  hPutStrLn stderr $ prog ++ ": " ++ msg
  exitFailure
