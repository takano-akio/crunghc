{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Char
import qualified Data.Digest.Pure.SHA as SHA
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Typeable
import System.Directory
import System.Environment
import System.Exit
import System.FileLock
import System.FilePath
import System.IO
import System.IO.Error
import System.Process

#if USE_UNIX
import Control.Concurrent (myThreadId)
import System.Posix.Files (createLink)
import qualified System.Posix.Signals as Sig
#endif

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
  | HashContent
      S.ByteString -- ^ 0-separated ghc args
      S.ByteString -- ^ content

main :: IO ()
main = do
  installSignalHandlers
  config <- fromRightIO . parseConfig =<< getArgs
  plan <- chooseCachePlan config
  createDirectoryIfMissing True $ cpCachedir plan
  E.bracket
    (prepareExecutable config plan)
    snd -- cleanup
    (runTemp config . fst)
  `E.catch` \(KilledBySignal _) -> exitFailure

-- | Prepare a cached executable by rebuilding it if necessary.
-- Return a fresh copy or link of the executable and a cleanup action.
prepareExecutable
  :: Config -> CachePlan -> IO (FilePath, IO ())
prepareExecutable config plan =
  withFileLock (lockfile plan) Exclusive $ \_ -> do
    needRecomp <- testRecompilationNeeded config plan
    when needRecomp $ recompile config plan

    -- Here we make a copy of the cached executable, and run that copy.
    -- This is needed for two reasons:
    --
    -- 1. we want to avoid the race condition where some other process replaces
    --  the cached executable after we unlock it and before we execute it.
    -- 2. on Windows you cannot replace an executable while it's running.
    makeTemporaryExeCopy plan

parseConfig :: [String] -> Either String Config
parseConfig args = case span ("-" `isPrefixOf`) args of
  (flags, path:scriptArgs) -> Right Config
    { cmdGhcFlags = flags
    , cmdScriptPath = path
    , cmdScriptArgs = scriptArgs
    }
  _ -> Left "No file is given"

lockfile :: CachePlan -> FilePath
lockfile plan = cpCachedir plan </> "lock"

-- | Decide where to store the cache, and which strategy to use.
chooseCachePlan :: Config -> IO CachePlan
chooseCachePlan config = do
  contentHead <- readPrefix (contentHashThreshold + 1) $ cmdScriptPath config
  let contentAndGhcArgs = contentHead <> packedGhcArgs
  (toHash, strat) <- if S.length contentAndGhcArgs <= contentHashThreshold
    then return (contentAndGhcArgs, HashContent packedGhcArgs contentHead)
    else do
      canonicalScriptPath <- canonicalizePath $ cmdScriptPath config
      return
        ( T.encodeUtf8 $ T.pack canonicalScriptPath
        , HashPath canonicalScriptPath
        )
  appDir <- getAppUserDataDirectory "crunghc"
  ghcVersion <- filter (/='\n') <$> readProcess "ghc" ["--numeric-version"] ""
  return $ CP
    { cpCachedir = appDir </> ghcVersion </> hashToDirname toHash
    , cpStrategy = strat
    }
  where
    -- | Use filepath hashing if the total size of the ghc command line and the
    -- script itself exceeds this value. In particular it should be large enough
    -- to allow the boilerplate Setup.hs to be content-hashed.
    contentHashThreshold = 50

    packedGhcArgs = S.intercalate (S.singleton 0) $
      map (T.encodeUtf8 . T.pack) $ cmdGhcFlags config

readPrefix :: Int -> FilePath -> IO S.ByteString
readPrefix n path = withFile path ReadMode $ \h -> S.hGet h n

-- | Does the script need to be recompiled?
testRecompilationNeeded :: Config -> CachePlan -> IO Bool
testRecompilationNeeded config CP{cpCachedir=cachedir, cpStrategy=strat}
    = fmap isNothing $ runMaybeT $ do
  case strat of
    HashPath canonicalScriptPath -> do
      shouldEqual canonicalScriptPath $
        readFile $ cachedir </> "path"
    HashContent ghcArgs content -> do
      shouldEqual ghcArgs $ S.readFile $ cachedir </> "ghc-args"
      shouldEqual content $ S.readFile $ cachedir </> "content"
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
  where
    shouldEqual val action = do
      r <- checkIOError action
      guard $ val == r

-- | Compile the script and populate the cache directory.
recompile :: Config -> CachePlan -> IO ()
recompile config CP{cpCachedir=cachedir, cpStrategy=strat} = do
  createDirectoryIfMissing True $ cachedir </> "build"
  runGhc $ fullGhcArgs ["-M", "-dep-makefile", cachedir </> "deps"]
  runGhc $ fullGhcArgs
    [ "-hidir", builddir, "-odir", builddir
    , "-threaded"
    , "-o", cachedir </> "cached.exe"
    ]
  -- Try to strip it, but don't care if it fails.
  void (rawSystem "strip" [cachedir </> "cached.exe"])
    `E.catch` \e -> const (return ()) (e::E.IOException)
  wdir <- getCurrentDirectory
  writeFile (cachedir </> "wdir") wdir
  case strat of
    HashPath canonicalScriptPath ->
      writeFile (cachedir </> "path") canonicalScriptPath
    HashContent ghcArgs content -> do
      S.writeFile (cachedir </> "ghc-args") ghcArgs
      S.writeFile (cachedir </> "content") content
  where
    fullGhcArgs flags = cmdGhcFlags config ++ flags ++ [cmdScriptPath config]
    builddir = cachedir </> "build"
    runGhc args = do
      (exitCode, _, err) <- readProcessWithExitCode "ghc" args ""
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure{} -> die err

-- | Create a temporary copy or hard link of the cached executable for
-- exclusive use by this process. Return the path of the copy and a cleanup
-- action.
makeTemporaryExeCopy :: CachePlan -> IO (FilePath, IO ())
makeTemporaryExeCopy plan = do
  createDirectoryIfMissing True tempdir
  mydir <- createFreshDirectoryIn tempdir
  let tempPath = mydir </> "temp.exe"
  linkOrCopy (cpCachedir plan </> "cached.exe") tempPath
  return (tempPath, removeDirectoryRecursive mydir)
  where
    tempdir = cpCachedir plan </> "temp"
#if USE_UNIX
    -- Use hard links if available, because they are much cheaper to create.
    linkOrCopy = createLink
#else
    linkOrCopy = copyFile
#endif

createFreshDirectoryIn :: FilePath -> IO FilePath
createFreshDirectoryIn dir = loop =<< randomNumber
  where
    loop n = do
      let fresh = dir </> show n
      r <- E.tryJust (guard . isAlreadyExistsError) $ createDirectory fresh
      case r of
        Left{} -> loop $ n + 1
        Right{} -> return fresh

    randomNumber = do
      tod <- utctDayTime <$> getCurrentTime
      return $ flip mod (100000::Int) $
        floor $ realToFrac tod * (1000000 :: Double)

-- | Run the cached executable.
runTemp :: Config -> FilePath -> IO ()
runTemp config path = exitWith =<< rawSystem path (cmdScriptArgs config)

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

----------
-- signals

newtype KilledBySignal = KilledBySignal Signal
  deriving (Show, Typeable)

instance E.Exception KilledBySignal

#if USE_UNIX
type Signal = Sig.Signal
#else
type Signal = ()
#endif

-- | Install signal handlers that turn signals into KilledBySignal exceptions.
installSignalHandlers :: IO ()
#if USE_UNIX
installSignalHandlers = do
  tid <- myThreadId
  forM_ [Sig.softwareTermination] $ \sig ->
    void $ Sig.installHandler sig (handler tid sig) Nothing
  where
    handler tid sig = Sig.Catch $ do
      E.throwTo tid $ KilledBySignal sig
#else
installSignalHandlers = return ()
#endif

------------
-- utilities

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = reverse . takeWhile p . reverse

checkIOError :: IO a -> MaybeT IO a
checkIOError action = MaybeT $
  (Just <$> action) `E.catch` \e -> const (return Nothing) (e::IOError)

hashToDirname :: S.ByteString -> FilePath
hashToDirname = SHA.showDigest . SHA.sha1 . L.fromChunks . (:[])

fromRightIO :: Either String a -> IO a
fromRightIO = either die return

die :: String -> IO a
die msg = do
  prog <- getProgName
  hPutStrLn stderr $ prog ++ ": " ++ msg
  exitFailure
