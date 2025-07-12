module Engine (engineMain) where

import CommandLine (LogMode(..),Config(..),BuildMode(..),CacheDirSpec(..))
import CommandLine qualified (exec)
import Control.Exception (try,SomeException)
import Control.Monad (ap,liftM)
import Control.Monad (when)
import Data.Hash.MD5 qualified as MD5
import Data.IORef (newIORef,readIORef,writeIORef)
import Data.List (intercalate)
import Data.List qualified as List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Interface (G(..),D(..),Rule(..),Action(..),Key(..),Loc(..))
import StdBuildUtils ((</>),dirLoc)
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory,removePathForcibly,copyFile,getHomeDirectory)
import System.Exit(ExitCode(..))
import System.FileLock (tryLockFile,SharedExclusive(Exclusive),unlockFile)
import System.FilePath qualified as FP
import System.Posix.Files (fileExist,createLink,removeLink,getFileStatus,fileMode,intersectFileModes,setFileMode,getFileStatus,isDirectory)
import System.Process (shell,callProcess,readCreateProcess,readCreateProcessWithExitCode,Pid,getCurrentPid)
import Text.Printf (printf)
import Text.Read (readMaybe)

----------------------------------------------------------------------
-- Engine main

type UserProg = [String] -> G ()

engineMain :: UserProg -> IO ()
engineMain userProg = do
  config@Config{cacheDirSpec,logMode} <- CommandLine.exec
  let see = case logMode of LogQuiet -> False; _ -> True
  myPid <- getCurrentPid

  cacheDir <-
    case cacheDirSpec of
      CacheDirDefault -> do
        home <- getHomeDirectory
        pure (Loc home </> ".cache/jenga")
      CacheDirChosen dir -> do
        pure (Loc dir  </> ".cache/jenga")
      CacheDirTemp -> do
        let loc = Loc (printf "/tmp/.cache/jenga/%s" (show myPid))
        when see $ printf "using temporary cache: %s\n" (show loc)
        pure loc

  elaborateAndBuild myPid cacheDir config userProg

elaborateAndBuild :: Pid -> Loc -> Config -> UserProg -> IO ()
elaborateAndBuild myPid cacheDir config@Config{buildMode,args,logMode} userProg = do
  let see = case logMode of LogQuiet -> False; _ -> True
  case buildMode of
    ModeListTargets -> do
      i <- runB myPid cacheDir config $ do
        initDirs
        system <- runElaboration config (userProg args)
        let System{rules} = system
        let allTargets = [ target | Rule{hidden,targets} <- rules, target <- targets, not hidden ]
        sequence_ [ BLog (show key) | key <- allTargets ]
      when (see && i>0) $ do
        printf "ran %s\n" (pluralize i "action")
    ModeListRules -> do
      i <- runB myPid cacheDir config $ do
        initDirs
        system <- runElaboration config (userProg args)
        let System{how,rules} = system
        staticRules :: [StaticRule] <- concat <$>
          sequence [ do (deps,action@Action{hidden=actionHidden}) <- gatherDeps config how depcom
                        pure [ StaticRule { tag, targets, deps, action }
                             | not actionHidden
                             ]
                   | Rule{tag,hidden=ruleHidden,targets,depcom} <- rules
                   , not ruleHidden
                   ]
        BLog (intercalate "\n\n" (map show staticRules))
      when (see && i>0) $ do
        printf "ran %s\n" (pluralize i "action")
    ModeBuild -> do
      i <- runB myPid cacheDir config $ do
        initDirs
        system <- runElaboration config (userProg args)
        reportSystem config system
        buildWithSystem config system
      when (see && i>0) $ do
        printf "ran %s\n" (pluralize i "action")
    ModeBuildAndRun target argsForTarget -> do
      _i <- runB myPid cacheDir config $ do
        initDirs
        system <- runElaboration config (userProg [FP.takeDirectory target])
        buildWithSystem config system
      callProcess (printf ",jenga/%s" target) argsForTarget

buildWithSystem :: Config -> System -> B ()
buildWithSystem config@Config{reverseDepsOrder} system = do
  let System{rules,how} = system
  let allTargets = [ target | Rule{hidden,targets} <- rules, target <- targets, not hidden ]
  _ :: [()] <- parallel [ buildAndMaterialize config how key
                        | key <- (if reverseDepsOrder then reverse allTargets else allTargets)
                        ]
  pure ()

data StaticRule = StaticRule
  { tag :: String
  , targets :: [Key]
  , deps :: [Key]
  , action :: Action
  }

-- TODO: this display of static rules does not take account of the fact that the
-- action is relative and designed to be run in a sandbox.
-- We can improve this simply by prefixing with "cd dir; "
instance Show StaticRule where
  show StaticRule{targets,deps,action=Action{command}} = do
    printf "%s : %s\n  %s" (seeKeys targets) (seeKeys deps) command

seeKeys :: [Key] -> String
seeKeys = intercalate " " . map show

pluralize :: Int -> String -> String
pluralize n what = printf "%d %s%s" n what (if n == 1 then "" else "s")

reportSystem :: Config -> System -> B ()
reportSystem Config{logMode} system = do
  let see = case logMode of LogQuiet -> False; _ -> True
  let System{rules} = system
  let nRules = sum [ if hidden then 0 else 1 | Rule{hidden} <- rules ]
  let nTargets = sum [ length targets |  Rule{targets,hidden} <- rules, not hidden ]
  when see $ BLog $ printf "elaborated %s and %s" (pluralize nRules "rule") (pluralize nTargets "target")

buildAndMaterialize :: Config -> How -> Key -> B ()
buildAndMaterialize config how key = do
  digest <- doBuild config how key
  materialize digest key
  pure ()

materialize :: Digest -> Key -> B ()
materialize digest key = do
  cacheFile <- cacheFile digest
  let materializedFile = artifactsDir </> show key
  Execute $ do
    XMakeDir (dirLoc materializedFile)
    XHardLink cacheFile materializedFile >>= \case
      True -> pure ()
      False -> do
        -- Likely from concurrently runnning jenga
        -- If we do materialization when (just after) the action is run, this wont happen.
        -- XLog (printf "Materialize/HardLink: we lost the race: %s -> %s" (show cacheFile) (show materializedFile))
        pure ()

----------------------------------------------------------------------
-- locations for cache, sandbox etc

cachedFilesDir,tracesDir :: B Loc
cachedFilesDir = do cacheDir <- BCacheDir; pure (cacheDir </> "files")
tracesDir = do cacheDir <- BCacheDir; pure (cacheDir </> "traces")

artifactsDir :: Loc
artifactsDir = Loc ",jenga"

initDirs :: B ()
initDirs = do
  tracesDir <- tracesDir
  cachedFilesDir <- cachedFilesDir
  Execute $ do
    XRemoveDirRecursive artifactsDir
    XMakeDir cachedFilesDir
    XMakeDir tracesDir
    XMakeDir artifactsDir

----------------------------------------------------------------------
-- Elaborate

type OrErr a = Either ErrMessage a
data ErrMessage = ErrMessage String

instance Show ErrMessage where show (ErrMessage s) = printf "Error: %s" s

data System = System { rules :: [Rule], how :: How }

type How = Map Key Rule

runElaboration :: Config -> G () -> B System
runElaboration config m =
  loop m system0 k0 >>= \case
    Left mes -> do
      error $ printf "runElaboration -> Error:\n%s\n" (show mes)
    Right system -> do
      pure system

  where
    system0 :: System
    system0 = System { rules = [], how = Map.empty }

    k0 :: System -> () -> B (OrErr System)
    k0 s () = pure (Right s)

    loop :: G a -> System -> (System -> a -> B (OrErr System)) -> B (OrErr System)
    loop m system k = case m of
      GRet a -> k system a
      GBind m f -> loop m system $ \system a -> loop (f a) system k
      GLog mes -> do
        Execute (XLog (printf "log: %s" mes))
        k system ()
      GFail mes -> pure (Left (ErrMessage mes)) -- ignore k
      GRule rule -> do
        let Rule{targets} = rule
        xs <- sequence [ do b <- Execute (XFileExists loc); pure (key,b)
                        | key@(Key loc) <- targets ]
        case [ key | (key,isSource) <- xs, isSource ] of
          clashS@(_:_) -> do
            let mes = printf "rule targets clash with source :%s" (show clashS)
            pure (Left (ErrMessage mes))
          [] -> do
            let System{rules,how} = system
            case filter (flip Map.member how) targets of
              clashR@(_:_) -> do
                let mes = printf "rule targets defined by previous rule :%s" (show clashR)
                pure (Left (ErrMessage mes))
              [] -> do
                how <- pure $ List.foldl' (flip (flip Map.insert rule)) how targets
                k system { rules = rule : rules, how } ()

      GGlob dir -> do
        locs <- Execute (XGlob dir)
        k system locs
      GExists loc -> do
        bool <- Execute (XFileExists loc)
        k system bool
      GIsDirectory loc -> do
        bool <- Execute (XIsdirectory loc)
        k system bool
      GReadKey key -> do
        let System{how} = system
        contents <- readKey config how key -- make cause building
        k system contents

locateKey :: Key -> Loc
locateKey (Key (Loc fp)) = Loc (FP.takeFileName fp)

----------------------------------------------------------------------
-- Build

doBuild :: Config -> How -> Key -> B Digest
doBuild config@Config{logMode,reverseDepsOrder} how = do
  -- TODO: document this flow.
  -- TODO: check for cycles.
  memoDigestByKey $ \sought -> do
    case Map.lookup sought how of
      Nothing -> do
        let Key loc = sought
        Execute (XFileExists loc) >>= \case
          False -> do
            error (printf "'%s' is not source and has no build rule" (show sought))
          True -> do
            digest <- copyIntoCache loc
            pure digest

      Just rule@Rule{depcom} -> do
        let seeV = case logMode of LogVerbose -> True; _ -> False
        when seeV $ BLog (printf "B: Require: %s" (show sought))
        (deps0,action@Action{command}) <- gatherDeps config how depcom

        let deps = if reverseDepsOrder then reverse deps0 else deps0

        wdeps <- (WitMap . Map.fromList) <$>
          parallel [ do digest <- doBuild config how dep; pure (locateKey dep,digest)
                   | dep <- deps
                   ]

        let witKey = WitnessKey { command, wdeps }
        let wks = hashWitnessKey witKey

        let
          again = do
            verifyWitness sought wks >>= \case
              Just digest -> do
                pure digest

              Nothing -> do
                tracesDir <- tracesDir
                let lockLoc@(Loc lockPath) = tracesDir </> (show wks ++ ".lock")
                Execute (XIO (tryLockFile lockPath Exclusive)) >>= \case
                  Just lock -> do
                    wtargets <- runJobAndSaveWitness config action wks witKey wdeps rule
                    let digest = lookWitMap (locateKey sought) wtargets
                    Execute $ do
                      XIO (unlockFile lock)
                      XUnLink lockLoc
                    pure digest
                  Nothing -> do
                    --BLog (printf "lockJob failed(%s): %s" (show wks) (show command))
                    BYield
                    -- simply trying again means failed jobs arre re-attempted
                    -- which is not necessarily a good strategy
                    again

        again


runJobAndSaveWitness :: Config -> Action -> WitKeyHash -> WitnessKey -> WitMap -> Rule -> B WitMap
runJobAndSaveWitness config action wks witKey wdeps rule = do
  wtargets <- buildWithRule config action wdeps rule
  let val = WitnessValue { wtargets }
  let wit = Witness { key = witKey, val }
  saveWitness wks wit
  pure wtargets



memoDigestByKey :: (Key -> B Digest) -> Key -> B Digest
memoDigestByKey f sought = do
  BGetKey sought >>= \case
    Just digest -> pure digest
    Nothing -> do
      digest <- f sought
      BSetKey sought digest
      pure digest


gatherDeps :: Config -> How -> D a -> B ([Key],a)
gatherDeps config how d = loop d [] k0
  where
    k0 xs a = pure (reverse xs,a)
    loop :: D a -> [Key] -> ([Key] -> a -> B ([Key],b)) -> B ([Key], b)
    loop d xs k = case d of
      DRet a -> k xs a
      DBind m f -> loop m xs $ \xs a -> loop (f a) xs k
      DLog mes -> do
        Execute (XLog (printf "log: %s" mes))
        k xs ()
      DNeed key -> k (key:xs) ()
      DReadKey key -> do
        contents <- readKey config how key
        k xs contents
      DExistsKey key -> do
        b <- existsKey how key
        k xs b

readKey :: Config -> How -> Key -> B String
readKey config how key = do
  digest <- doBuild config how key
  file <- cacheFile digest
  Execute (XReadFile file)

existsKey :: How -> Key -> B Bool
existsKey how key =
  if Map.member key how
  then pure True
  else do
    let Key loc = key
    Execute (XFileExists loc)


buildWithRule :: Config -> Action -> WitMap -> Rule -> B WitMap
buildWithRule Config{keepSandBoxes} action depWit rule = do
  sandbox <- BNewSandbox
  let Rule{tag,targets} = rule
  Execute (XMakeDir sandbox)
  setupInputs sandbox depWit
  BRunActionInDir sandbox action >>= \case
    False ->
      error (printf "user action failed for rule: '%s'" tag)
    True -> do
      targetWit <- cacheOutputs sandbox targets
      when (not keepSandBoxes) $ Execute (XRemoveDirRecursive sandbox)
      pure targetWit


setupInputs :: Loc -> WitMap -> B ()
setupInputs sandbox (WitMap m1) = do
  sequence_
    [ do
        file <- cacheFile digest
        Execute $ do
          XHardLink file (sandbox </> show loc) >>= \case
            True -> pure ()
            False -> error "setupInput/HardLink: we lost the race" -- nothing ought to be racing us!

    | (loc,digest) <- Map.toList m1
    ]

cacheOutputs :: Loc -> [Key] -> B WitMap
cacheOutputs sandbox targets = do
  WitMap . Map.fromList <$> sequence
    [ do
        let tag = locateKey target
        let sandboxLoc = sandbox </> show tag
        Execute (XFileExists sandboxLoc) >>= \case
          False -> do
            error (printf "rule failed to produced declared target '%s'" (show target))
          True -> do
            digest <- linkIntoCache sandboxLoc
            pure (tag,digest)
    | target <- targets
    ]

copyIntoCache :: Loc -> B Digest
copyIntoCache loc = do
  digest <- Execute (XDigest loc)
  file <- cacheFile digest
  Execute (XFileExists file) >>= \case
    True -> pure ()
    False -> do
      Execute $ do
        XCopyFile loc file
        XMakeReadOnly file
  pure digest

linkIntoCache :: Loc -> B Digest
linkIntoCache loc = do
  digest <- Execute (XDigest loc)
  file <- cacheFile digest
  Execute $ do
    XFileExists file >>= \case -- small optimization
      True -> pure ()
      False -> do
        -- even though the file didn't exist just above, it could appear by now
        -- and so make this hard link fail.
        XHardLink loc file >>= \case
          True -> pure ()
          False -> do
            -- Likely a concurrently runnning jenga has chosen to run the same rule as us,
            -- at the same time, producing the same output.
            -- When we implement job locking, this should't happen.
            XLog (printf "linkIntoCache/HardLink: we lost the race: %s -> %s" (show loc) (show file))
            -- TODO: propogate the executable status
            pure ()
    XMakeReadOnly file
  pure digest

cacheFile :: Digest -> B Loc
cacheFile (Digest str) = do
  cachedFilesDir <- cachedFilesDir
  pure (cachedFilesDir </> str)

-- message digest of a file; computed by call to external md5sum
data Digest = Digest String

instance Show Digest where show (Digest str) = str

----------------------------------------------------------------------
-- Build witnesses (AKA constructive traces)

data Witness = Witness { key :: WitnessKey, val :: WitnessValue }

data WitnessValue = WitnessValue { wtargets :: WitMap }

-- TODO: target set in witness key? i.e. forcing rerun when add or remove targets
data WitnessKey = WitnessKey { command :: String, wdeps :: WitMap } deriving Show

data WitMap = WitMap (Map Loc Digest) deriving Show

-- message digest of a witness trace; computer by internal MD5 code
data WitKeyHash = WitKeyHash String -- TODO: renamed WitKeyDigest; name vars "wkd"

instance Show WitKeyHash where show (WitKeyHash str) = str

lookWitMap :: Loc -> WitMap -> Digest
lookWitMap loc (WitMap m) = maybe err id $ Map.lookup loc m
  where err = error "lookWitMap"

hashWitnessKey :: WitnessKey -> WitKeyHash
hashWitnessKey wk = WitKeyHash (MD5.md5s (MD5.Str (show wk)))

verifyWitness :: Key -> WitKeyHash -> B (Maybe Digest)
verifyWitness sought wks = do
  lookupWitness wks >>= \case
    Nothing -> pure Nothing
    Just wit -> do
      let Witness{val} = wit
      let WitnessValue{wtargets} = val
      let WitMap m = wtargets
      ok <- all id <$> sequence [ existsCacheFile digest | (_,digest) <- Map.toList m ]
      if not ok then pure Nothing else do
        -- The following lookup can fail if the sought-key was not recorded.
        -- i.e. we have added a target; but the actions/deps are otherwise unchanged
        -- the user action will have to be rerun.
        pure $ Map.lookup (locateKey sought) m

lookupWitness :: WitKeyHash -> B (Maybe Witness)
lookupWitness wks = do
  witFile <- witnessFile wks
  Execute (XFileExists witFile) >>= \case
    False -> pure Nothing
    True -> Execute $ do
      contents <- XReadFile witFile
      case importWitness contents of
        Just wit -> pure (Just wit)
        Nothing -> do
          --XLog (printf "importWitness failed: [%s]" contents)
          pure Nothing

existsCacheFile :: Digest -> B Bool
existsCacheFile digest = do
  file <- cacheFile digest
  Execute (XFileExists file)

saveWitness :: WitKeyHash -> Witness -> B ()
saveWitness wks wit = do
  witFile <- witnessFile wks
  Execute $ do
    XWriteFile (exportWitness wit ++ "\n") witFile

witnessFile :: WitKeyHash -> B Loc
witnessFile wks = do
  tracesDir <- tracesDir
  pure (tracesDir </> show wks)

----------------------------------------------------------------------
-- export/import Witness data in fixed format using flatter type

importWitness :: String -> Maybe Witness
importWitness s = fromQ <$> readMaybe s

exportWitness :: Witness -> String
exportWitness = show . toQ

data QWitness = WIT
  { command :: String
  , deps :: QWitMap
  , targets :: QWitMap
  }
  deriving (Show,Read)

type QWitMap = [(FilePath,QDigest)]
type QDigest = String

toQ :: Witness -> QWitness
toQ wit = do
  let Witness{key,val} = wit
  let WitnessKey{command,wdeps} = key
  let WitnessValue{wtargets} = val
  let fromStore (WitMap m) = [ (fp,digest) | (Loc fp,Digest digest) <- Map.toList m ]
  WIT { command, deps = fromStore wdeps, targets = fromStore wtargets }

fromQ :: QWitness -> Witness
fromQ WIT{command,deps,targets} = do
  let toStore xs = WitMap (Map.fromList [ (Loc fp,Digest digest) | (fp,digest) <- xs ])
  let key = WitnessKey{command, wdeps = toStore deps}
  let val = WitnessValue{wtargets = toStore targets}
  Witness{key,val}

----------------------------------------------------------------------
-- B: build monad

parallel :: [B a] -> B [a]
parallel = \case
  [] -> pure []
  [p] -> (\x->[x]) <$> p
  p:ps -> (\(x,xs) -> x:xs) <$> BPar p (parallel ps)

instance Functor B where fmap = liftM
instance Applicative B where pure = BRet; (<*>) = ap
instance Monad B where (>>=) = BBind

data B a where -- TODO: BFail?
  BRet :: a -> B a
  BBind :: B a -> (a -> B b) -> B b
  BLog :: String -> B ()
  BCacheDir :: B Loc
  BNewSandbox :: B Loc
  BRunActionInDir :: Loc -> Action -> B Bool
  Execute :: X a -> B a
  BGetKey :: Key -> B (Maybe Digest)
  BSetKey :: Key -> Digest -> B ()
  BPar :: B a -> B b -> B (a,b)
  BYield :: B ()

runB :: Pid -> Loc -> Config -> B () -> IO Int
runB myPid cacheDir config@Config{keepSandBoxes,logMode} b = runX config $ do
  let see = case logMode of LogQuiet -> False; _ -> True
  when (see && keepSandBoxes) $ XLog (printf "sandboxes created in: %s" (show sandboxParent))
  loop b bstate0 k0
  where
    -- TODO: We should cleanup our sandbox dir on abort.
    -- Currently it gets left if we C-c jenga
    -- Or if any build command fails; since that aborts jenga (TODO: fix that!)
    -- This means that even the cram tests leave .jbox droppings around.
    -- Not the end of the day, but we could be cleaner.
    sandboxParent = Loc (printf "/tmp/.jbox/%s" (show myPid))

    k0 :: BState -> () -> X Int
    k0 BState{runCounter,active,blocked} () = do
      when (not keepSandBoxes) $ XRemoveDirRecursive sandboxParent
      when ((length active + length blocked) /= 0) $ error "runB: unexpected left over jobs"
      pure runCounter

    loop :: B a -> BState -> (BState -> a -> X Int) -> X Int
    loop m0 s k = case m0 of
      BRet a -> k s a
      BBind m f -> loop m s $ \s a -> loop (f a) s k
      BLog mes -> do XLog mes; k s ()
      BCacheDir -> k s cacheDir
      BNewSandbox -> do
        let BState{sandboxCounter=i} = s
        k s { sandboxCounter = 1 + i } (sandboxParent </> show i)
      BRunActionInDir sandbox action@Action{hidden} -> do
        bool <- XRunActionInDir sandbox action
        k s { runCounter = runCounter s + (if hidden then 0 else 1) } bool
      Execute x -> do a <- x; k s a
      BGetKey key -> do
        let BState{memo} = s
        case Map.lookup key memo of
          Nothing -> k s Nothing
          Just digest -> k s (Just digest)
      BSetKey key digest -> do
        let BState{memo} = s
        k s { memo = Map.insert key digest memo } ()

      BPar a b -> do
        -- continutation k is called only when both sides have completed
        varA <- XIO (newIORef Nothing)
        varB <- XIO (newIORef Nothing)
        let
          kA s a = do
            XIO (readIORef varB) >>= \case
              Nothing -> do XIO (writeIORef varA (Just a)); next s
              Just b -> do k s (a,b)
        let
          kB s b = do
            XIO (readIORef varA) >>= \case
              Nothing -> do XIO (writeIORef varB (Just b)); next s
              Just a -> do k s (a,b)

        loop a s { active = BJob b kB : active s } kA

      BYield -> do
        let BState{blocked} = s
        let me = BJob (pure ()) k
        next s { blocked = me : blocked }

    next :: BState -> X Int
    next s@BState{active,blocked} = do
      case active of
        j:active -> do
          resume j s { active }
        [] -> case blocked of
          [] -> error "next: no more jobs"
          blocked -> do
            next s { active = reverse blocked, blocked = [] }

    resume :: BJob -> BState -> X Int
    resume (BJob p k) s = loop p s k

data BState = BState
  { sandboxCounter :: Int
  , runCounter :: Int -- less that sandboxCounter because of hidden actions
  , memo :: Map Key Digest
  , active :: [BJob]
  , blocked :: [BJob]
  }

bstate0 :: BState
bstate0 = BState
  { sandboxCounter = 0
  , runCounter = 0
  , memo = Map.empty
  , active = []
  , blocked = []
  }

data BJob where
  BJob :: B a -> (BState -> a -> X Int) -> BJob

----------------------------------------------------------------------
-- X: execution monad

instance Functor X where fmap = liftM
instance Applicative X where pure = XRet; (<*>) = ap
instance Monad X where (>>=) = XBind

data X a where
  XRet :: a -> X a
  XBind :: X a -> (a -> X b) -> X b
  XLog :: String -> X ()
  XIO :: IO a -> X a

  XRunActionInDir :: Loc -> Action -> X Bool
  XDigest :: Loc -> X Digest

  XMakeDir :: Loc -> X ()
  XGlob :: Loc -> X [Loc]
  XFileExists :: Loc -> X Bool
  XIsdirectory :: Loc -> X Bool
  XCopyFile :: Loc -> Loc -> X ()
  XMakeReadOnly :: Loc -> X ()
  XReadFile :: Loc -> X String
  XWriteFile :: String -> Loc -> X ()
  XHardLink :: Loc -> Loc -> X Bool -- False means it already exists
  XUnLink :: Loc -> X ()
  XRemoveDirRecursive :: Loc -> X ()

runX :: Config -> X a -> IO a
runX Config{logMode,seePid,seeX,seeI} = loop
  where
    log mes = do
      case seePid of
        False -> putStrLn mes
        True -> do
          myPid <- getCurrentPid
          printf "[%s] %s\n" (show myPid) mes

    seeA = case logMode of LogQuiet -> False; _ -> True
    logA,logX,logI :: String -> IO ()
    logA mes = when seeA $ log (printf "A: %s" mes)
    logX mes = when seeX $ log (printf "X: %s" mes)
    logI mes = when seeI $ log (printf "I: %s" mes)

    loop :: X a -> IO a
    loop x = case x of
      XRet a -> pure a
      XBind m f -> do a <- loop m; loop (f a)
      XIO io -> io
      XLog mes -> log mes

      -- sandboxed execution of user's action; for now always a bash command
      XRunActionInDir (Loc dir) Action{hidden,command} -> do
        let showHidden = False -- command line flag?
        if showHidden
          then logA $ printf "%s%s" command (if hidden then " (hidden)" else "")
          else when (not hidden) $ logA $ command

        (exitCode,stdout,stderr) <-
          withCurrentDirectory dir (readCreateProcessWithExitCode (shell command) "")
        -- TODO: better management & report of stdout/stderr
        putStr stdout
        putStr stderr
        let ok = case exitCode of ExitSuccess -> True; ExitFailure{} -> False
        pure ok

      -- other commands with shell out to external process
      XDigest (Loc fp) -> do
        let command = printf "md5sum %s" fp
        logX command
        output <- readCreateProcess (shell command) ""
        let str = case (splitOn " " output) of [] -> error "XDigest/split"; x:_ -> x
        pure (Digest str)

      -- internal file system access (log approx equivalent external command)
      XMakeDir (Loc fp) -> do
        logI $ printf "mkdir -p %s" fp
        createDirectoryIfMissing True fp
      XGlob (Loc fp) -> do
        logI $ printf "ls %s" fp
        xs <- listDirectory fp
        pure [ Loc fp </> x | x <- xs ]
      XFileExists (Loc fp) -> do
        logI $ printf "test -e %s" fp
        fileExist fp
      XIsdirectory (Loc fp) -> do
        logI $ printf "test -d %s" fp
        fileExist fp >>= \case
          False -> pure False
          True -> do
            status <- getFileStatus fp
            pure (isDirectory status)
      XCopyFile (Loc src) (Loc dest) -> do
        logI $ printf "cp %s %s" src dest
        copyFile src dest
      XMakeReadOnly (Loc fp) -> do
        logI $ printf "chmod a-w %s" fp
        old_mode <- fileMode <$> getFileStatus fp
        let new_mode = intersectFileModes 0o555 old_mode
        setFileMode fp new_mode
      XReadFile (Loc fp) -> do
        logI $ printf "cat %s" fp
        readFile fp
      XWriteFile contents (Loc dest) -> do
        logI $ printf "cat> %s" dest
        writeFile dest contents
      XHardLink (Loc src) (Loc dest) -> do
        logI $ printf "ln %s %s" src dest
        myCreateLink src dest
      XUnLink (Loc fp) -> do
        logI $ printf "rm -f %s" fp
        safeRemoveLink fp
      XRemoveDirRecursive (Loc fp) -> do
        logI $ printf "rm -rf %s" fp
        removePathForcibly fp


myCreateLink :: FilePath -> FilePath -> IO Bool
myCreateLink a b = do
  try (createLink a b) >>= \case
    Right () -> pure True
    Left (_e::SomeException) -> do
      pure False

safeRemoveLink :: FilePath -> IO ()
safeRemoveLink fp = do
  try (removeLink fp) >>= \case
    Right () -> pure ()
    Left (_e::SomeException) -> do
      printf "safeRemoveLink: caught %s\n" (show _e)
      pure ()
