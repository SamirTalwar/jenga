module Engine (engineMain) where

import CommandLine (Config(..),Mode(..))
import CommandLine qualified (exec)
import Control.Monad (ap,liftM)
import Control.Monad (when)
import Data.Hash.MD5 qualified as MD5
import Data.List (intercalate)
import Data.List qualified as List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Interface (G(..),D(..),Rule(..),Action(..),Key(..),Loc(..))
import StdBuildUtils ((</>),dirLoc)
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory,removePathForcibly,copyFile,getHomeDirectory)
import System.Exit(ExitCode(..))
import System.FilePath qualified as FP
import System.Posix.Files (fileExist,createLink,getFileStatus,fileMode,intersectFileModes,setFileMode,getFileStatus,isDirectory)
import System.Process (shell,readCreateProcess,readCreateProcessWithExitCode,Pid,getCurrentPid)
import Text.Printf (printf)
import Control.Exception (try,SomeException)

----------------------------------------------------------------------
-- Engine main

type UserProg = [String] -> G ()

engineMain :: UserProg -> IO ()
engineMain userProg = do
  config@Config{cacheParentOverride} <- CommandLine.exec
  cacheDirParent <-
    case cacheParentOverride of
      Nothing -> Loc <$> getHomeDirectory
      Just dir -> pure (Loc dir)
  let cacheDir = cacheDirParent </> ".cache/jenga"
 -- printf "cache = %s\n" (show cacheDir) -- TODO: maybe on a verbose mode
  myPid <- getCurrentPid
  i <- runB myPid cacheDir config $ do
    initDirs
    elaborateAndBuild config userProg
  when (i>0) $ do
    printf "ran %s\n" (pluralize i "action") -- TODO: only when verbose?

elaborateAndBuild :: Config -> UserProg -> B ()
elaborateAndBuild config@Config{mode,args} userProg =
    runElaboration config (userProg args) >>= \case
      Left mes -> do
        BLog $ printf "go -> Error:\n%s\n" (show mes)
      Right system -> do
        case mode of
          ModeBuild -> do
            buildWithSystem config system
          ModeListTargets -> do
            let System{how} = system
            let allTargets = Map.keys how
            sequence_ [ BLog (show key) | key <- allTargets ]
          ModeListRules -> do
            let System{how,rules} = system
            staticRules <-
              sequence [ do (deps,action) <- gatherDeps config how depcom
                            pure $ StaticRule { tag, targets, deps, action }
                       | Rule{tag,targets,depcom} <- rules
                       ]
            BLog (intercalate "\n\n" (map show staticRules))

data StaticRule = StaticRule
  { tag :: String
  , targets :: [Key]
  , deps :: [Key]
  , action :: Action
  }

-- TODO: this display of static rules does not take account of the fact that the
-- action is relative and designed to be run in a sandbox.
instance Show StaticRule where
  show StaticRule{targets,deps,action} = do
    printf "%s : %s\n  %s" (seeKeys targets) (seeKeys deps) (show action)

seeKeys :: [Key] -> String
seeKeys = intercalate " " . map show

pluralize :: Int -> String -> String
pluralize n what = printf "%d %s%s" n what (if n == 1 then "" else "s")


buildWithSystem :: Config -> System -> B ()
buildWithSystem config@Config{materializeAll,reverseDepsOrder} system = do
  reportSystem system
  let System{artifacts,how} = system
  let allTargets = Map.keys how
  BLog $ printf "materalizing %s"
    (if materializeAll then "all targets" else (pluralize (length artifacts) "artifact"))
  let whatToBuild = if materializeAll then allTargets else artifacts
  mapM_ (buildAndMaterialize config how)
    (if reverseDepsOrder then reverse whatToBuild else whatToBuild)

reportSystem :: System -> B () -- TODO: only when verbose?
reportSystem system = do
  let System{rules,how} = system
  BLog $ printf "elaborated %s and %s"
    (pluralize (length rules) "rule")
    (pluralize (length how) "target")

buildAndMaterialize :: Config -> How -> Key -> B ()
buildAndMaterialize config how key = do
  checksum <- doBuild config how key
  materialize checksum key

materialize :: Checksum -> Key -> B ()
materialize (Checksum sum) (Key loc) = do
  cachedFilesDir <- cachedFilesDir
  let cacheFile = cachedFilesDir </> sum
  let materializedFile = artifactsDir </> show loc
  Execute $ do
    XMakeDir (dirLoc materializedFile)
    XHardLink cacheFile materializedFile >>= \case
      True -> pure ()
      False -> error "Materialize/HardLink: we lost the race" -- TODO

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

data System = System { artifacts :: [Key], rules :: [Rule], how :: How }

type How = Map Key Rule

runElaboration :: Config -> G () -> B (OrErr System)
runElaboration config m = loop m system0 k0
  where
    system0 :: System
    system0 = System { rules = [], artifacts = [], how = Map.empty }

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
        --BLog $ printf "Elaborate rule: %s" (show rule)
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

      GArtifact key -> do
        let System{artifacts} = system
        k system { artifacts = key : artifacts } ()
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
        _ <- doBuild config how key -- building before elaboration is finished
        let Key loc = key
        contents <- Execute (XReadFile loc)
        k system contents

locateKey :: Key -> Loc
locateKey (Key (Loc fp)) = Loc (FP.takeFileName fp)

----------------------------------------------------------------------
-- Build

doBuild :: Config -> How -> Key -> B Checksum
doBuild config@Config{seeB,reverseDepsOrder} how key = demand key
  where
    log :: String -> B ()
    log mes = when seeB $ BLog (printf "B: %s" mes)

    -- TODO: detect & error on build cycles
    demand :: Key -> B Checksum -- TODO: inline demand/demand1
    demand sought = do
      BGetKey sought >>= \case
        Just sum -> pure sum
        Nothing -> do
          sum <- demand1 sought
          BSetKey sought sum -- test/example6 fails if this line is removed
          pure sum

    demand1 :: Key -> B Checksum
    demand1 sought = do
      --log $ printf "Require: %s" (show sought) -- too noisy to see when it is source?
      case Map.lookup sought how of
        Nothing -> do
          let Key loc = sought
          Execute (XFileExists loc) >>= \case
            False -> do
              error (printf "'%s' is not source and has no build rule" (show sought))
            True -> do
              checksum <- copyIntoCache loc
              pure checksum

        -- TODO: document this flow...
        Just rule -> do
          log $ printf "Require: %s" (show sought)
          --log $ printf "Consult: %s" (show rule)
          let Rule{depcom} = rule
          (deps0,action) <- gatherDeps config how depcom

          let deps = if reverseDepsOrder then reverse deps0 else deps0

          wdeps <- (WitMap . Map.fromList) <$>
            sequence [ do checksum <- demand dep; pure (locateKey dep,checksum)
                     | dep <- deps
                     ]

          let witKey = WitnessKey { action, wdeps }
          wks <- hashWitnessKey witKey
          verifyWitness sought wks >>= \case
            Just checksum -> do
              --let Bash command = action in BLog $ printf "NOT RUNNING: %s" command -- debug
              pure checksum

            Nothing -> do
              --log $ printf "Execute: %s" (show rule)
              wtargets <- buildWithRule config action wdeps rule
              let val = WitnessValue { wtargets }
              let wit = Witness { key = witKey, val }
              saveWitness wks wit
              let checksum = lookWitMap (locateKey sought) wtargets
              pure checksum


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
        --Execute (XLog (printf "exists: %s -> %s" (show key) (show b)))
        k xs b

readKey :: Config -> How -> Key -> B String
readKey config how key = do
  checksum <- doBuild config how key
  file <- cacheFile checksum
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
  let Rule{targets} = rule
  Execute (XMakeDir sandbox)
  setupInputs sandbox depWit
  Execute (XRunActionInDir sandbox action) >>= \case
    False ->
      error (printf "user action failed for rule: '%s'" (show rule))
    True -> do
      targetWit <- cacheOutputs sandbox targets
      when (not keepSandBoxes) $ Execute (XRemoveDirRecursive sandbox)
      pure targetWit

setupInputs :: Loc -> WitMap -> B ()
setupInputs sandbox (WitMap m1) = do
  sequence_
    [ do
        file <- cacheFile checksum
        Execute $ do
          XHardLink file (sandbox </> show loc) >>= \case
            True -> pure ()
            False -> error "setupInput/HardLink: we lost the race" -- TODO

    | (loc,checksum) <- Map.toList m1
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
            checksum <- linkIntoCache sandboxLoc
            pure (tag,checksum)
    | target <- targets
    ]

copyIntoCache :: Loc -> B Checksum
copyIntoCache loc = do
  checksum <- Execute (XMd5sum loc)
  file <- cacheFile checksum
  Execute (XFileExists file) >>= \case
    True -> pure ()
    False -> do
      Execute $ do
        XCopyFile loc file
        XMakeReadOnly file
  pure checksum

linkIntoCache :: Loc -> B Checksum
linkIntoCache loc = do
  checksum <- Execute (XMd5sum loc)
  file <- cacheFile checksum
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
  pure checksum

cacheFile :: Checksum -> B Loc
cacheFile (Checksum sum) = do
  cachedFilesDir <- cachedFilesDir
  pure (cachedFilesDir </> sum)

----------------------------------------------------------------------
-- Build witnesses (AKA constructive traces)

data Witness = Witness { key :: WitnessKey, val :: WitnessValue }

data WitnessKey = WitnessKey { action :: Action, wdeps :: WitMap } deriving Show

data WitnessValue = WitnessValue { wtargets :: WitMap }

data WitMap = WitMap (Map Loc Checksum) deriving Show

data WitKeySum = WitKeySum Checksum

data Checksum = Checksum String

instance Show WitKeySum where show (WitKeySum (Checksum sum)) = sum
instance Show Checksum where show (Checksum sum) = sum

lookWitMap :: Loc -> WitMap -> Checksum
lookWitMap loc (WitMap m) = maybe err id $ Map.lookup loc m
  where err = error "lookWitMap"

hashWitnessKey :: WitnessKey -> B WitKeySum
hashWitnessKey wk = do
  checksum <- Execute (XHash (show wk))
  pure (WitKeySum checksum)

verifyWitness :: Key -> WitKeySum -> B (Maybe Checksum)
verifyWitness sought wks = do
  lookupWitness wks >>= \case
    Nothing -> pure Nothing
    Just wit -> do
      let Witness{val} = wit
      let WitnessValue{wtargets} = val
      let WitMap m = wtargets
      ok <- all id <$> sequence [ existsCacheFile sum | (_,sum) <- Map.toList m ]
      if not ok then pure Nothing else do
        -- The following lookup can fail if the sought-key was not recorded.
        -- i.e. we have added a target; but the actions/deps are otherwise unchanged
        -- the user action will have to be rerun.
        pure $ Map.lookup (locateKey sought) m

lookupWitness :: WitKeySum -> B (Maybe Witness)
lookupWitness wks = do
  tracesDir <- tracesDir
  let witFile = tracesDir </> show wks
  Execute (XFileExists witFile) >>= \case
    False -> pure Nothing
    True -> Execute $ do
      contents <- XReadFile witFile
      pure $ Just (exportWitness contents)

existsCacheFile :: Checksum -> B Bool
existsCacheFile checksum = do
  file <- cacheFile checksum
  Execute (XFileExists file)

saveWitness :: WitKeySum -> Witness -> B ()
saveWitness wks wit = do
  tracesDir <- tracesDir
  let witFile = tracesDir </> show wks
  Execute $ do
    XMakeDir (dirLoc witFile)
    XWriteFile (importWitness wit ++ "\n") witFile

----------------------------------------------------------------------
-- export/import Witness data in fixed format using flatter type

exportWitness :: String -> Witness
exportWitness = fromQ . read

importWitness :: Witness -> String
importWitness = show . toQ

data QWitness = WIT
  { command :: String
  , deps :: QWitMap
  , targets :: QWitMap
  }
  deriving (Show,Read)

type QWitMap = [(FilePath,QChecksum)]
type QChecksum = String

toQ :: Witness -> QWitness
toQ wit = do
  let Witness{key,val} = wit
  let WitnessKey{action=Bash command,wdeps} = key
  let WitnessValue{wtargets} = val
  let fromStore (WitMap m) = [ (fp,sum) | (Loc fp,Checksum sum) <- Map.toList m ]
  WIT { command, deps = fromStore wdeps, targets = fromStore wtargets }

fromQ :: QWitness -> Witness
fromQ WIT{command,deps,targets} = do
  let toStore xs = WitMap (Map.fromList [ (Loc fp,Checksum sum) | (fp,sum) <- xs ])
  let key = WitnessKey{action=Bash command,wdeps = toStore deps}
  let val = WitnessValue{wtargets = toStore targets}
  Witness{key,val}

----------------------------------------------------------------------
-- B: build monad

instance Functor B where fmap = liftM
instance Applicative B where pure = BRet; (<*>) = ap
instance Monad B where (>>=) = BBind

data B a where -- TODO: BFail?
  BRet :: a -> B a
  BBind :: B a -> (a -> B b) -> B b
  BLog :: String -> B ()
  BCacheDir :: B Loc
  BNewSandbox :: B Loc
  Execute :: X a -> B a
  BGetKey :: Key -> B (Maybe Checksum)
  BSetKey :: Key -> Checksum -> B ()

runB :: Pid -> Loc -> Config -> B () -> IO Int
runB myPid cacheDir config@Config{keepSandBoxes} b = runX config $ do
  loop b state0 k0
  where
    -- TODO: We should cleanup our sandbox dir on abort.
    -- Currently it gets left if we C-c jenga
    -- Or if any build command fails; since that aborts jenga (TODO: fix that!)
    -- This means that even the cram tests leave .jbox droppings around.
    -- Not the end of the day, but we could be cleaner.
    sandboxParentDirForThisProcess = Loc (printf "/tmp/.jbox/%s" (show myPid))

    state0 = BState { sandboxCounter = 0, memo = Map.empty }

    k0 :: BState -> () -> X Int
    k0 BState{sandboxCounter=i} () = do
      when (not keepSandBoxes) $ XRemoveDirRecursive sandboxParentDirForThisProcess
      pure i

    loop :: B a -> BState -> (BState -> a -> X Int) -> X Int
    loop m0 s k = case m0 of
      BRet a -> k s a
      BBind m f -> loop m s $ \s a -> loop (f a) s k
      BLog mes -> do XLog mes; k s ()
      BCacheDir -> k s cacheDir
      BNewSandbox -> do
        let BState{sandboxCounter=i} = s
        k s { sandboxCounter = i+1 } (sandboxParentDirForThisProcess </> show i)
      Execute x -> do a <- x; k s a
      BGetKey key -> do
        let BState{memo} = s
        case Map.lookup key memo of
          Nothing -> k s Nothing
          Just sum -> k s (Just sum)

      BSetKey key sum -> do
        let BState{memo} = s
        k s { memo = Map.insert key sum memo } ()

data BState = BState { sandboxCounter :: Int, memo :: Map Key Checksum }

----------------------------------------------------------------------
-- X: execution monad

instance Functor X where fmap = liftM
instance Applicative X where pure = XRet; (<*>) = ap
instance Monad X where (>>=) = XBind

data X a where
  XRet :: a -> X a
  XBind :: X a -> (a -> X b) -> X b
  XLog :: String -> X ()

  XRunActionInDir :: Loc -> Action -> X Bool
  XMd5sum :: Loc -> X Checksum

  XHash :: String -> X Checksum
  XMakeDir :: Loc -> X ()
  XGlob :: Loc -> X [Loc]
  XFileExists :: Loc -> X Bool
  XIsdirectory :: Loc -> X Bool
  XCopyFile :: Loc -> Loc -> X ()
  XMakeReadOnly :: Loc -> X ()
  XReadFile :: Loc -> X String
  XWriteFile :: String -> Loc -> X ()
  XHardLink :: Loc -> Loc -> X Bool -- False means it already exists
  XRemoveDirRecursive :: Loc -> X ()

runX :: Config -> X a -> IO a
runX Config{seeA,seeX,seeI} = loop
  where
    logA,logX,logI,_logD :: String -> IO ()
    logA mes = when seeA $ printf "A: %s\n" mes -- TODO verbose mode?
    logX mes = when seeX $ printf "X: %s\n" mes
    logI mes = when seeI $ printf "I: %s\n" mes
    _logD mes = printf "D: %s\n" mes

    loop :: X a -> IO a
    loop x = case x of
      XRet a -> pure a
      XBind m f -> do a <- loop m; loop (f a)
      XLog s -> do putStrLn s

      -- sandboxed execution of user's action; for now always a bash command
      XRunActionInDir (Loc dir) action -> do
        let (Bash command) = action
        -- Don't print sandbox dir as it contains $$ and so is not stable. Bad for testing!
        -- logA $ printf "cd %s; %s" dir command
        logA $ command
        (exitCode,stdout,stderr) <-
          withCurrentDirectory dir (readCreateProcessWithExitCode (shell command) "")
        -- TODO: better management & report of stdout/stderr
        putStr stdout
        putStr stderr
        let ok = case exitCode of ExitSuccess -> True; ExitFailure{} -> False
        --logA $ printf "cd %s; %s --> %s" dir command (show ok)
        pure ok

      -- other commands with shell out to external process
      XMd5sum (Loc fp) -> do
        let command = printf "md5sum %s" fp
        logX command
        output <- readCreateProcess (shell command) ""
        let sum = case (splitOn " " output) of [] -> undefined; x:_ -> x
        pure (Checksum sum)

      -- internal file system access (log approx equivalent external command)
      XHash contents -> do
        logI $ printf "md5sum"
        let sum = MD5.md5s (MD5.Str contents)
        pure (Checksum sum)
      XMakeDir (Loc fp) -> do
        logI $ printf "mkdir -p %s" fp
        createDirectoryIfMissing True fp
      XGlob (Loc fp) -> do
        logI $ printf "ls %s" fp
        xs <- listDirectory fp
        -- logI $ printf "ls %s --> %s" fp (show xs)
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
      XRemoveDirRecursive (Loc fp) -> do
        logI $ printf "rm -rf %s" fp
        removePathForcibly fp


myCreateLink :: FilePath -> FilePath -> IO Bool
myCreateLink a b = do
  try (createLink a b) >>= \case
    Right () -> pure True
    Left (_e::SomeException) -> do
      --printf "myCreateLink: caught %s\n" (show _e)
      pure False
