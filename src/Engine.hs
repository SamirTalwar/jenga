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
import System.Process (shell,readCreateProcess,readCreateProcessWithExitCode)
import Text.Printf (printf)

----------------------------------------------------------------------
-- Engine main

engineMain :: ([String] -> G ()) -> IO ()
engineMain userProg = do
  config@Config{args,localCache,mode} <- CommandLine.exec
  cacheDir <- if localCache then pure (Loc ".cache") else do
    home <- Loc <$> getHomeDirectory
    pure (home </> ".cache/jenga")
  i <- runB cacheDir config $ do
    initDirs
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

  when (i>0) $ do
    printf "ran %s\n" (pluralize i "action")

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
buildWithSystem config@Config{materializeAll} system = do
  reportSystem system
  let System{artifacts,how} = system
  let allTargets = Map.keys how
  BLog $ printf "materalizing %s"
    (if materializeAll then "all targets" else (pluralize (length artifacts) "artifact"))
  let whatToBuild = if materializeAll then allTargets else artifacts
  mapM_ (buildAndMaterialize config how) whatToBuild

reportSystem :: System -> B ()
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
    XHardLink cacheFile materializedFile

----------------------------------------------------------------------
-- locations for cache, sandbox etc

cachedFilesDir,tracesDir :: B Loc
cachedFilesDir = do cacheDir <- BCacheDir; pure (cacheDir </> "files")
tracesDir = do cacheDir <- BCacheDir; pure (cacheDir </> "traces")

sandboxDir,artifactsDir :: Loc
artifactsDir = Loc ",jenga"
sandboxDir = Loc ".jbox"

initDirs :: B ()
initDirs = do
  tracesDir <- tracesDir
  cachedFilesDir <- cachedFilesDir
  Execute $ do
    XRemoveDirRecursive sandboxDir
    XRemoveDirRecursive artifactsDir
    XMakeDir cachedFilesDir
    XMakeDir tracesDir
    XMakeDir sandboxDir
    XMakeDir artifactsDir

----------------------------------------------------------------------
-- Elaborate

type OrErr a = Either ErrMessage a
data ErrMessage = ErrMessage String

instance Show ErrMessage where show (ErrMessage s) = printf "Error: %s" s

data System = System { artifacts :: [Key], rules :: [Rule], how :: How }

type How = Map Key Rule

runElaboration :: Config -> G () -> B (OrErr System)
runElaboration config@Config{seeE} m = loop m system0 k0
  where
    system0 :: System
    system0 = System { rules = [], artifacts = [], how = Map.empty }

    k0 :: System -> () -> B (OrErr System)
    k0 s () = pure (Right s)

    logE :: String -> B ()
    logE mes = when seeE $ BLog (printf "E: %s" mes)

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
        logE $ printf "Elaborate rule: %s" (show rule)
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
        -- logE $ printf "Elaborate artifact: %s" (show key)
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
doBuild config@Config{seeB} how key = demand key
  where
    log :: String -> B ()
    log mes = when seeB $ BLog (printf "B: %s" mes)

    -- TODO: detect & error on build cycles
    demand :: Key -> B Checksum
    demand sought = do
      BGetKey sought >>= \case
        Just sum -> pure sum
        Nothing -> do
          sum <- demand1 sought
          BSetKey sought sum -- test/example6 fails if this line is removed
          pure sum

    demand1 :: Key -> B Checksum
    demand1 sought = do
      log $ printf "Require: %s" (show sought)
      case Map.lookup sought how of
        Nothing -> do
          let Key loc = sought
          Execute (XFileExists loc) >>= \case
            False -> do
              error (printf "'%s' is not source and has no build rule" (show sought))
            True -> do
              checksum <- insertIntoCache Soft loc
              pure checksum

        -- TODO: document this flow...
        Just rule -> do
          log $ printf "Consult: %s" (show rule)
          let Rule{depcom} = rule
          (deps,action) <- gatherDeps config how depcom

          wdeps <- (WitMap . Map.fromList) <$>
            sequence [ do checksum <- demand dep; pure (locateKey dep,checksum)
                     | dep <- deps
                     ]

          let witKey = WitnessKey { action, wdeps }
          wks <- hashWitnessKey witKey
          verifyWitness sought wks >>= \case
            Just checksum -> do
              pure checksum

            Nothing -> do
              log $ printf "Execute: %s" (show rule)
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
        Execute (XHardLink file (sandbox </> show loc))
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
            checksum <- insertIntoCache Hard sandboxLoc
            pure (tag,checksum)
    | target <- targets
    ]

data InsertMode = Soft | Hard

insertIntoCache :: InsertMode -> Loc -> B Checksum
insertIntoCache mode loc = do
  let insertCommand = case mode of Soft -> XCopyFile; Hard -> XHardLink
  checksum <- Execute (XMd5sum loc)
  file <- cacheFile checksum
  Execute (XFileExists file) >>= \case
    -- TODO: when the file exists, we should propogate the executable status bit in case it has changed
    True -> pure ()
    False -> Execute $ do
      insertCommand loc file
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

runB :: Loc -> Config -> B () -> IO Int
runB cacheDir config b = runX config $ do
  loop b state0 k0
  where
    state0 = BState { sandboxCounter = 0, memo = Map.empty }

    k0 :: BState -> () -> X Int
    k0 BState{sandboxCounter=i} () = do
      pure i

    loop :: B a -> BState -> (BState -> a -> X Int) -> X Int
    loop m0 s k = case m0 of
      BRet a -> k s a
      BBind m f -> loop m s $ \s a -> loop (f a) s k
      BLog mes -> do XLog mes; k s ()
      BCacheDir -> k s cacheDir
      BNewSandbox -> do
        let BState{sandboxCounter=i} = s
        k s { sandboxCounter = i+1 } (sandboxDir </> show i)
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
  XHardLink :: Loc -> Loc -> X ()
  XRemoveDirRecursive :: Loc -> X ()

runX :: Config -> X a -> IO a
runX Config{seeA,seeX,seeI} = loop
  where
    logA,logX,logI,_logD :: String -> IO ()
    logA mes = when seeA $ printf "A: %s\n" mes
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
        logA $ printf "cd %s; %s" dir command
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
        createLink src dest
      XRemoveDirRecursive (Loc fp) -> do
        logI $ printf "rm -rf %s" fp
        removePathForcibly fp
