module Engine (engineMain) where

import CommandLine (Config(..))
import CommandLine qualified (exec)
import Control.Monad (ap,liftM)
import Control.Monad (when)
import Data.Hash.MD5 qualified as MD5
import Data.List qualified as List (foldl')
import Data.List.Ordered (nubSort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Interface (G(..),D(..),Rule(..),Action(..),Key(..),Loc(..))
import StdBuildUtils ((</>),dirLoc)
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory,removePathForcibly,copyFile)
import System.FilePath qualified as FP
import System.Posix.Files (fileExist,createLink,getFileStatus,fileMode,intersectFileModes,setFileMode,getFileStatus,isDirectory)
import System.Process (callCommand,shell,readCreateProcess)
import Text.Printf (printf)

----------------------------------------------------------------------
-- Engine main

engineMain :: ([Loc] -> G ()) -> IO ()
engineMain userProg = do
  config@Config{args} <- CommandLine.exec
  let args' = case args of [] -> ["."]; _ -> args
  elaborateAndBuild config $ do
    xss <- mapM findStartingPointsFromTopLoc args'
    let startingLocs = nubSort (concat xss)
    (userProg startingLocs)

findStartingPointsFromTopLoc :: String -> G [Loc]
findStartingPointsFromTopLoc arg = do
  let loc = Loc arg
  GExists loc >>= \case
    False -> pure []
    True -> do
      GIsDirectory loc >>= \case
        False -> pure []
        True -> findSubdirsDeep loc

findSubdirsDeep :: Loc -> G [Loc]
findSubdirsDeep loc@(Loc fp) = do
  if blockName fp then pure [] else do
    subs <- findSubdirs loc
    subsubs <- mapM findSubdirsDeep subs
    pure (loc : concat subsubs)

blockName :: String -> Bool
blockName = \case
  ".git" -> True
  ".stack-work" -> True
  ".cache" -> True
  ",jenga" -> True
  _ -> False

findSubdirs :: Loc -> G [Loc]
findSubdirs loc = do
  locs <- GGlob loc
  blocs <- sequence [ do b <- GIsDirectory loc; pure (b,loc) | loc <- locs ]
  pure [ loc | (b,loc) <- blocs, b ]

elaborateAndBuild :: Config -> G () -> IO ()
elaborateAndBuild config@Config{materializeAll} m = do
  runB config (runElaboration config m) >>= \case
    Left mes -> printf "go -> Error:\n%s\n" (show mes)
    Right system -> do
      let System{artifacts,rules,how} = system
      let allTargets = Map.keys how
      let whatToBuild = if materializeAll then allTargets else artifacts
      printf "elaborated %s and %s\n"
        (pluralize (length rules) "rule")
        (pluralize (length how) "target")
      printf "materalizing %s\n"
        (if materializeAll then "all targets" else (pluralize (length artifacts) "artifact"))
      runB config $ do
        let System{how} = system
        sums <- doBuild config how whatToBuild
        sequence_ [ materialize sum key | (sum,key) <- zip sums whatToBuild ]

pluralize :: Int -> String -> String
pluralize n what = printf "%d %s%s" n what (if n == 1 then "" else "s")

----------------------------------------------------------------------
-- locations for cache, sandbox etc

jengaCache,cachedFilesDir,tracesDir :: Loc
jengaCache = Loc ".cache"
cachedFilesDir = jengaCache </> "files"
tracesDir = jengaCache </> "traces"

jengaDir,sandboxDir,artifactsDir :: Loc
jengaDir = Loc ",jenga"
sandboxDir = jengaDir </> "box"
artifactsDir = jengaDir </> "artifacts"

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
        xs <- sequence [ do b <- Execute (XExists loc); pure (key,b)
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
        bool <- Execute (XExists loc)
        k system bool
      GIsDirectory loc -> do
        bool <- Execute (XIsdirectory loc)
        k system bool
      GReadKey key -> do
        let System{how} = system
        _ <- doBuild config how [key] -- building before elaboration is finished
        let Key loc = key
        contents <- Execute (XReadFile loc)
        k system contents

locateKey :: Key -> Loc
locateKey (Key (Loc fp)) = Loc (FP.takeFileName fp)

----------------------------------------------------------------------
-- Build

materialize :: Checksum -> Key -> B ()
materialize (Checksum sum) (Key loc) = do
  let cacheFile = cachedFilesDir </> sum
  let materializedFile = artifactsDir </> show loc
  Execute $ do
    XMakeDir (dirLoc materializedFile)
    XHardLink cacheFile materializedFile

doBuild :: Config -> How -> [Key] -> B [Checksum]
doBuild config@Config{seeB} how artifacts = mapM demand artifacts
  where
    log :: String -> B ()
    log mes = when seeB $ BLog (printf "B: %s" mes)

    readKey :: Key -> B String
    readKey key = do
      checksum <- demand key
      Execute (XReadFile (cacheFile checksum))

    demand :: Key -> B Checksum
    demand requiredKey = do
      log $ printf "Require: %s" (show requiredKey)
      case Map.lookup requiredKey how of
        Nothing -> do
          -- we have no rule; so we hope/assume we have a source
          let Key loc = requiredKey
          checksum <- Execute (insertIntoCache Soft loc)
          pure checksum

        -- TODO: document this flow...
        Just rule -> do
          log $ printf "Consult: %s" (show rule)
          let Rule{depcom} = rule
          (deps,action) <- gatherDeps readKey depcom

          wdeps <- (WitMap . Map.fromList) <$>
            sequence [ do checksum <- demand dep; pure (locateKey dep,checksum)
                     | dep <- deps
                     ]

          let witKey = WitnessKey { action, wdeps }
          wks <- hashWitnessKey witKey
          verifyWitness wks >>= \case
            Just Witness{val=WitnessValue{wtargets}} -> do
              pure (lookWitMap (locateKey requiredKey) wtargets)

            Nothing -> do
              log $ printf "Execute: %s" (show rule)
              wtargets <- buildWithRule config action wdeps rule
              let val = WitnessValue { wtargets }
              let wit = Witness { key = witKey, val }
              saveWitness wks wit
              let requiredLoc = locateKey requiredKey
              let checksum = lookWitMap requiredLoc wtargets
              pure checksum

gatherDeps :: (Key -> B String) -> D a -> B ([Key],a)
gatherDeps readKey d = loop d [] k0
  where
    k0 xs a = pure (reverse xs,a) -- TODO: sort deps?
    loop :: D a -> [Key] -> ([Key] -> a -> B ([Key],b)) -> B ([Key], b)
    loop d xs k = case d of
      DRet a -> k xs a
      DBind m f -> loop m xs $ \xs a -> loop (f a) xs k
      DNeed key -> k (key:xs) ()
      DReadKey key -> do
        contents <- readKey key
        k xs contents

buildWithRule :: Config -> Action -> WitMap -> Rule -> B WitMap
buildWithRule Config{keepSandBoxes} action depWit rule = do
  sandbox <- NewSandbox
  let Rule{targets} = rule
  Execute (XMakeDir sandbox)
  Execute (setupInputs sandbox depWit)
  Execute (XRunActionInDir sandbox action)
  targetWit <- Execute (cacheOutputs sandbox targets)
  when (not keepSandBoxes) $ Execute (XRemoveDirRecursive sandbox)
  pure targetWit

setupInputs :: Loc -> WitMap -> X ()
setupInputs sandbox (WitMap m1) = do
  sequence_
    [ XHardLink (cacheFile checksum) (sandbox </> show loc)
    | (loc,checksum) <- Map.toList m1
    ]

cacheOutputs :: Loc -> [Key] -> X WitMap
cacheOutputs sandbox targets = do
  WitMap . Map.fromList <$> sequence
    [ do
        let loc = locateKey target
        checksum <- insertIntoCache Hard (sandbox </> show loc)
        pure (loc,checksum)
    | target <- targets
    ]

data InsertMode = Soft | Hard

insertIntoCache :: InsertMode -> Loc -> X Checksum
insertIntoCache mode loc = do
  let insertCommand = case mode of Soft -> XCopyFile; Hard -> XHardLink
  checksum <- XMd5sum loc
  let file = cacheFile checksum
  XExists file >>= \case
    True -> pure ()
    False -> do
      insertCommand loc file
      XMakeReadOnly file
  pure checksum

cacheFile :: Checksum -> Loc
cacheFile (Checksum sum) = cachedFilesDir </> sum

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

verifyWitness :: WitKeySum -> B (Maybe Witness)
verifyWitness wks = do
  lookupWitness wks >>= \case
    Nothing -> pure Nothing
    Just wit -> do
      let Witness{val} = wit
      let WitnessValue{wtargets=WitMap m} = val
      ok <- all id <$> sequence [ existsCacheFile sum | (_,sum) <- Map.toList m ]
      if not ok then pure Nothing else do
        pure (Just wit)

lookupWitness :: WitKeySum -> B (Maybe Witness)
lookupWitness wks = Execute $ do
  let witFile = tracesDir </> show wks
  XExists witFile >>= \case
    False -> pure Nothing
    True -> do
      contents <- XReadFile witFile
      pure $ Just (exportWitness contents)

existsCacheFile :: Checksum -> B Bool
existsCacheFile checksum = Execute (XExists (cacheFile checksum))

saveWitness :: WitKeySum -> Witness -> B ()
saveWitness wks wit = do
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

data B a where
  BRet :: a -> B a
  BBind :: B a -> (a -> B b) -> B b
  BLog :: String -> B ()
  NewSandbox :: B Loc
  Execute :: X a -> B a

runB :: Config -> B a -> IO a
runB config b = runX config $ do
  XMakeDir cachedFilesDir
  XMakeDir tracesDir
  XRemoveDirRecursive jengaDir
  XMakeDir sandboxDir
  XMakeDir artifactsDir
  loop b 0 k0
  where
    k0 :: Int -> a -> X a
    k0 i a = do
      when (i>0) $ XLog (printf "ran %s" (pluralize i "action"))
      pure a

    loop :: B a -> Int -> (Int -> a -> X b) -> X b
    loop m0 i k = case m0 of
      BRet a -> k i a
      BBind m f -> loop m i $ \i a -> loop (f a) i k
      BLog s -> do XLog s; k i ()
      NewSandbox -> k (i+1) (sandboxDir </> show i)
      Execute x -> do a <- x; k i a

----------------------------------------------------------------------
-- X: execution monad

instance Functor X where fmap = liftM
instance Applicative X where pure = XRet; (<*>) = ap
instance Monad X where (>>=) = XBind

data X a where
  XRet :: a -> X a
  XBind :: X a -> (a -> X b) -> X b
  XLog :: String -> X ()

  XRunActionInDir :: Loc -> Action -> X ()
  XMd5sum :: Loc -> X Checksum

  XHash :: String -> X Checksum
  XMakeDir :: Loc -> X ()
  XGlob :: Loc -> X [Loc]
  XExists :: Loc -> X Bool
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
        withCurrentDirectory dir (callCommand command)

      -- other commands with shell out to external process
      XMd5sum (Loc fp) -> do
        let command = printf "md5sum %s" fp
        logX command
        output <- readCreateProcess (shell command) ""
        let sum = case (splitOn " " output) of [] -> undefined; x:_ -> x
        pure (Checksum sum)

      -- internal file system access (log equivalent external command)
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
      XExists (Loc fp) -> do
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
