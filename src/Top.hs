
module Top (main) where

import Control.Monad (ap,liftM)
import Control.Monad (when)
import Data.Char qualified as Char
import Data.Hash.MD5 qualified as MD5
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory,removePathForcibly,copyFile)
import System.Environment (getArgs)
import System.FilePath qualified as FP
import System.Posix.Files (fileExist,createLink,removeLink,getFileStatus,fileMode,intersectFileModes,setFileMode)
import System.Process (callCommand,shell,readCreateProcess)
import Text.Printf (printf)

main :: IO ()
main = engineMain $ do
  let dir = RelPath "example"
  let config = dir </> "config"
  configContents <- readSourceDefaulting "default.exe" config
  main <- Key <$> parseSingleName config configContents
  GRoot main
  xs <- listBaseNamesWithSuffix dir ".c"
  mapM_ (GSource . cKey) xs
  mapM_ setupCrule xs
  setupLinkRule main xs
  pure ()

parseSingleName :: RelPath -> String -> G RelPath
parseSingleName rp str =
  case lines str of
    [] -> GFail $ printf "parseSingleName(%s): no lines" (show rp)
    _:_:_ -> GFail $ printf "parseSingleName(%s): unexpected multiple lines" (show rp)
    [line] ->
      if any badChar line
      then GFail (printf "parseSingleName(%s): Bad name: '%s'" (show rp) line)
      else do
        let dir = dirRelPath rp
        pure (dir </> line)
      where
        badChar c = c == '/' || n < 33 || n > 126
          where n = Char.ord c

----------------------------------------------------------------------
-- rule stdlib common code -- build/link C

setupLinkRule :: Key -> [String] -> G ()
setupLinkRule exe xs =
  if length xs == 0 then GFail (printf "setupLinkRule(%s):no objects" (show exe)) else do
  let obs = [ oKey x | x <- xs ]
  GRule $ Rule
    { tag = printf "LINK-%s" (show exe)
    , targets = locateKeys [exe]
    , deps = locateKeys obs
    , command = printf "gcc %s -o %s" (baseKeys obs) (baseKey exe)
    }

setupCrule :: String -> G ()
setupCrule x = do
  let c = cKey x
  let o = oKey x
  GRule $ Rule
    { tag = printf "CC-%s" x
    , targets = locateKeys [o]
    , deps = locateKeys [c]
    , command = printf "gcc -c %s -o %s" (baseKey c) (baseKey o)
    }

locateKeys :: [Key] -> KeyMap
locateKeys ks = KeyMap (Map.fromList [ (k, baseRelPath rp) | k@(Key rp) <- ks ])

cKey :: String -> Key
cKey x = Key (RelPath (x++".c"))

oKey :: String -> Key
oKey x = Key (RelPath (x++".o"))

----------------------------------------------------------------------
-- rule stdlib util code

readSourceDefaulting :: String -> RelPath -> G String
readSourceDefaulting def path = do
  GExists path >>= \case
    False -> pure def
    True -> do
      let key = Key path
      GSource key
      GReadKey key

listBaseNamesWithSuffix :: RelPath -> String -> G [String]
listBaseNamesWithSuffix dir sought = do
  rps <- GGlob dir
  pure [ base
       | RelPath x <- rps
       , FP.hasExtension x
       , (base,suf) <- [FP.splitExtensions x]
       , suf == sought
       ]

baseKeys :: [Key] -> String
baseKeys = intercalate " " . map baseKey

baseKey :: Key -> String
baseKey (Key (RelPath fp)) = FP.takeFileName fp

----------------------------------------------------------------------
-- build tool interface

instance Functor G where fmap = liftM
instance Applicative G where pure = GRet; (<*>) = ap
instance Monad G where (>>=) = GBind

data G a where
  GRet :: a -> G a
  GBind :: G a -> (a -> G b) -> G b
  GFail :: String -> G a
  GRoot :: Key -> G ()
  GSource :: Key -> G ()
  GRule :: Rule -> G ()
  GGlob :: RelPath -> G [RelPath]
  GExists :: RelPath -> G Bool
  GReadKey :: Key -> G String

data Rule = Rule
  { tag :: String
  , targets :: KeyMap
  , deps :: KeyMap
  , command :: String
  }

data Key = Key RelPath deriving (Eq,Ord)

data KeyMap = KeyMap (Map Key RelPath)

data RelPath = RelPath String deriving (Eq,Ord) -- TODO: renamae Location -- doc is relative

----------------------------------------------------------------------
-- show

instance Show Key where show (Key rp) = show rp
instance Show Rule where show Rule{tag} = tag
instance Show ErrMessage where show (ErrMessage s) = printf "Error: %s" s
instance Show Checksum where show (Checksum sum) = sum
instance Show RelPath where show (RelPath fp) = fp

----------------------------------------------------------------------
-- directories locations for cache, sandbox etc

jengaCache,cachedFilesDir,tracesDir :: RelPath
jengaCache = RelPath ".cache" -- TODO would be better shared at: ~/.cache/jenga/
cachedFilesDir = jengaCache </> "files"
tracesDir = jengaCache </> "traces"

jengaDir,sandboxDir,artifactsDir :: RelPath
jengaDir = RelPath ",jenga"
sandboxDir = jengaDir </> "box"
artifactsDir = jengaDir </> "artifacts"

----------------------------------------------------------------------
-- Engine main

engineMain :: G () -> IO ()
engineMain userDefs = do
  config <- parseCommandLine <$> getArgs
  generateAndBuild config userDefs

generateAndBuild :: Config -> G () -> IO ()
generateAndBuild config m = do
  runB config (runGenerate config m) >>= \case
    Left mes -> printf "go -> Error:\n%s\n" (show mes)
    Right system -> do
      let System{roots,rules} = system
      printf "elaborated %s and %s\n"
        (pluralize (length rules) "rule")
        (pluralize (length roots) "root")
      runB config $ do
        sums <- doBuild config system roots
        sequence_ [ materialize sum key | (sum,key) <- zip sums roots ]

pluralize :: Int -> String -> String
pluralize n what = printf "%d %s%s" n what (if n == 1 then "" else "s")

----------------------------------------------------------------------
-- Command line: control logging

data Config = Config
  { seeE :: Bool -- log steps for elaboration of rules and roots
  , seeB :: Bool -- log steps for bringing a build up to date
  , seeA :: Bool -- log execution of user build commands
  , seeX :: Bool -- log execution of other externally run commands
  , seeI :: Bool -- log execution of internal file system access (i.e not shelling out)
  }

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    config0 = Config False False False False False
    loop :: Config -> [String] -> Config
    loop config = \case
      [] -> config
      "-e":xs      -> loop config { seeE = True } xs
      "-b":xs      -> loop config { seeB = True } xs
      "-a":xs      -> loop config { seeA = True } xs
      "-x":xs      -> loop config { seeX = True } xs
      "-i":xs      -> loop config { seeI = True } xs
      ('-':flag):_ -> error (show ("unknown flag",flag))
      x:_          -> error (show ("unknown arg",x))

----------------------------------------------------------------------
-- Generate

data System = System { roots :: [Key], sources :: [Key], rules :: [Rule] }

type OrErr a = Either ErrMessage a
data ErrMessage = ErrMessage String

runGenerate :: Config -> G () -> B (OrErr System) -- TODO: rename elaborate?
runGenerate config@Config{seeE} m = loop m sys0 k0
  where
    sys0 :: System
    sys0 = System { sources = [], rules = [], roots = [] }
    k0 :: System -> () -> B (OrErr System)
    k0 s () = pure (Right s)

    logE :: String -> B ()
    logE mes = when seeE $ BLog (printf "E: %s" mes)

    loop :: G a -> System -> (System -> a -> B (OrErr System)) -> B (OrErr System)
    loop m s k = case m of
      GRet a -> k s a
      GBind m f -> loop m s $ \s a -> loop (f a) s k
      GFail mes -> pure (Left (ErrMessage mes)) -- ignore k
      -- TODO: as we add rule/source, check no previous def
      GSource key -> do
        logE $ printf "Elaborate source: %s" (show key)
        let System{sources} = s
        k s { sources = key : sources } ()
      GRule rule -> do
        logE $ printf "Elaborate rule: %s" (show rule)
        let System{rules} = s
        k s { rules = rule : rules } ()
      GRoot key -> do
        logE $ printf "Elaborate root: %s" (show key)
        let System{roots} = s
        k s { roots = key : roots } ()
      GGlob dir -> do
        rps <- Execute (XGlob dir)
        k s rps
      GExists rp -> do
        bool <- Execute (XExists rp)
        k s bool
      GReadKey key -> do
        let system = s
        _ <- doBuild config system [key]
        let Key rp = key
        contents <- Execute (XReadFile rp)
        k s contents

----------------------------------------------------------------------
-- Build

data RuleOrSource = R Rule RelPath | S RelPath -- TODO: better name?

data Witness = Witness { key :: WitnessKey, val :: WitnessValue }

data WitnessKey = WitnessKey { command :: String, wdeps :: StoreWit } deriving Show

data WitnessValue = WitnessValue { wtargets :: StoreWit }

data StoreWit = StoreWit (Map RelPath Checksum) deriving Show -- TODO: better name WitMap

data Checksum = Checksum String

materialize :: Checksum -> Key -> B ()
materialize (Checksum sum) (Key rp) = do
  let cacheFile = cachedFilesDir </> sum
  let materializedFile = artifactsDir </> show rp
  Execute $ do
    XMakeDir (dirRelPath materializedFile)
    XExists materializedFile >>= \case
      False -> pure ()
      True -> XRemovelink materializedFile
    XHardLink cacheFile materializedFile

doBuild :: Config -> System -> [Key] -> B [Checksum]
doBuild Config{seeB} System{sources,rules} roots = mapM demand roots
  where

    log :: String -> B ()
    log mes = when seeB $ BLog (printf "B: %s" mes)

    xs1 = [ (key, S rp) | key@(Key rp) <- sources ]
    xs2 = [ (key, R rule rp) | rule@Rule{targets=KeyMap m} <- rules, (key,rp) <- Map.toList m ]

    -- TODO: check no duplicate ways to build a key; better still caller will have checked
    how :: Map Key RuleOrSource
    how = Map.fromList (xs1 ++ xs2)

    demand :: Key -> B Checksum
    demand key = do
      log $ printf "Require: %s" (show key)
      case Map.lookup key how of
        Nothing -> do
          -- TODO: this needs to be a softer error
          error (printf "dont know how to build key: %s" (show key))
        Just rs ->
          case rs of
            S rp -> do
              checksum <- Execute (insertIntoCache rp)
              pure checksum

            -- TODO: document this flow...
            R rule rpTarget -> do
              log $ printf "Consult: %s" (show rule)
              let Rule{command,targets,deps=KeyMap deps} = rule

              wdeps <- (StoreWit . Map.fromList) <$>
                sequence [ do checksum <- demand key; pure (rp,checksum)
                         | (key,rp) <- Map.toList deps
                         ]

              let witKey = WitnessKey { command, wdeps }
              wks <- hashWitnessKey witKey
              verifyWitness wks >>= \case
                Just Witness{val=WitnessValue{wtargets}} -> do
                  pure (lookStoreWit (lookKeyMap key targets) wtargets)

                Nothing -> do
                  log $ printf "Execute: %s" (show rule)
                  wtargets <- buildWithRule wdeps rule
                  let val = WitnessValue { wtargets }
                  let wit = Witness { key = witKey, val }
                  saveWitness wks wit
                  let checksum = lookStoreWit rpTarget wtargets
                  pure checksum

lookStoreWit :: RelPath -> StoreWit -> Checksum
lookStoreWit rp (StoreWit m) = maybe err id $ Map.lookup rp m
  where err = error "lookStoreWit"

lookKeyMap :: Key -> KeyMap -> RelPath
lookKeyMap key (KeyMap m) = maybe err id $ Map.lookup key m
  where err = error "lookKeyMap"

data WitKeySum = WitKeySum String

instance Show WitKeySum where show (WitKeySum sum) = sum

hashWitnessKey :: WitnessKey -> B WitKeySum
hashWitnessKey wk = do
  Checksum sum <- Execute (XHash (show wk))
  pure (WitKeySum sum)

verifyWitness :: WitKeySum -> B (Maybe Witness)
verifyWitness wks = do
  lookupWitness wks >>= \case
    Nothing -> pure Nothing
    Just wit -> do
      let Witness{val} = wit
      let WitnessValue{wtargets=StoreWit m} = val
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
    XMakeDir (dirRelPath witFile)
    XWriteFile (importWitness wit ++ "\n") witFile

buildWithRule :: StoreWit -> Rule -> B StoreWit
buildWithRule depWit rule = do
  sandbox <- NewSandbox
  let Rule{command,targets} = rule
  Execute (XMakeDir sandbox)
  Execute (setupInputs sandbox depWit)
  Execute (XRunCommandInDir sandbox command)
  targetWit <- Execute (cacheOutputs sandbox targets)
  Execute (XRemoveDirRecursive sandbox)
  pure targetWit

setupInputs :: RelPath -> StoreWit -> X ()
setupInputs sandbox (StoreWit m1) = do
  sequence_
    [ XHardLink (cacheFile checksum) (sandbox </> show rp)
    | (rp,checksum) <- Map.toList m1
    ]

cacheOutputs :: RelPath -> KeyMap -> X StoreWit
cacheOutputs sandbox (KeyMap m1) = do
  StoreWit . Map.fromList <$> sequence
    [ do
        checksum <- insertIntoCache (sandbox </> show rp)
        pure (rp,checksum)
    | (_,rp) <- Map.toList m1
    ]

insertIntoCache :: RelPath -> X Checksum
insertIntoCache rp = do
  checksum <- XMd5sum rp
  let file = cacheFile checksum
  XExists file >>= \case
    True -> pure ()
    False -> do
      XCopyFile rp file
      XMakeReadOnly file
  pure checksum

cacheFile :: Checksum -> RelPath
cacheFile (Checksum sum) = cachedFilesDir </> sum

(</>) :: RelPath -> String -> RelPath
(</>) (RelPath dir) filename = RelPath (dir FP.</> filename)

dirRelPath :: RelPath -> RelPath
dirRelPath (RelPath s) = RelPath (FP.takeDirectory s)

baseRelPath :: RelPath -> RelPath
baseRelPath (RelPath s) = RelPath (FP.takeFileName s)

----------------------------------------------------------------------
-- export/import Witness data in fixed format using flatter type

exportWitness :: String -> Witness
exportWitness = fromQ . read

importWitness :: Witness -> String
importWitness = show . toQ

data QWitness = WIT
  { command :: String
  , deps :: QStoreWit
  , targets :: QStoreWit
  }
  deriving (Show,Read)

type QStoreWit = [(FilePath,QChecksum)]
type QChecksum = String

toQ :: Witness -> QWitness
toQ wit = do
  let Witness{key,val} = wit
  let WitnessKey{command,wdeps} = key
  let WitnessValue{wtargets} = val
  let fromStore (StoreWit m) = [ (fp,sum) | (RelPath fp,Checksum sum) <- Map.toList m ]
  WIT { command, deps = fromStore wdeps, targets = fromStore wtargets }

fromQ :: QWitness -> Witness
fromQ WIT{command,deps,targets} = do
  let toStore xs = StoreWit (Map.fromList [ (RelPath fp,Checksum sum) | (fp,sum) <- xs ])
  let key = WitnessKey{command,wdeps = toStore deps}
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
  NewSandbox :: B RelPath
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

  XRunCommandInDir :: RelPath -> String -> X ()
  XMd5sum :: RelPath -> X Checksum

  XHash :: String -> X Checksum
  XMakeDir :: RelPath -> X ()
  XGlob :: RelPath -> X [RelPath]
  XExists :: RelPath -> X Bool
  XCopyFile :: RelPath -> RelPath -> X ()
  XMakeReadOnly :: RelPath -> X ()
  XReadFile :: RelPath -> X String
  XWriteFile :: String -> RelPath -> X ()
  XHardLink :: RelPath -> RelPath -> X ()
  XRemovelink :: RelPath -> X ()
  XRemoveDirRecursive :: RelPath -> X ()

runX :: Config -> X a -> IO a
runX Config{seeA,seeX,seeI} = loop
  where
    logA,logX,logI :: String -> IO ()
    logA mes = when seeA $ printf "A: %s\n" mes
    logX mes = when seeX $ printf "X: %s\n" mes
    logI mes = when seeI $ printf "I: %s\n" mes

    loop :: X a -> IO a
    loop x = case x of
      XRet a -> pure a
      XBind m f -> do a <- loop m; loop (f a)
      XLog s -> do putStrLn s

      -- sandboxed execution of user command
      XRunCommandInDir (RelPath dir) command -> do
        logA $ printf "cd %s; %s" dir command
        withCurrentDirectory dir (callCommand command)

      -- other commands with shell out to external process
      XMd5sum (RelPath fp) -> do
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
      XMakeDir (RelPath fp) -> do
        logI $ printf "mkdir -p %s" fp
        createDirectoryIfMissing True fp
      XGlob (RelPath fp) -> do
        logI $ printf "ls %s" fp
        xs <- listDirectory fp
        pure [ RelPath fp </> x | x <- xs ]
      XExists (RelPath fp) -> do
        logI $ printf "test -e %s" fp
        fileExist fp
      XCopyFile (RelPath src) (RelPath dest) -> do
        logI $ printf "cp %s %s" src dest
        copyFile src dest
      XMakeReadOnly (RelPath fp) -> do
        logI $ printf "chmod a-w %s" fp
        old_mode <- fileMode <$> getFileStatus fp
        let new_mode = intersectFileModes 0o555 old_mode
        setFileMode fp new_mode
      XReadFile (RelPath p) -> do
        logI $ printf "cat %s" p
        readFile p
      XWriteFile contents (RelPath dest) -> do
        logI $ printf "cat> %s" dest
        writeFile dest contents
      XHardLink (RelPath src) (RelPath dest) -> do
        logI $ printf "ln %s %s" src dest
        createLink src dest
      XRemovelink (RelPath rp) -> do
        logI $ printf "rm %s" rp
        removeLink rp
      XRemoveDirRecursive (RelPath fp) -> do
        logI $ printf "rm -rf %s" fp
        removePathForcibly fp
