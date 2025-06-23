
module Top (main) where

import Control.Monad (ap,liftM)
import Data.Char qualified as Char
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Directory (listDirectory,createDirectoryIfMissing,withCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath qualified as FP
import System.Posix.Files (fileExist,createLink,removeLink)
import System.Process (callCommand,shell,readCreateProcess)
import Text.Printf (printf)

main :: IO ()
main = jengaMain $ do
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
    , targets = [exe]
    , deps = obs
    , action = Action
      { command = printf "gcc %s -o %s" (baseKeys obs) (baseKey exe)
      , inputs = substore obs
      , outputs = substore [exe] }}

setupCrule :: String -> G ()
setupCrule x = do
  let c = cKey x
  let o = oKey x
  GRule $ Rule
    { tag = printf "CC-%s" x
    , targets = [o]
    , deps = [c]
    , action = Action
      { command = printf "gcc -c %s -o %s" (baseKey c) (baseKey o)
      , inputs = substore [c]
      , outputs = substore [o] }}

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

substore :: [Key] -> SubStore
substore ks = SubStore (Map.fromList [ (baseRelPath rp, k)
                                     | k@(Key rp) <- ks ])

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

data Rule = Rule { tag :: String, targets :: [Key], deps :: [Key], action :: Action }

data Key = Key RelPath deriving (Eq,Ord)

data Action = Action
  { command :: String
  , inputs :: SubStore
  , outputs :: SubStore
  }

data SubStore = SubStore (Map RelPath Key)

data RelPath = RelPath String deriving (Eq,Ord)

----------------------------------------------------------------------
-- show

instance Show Key where show (Key rp) = show rp

instance Show Rule where show Rule{tag} = tag

instance Show ErrMessage where show (ErrMessage s) = printf "Error: %s" s

instance Show Checksum where show (Checksum sum) = sum

instance Show RelPath where show (RelPath fp) = fp

----------------------------------------------------------------------
-- jenga main

jengaMain :: G () -> IO ()
jengaMain userDefs = do
  config <- parseCommandLine <$> getArgs
  generateAndBuild config userDefs

----------------------------------------------------------------------
-- Config: control logging of stages: Generate/Build/Execite

data Config = Config
  { seeA :: Bool
  , seeB :: Bool
  , seeX :: Bool
  }

parseCommandLine :: [String] -> Config
parseCommandLine = loop config0
  where
    config0 = Config { seeA = False, seeB = False, seeX = False }
    loop :: Config -> [String] -> Config
    loop config = \case
      [] -> config
      "-a":xs      -> loop config { seeA = True } xs
      "-b":xs      -> loop config { seeB = True } xs
      "-x":xs      -> loop config { seeX = True } xs
      ('-':flag):_ -> error (show ("unknown flag",flag))
      x:_          -> error (show ("unknown arg",x))

----------------------------------------------------------------------
-- Generate

generateAndBuild :: Config -> G () -> IO ()
generateAndBuild config m = do
  runB config (runGenerate config m) >>= \case
    Left mes -> printf "go -> Error:\n%s\n" (show mes)
    Right system -> do
      let System{roots} = system
      runB config $ do
        sums <- doBuild config system roots
        sequence_ [ materialize sum key | (sum,key) <- zip sums roots ]

data System = System { roots :: [Key], sources :: [Key], rules :: [Rule] }

type OrErr a = Either ErrMessage a
data ErrMessage = ErrMessage String

runGenerate :: Config -> G () -> B (OrErr System)
runGenerate config m = loop m sys0 k0
  where
    sys0 :: System
    sys0 = System { sources = [], rules = [], roots = [] }
    k0 :: System -> () -> B (OrErr System)
    k0 s () = pure (Right s)
    loop :: G a -> System -> (System -> a -> B (OrErr System)) -> B (OrErr System)
    loop m s k = case m of
      GRet a -> k s a
      GBind m f -> loop m s $ \s a -> loop (f a) s k
      GFail mes -> pure (Left (ErrMessage mes)) -- ignore k
      -- TODO: as we add rule/source, check no previous def
      GSource key -> do
        let System{sources} = s
        k s { sources = key : sources } ()
      GRule rule -> do
        let System{rules} = s
        k s { rules = rule : rules } ()
      GRoot key -> do
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

jengaDir,cacheDir,sandboxDir,materializeDir,witDir :: RelPath
jengaDir = RelPath ",jenga"
cacheDir = jengaDir </> "cache"
sandboxDir = jengaDir </> "box"
materializeDir = jengaDir </> "artifacts"
witDir = jengaDir </> "witness"

data RuleOrSource = R Rule Int | S RelPath

data Witness = Witness { key :: WitnessKey, val :: WitnessValue }

data WitnessKey = WitnessKey { command :: String, deps :: StoreWit } deriving Show

data WitnessValue = WitnessValue { wtargets :: StoreWit } -- TODO: and stdout?

data StoreWit = StoreWit (Map RelPath Checksum) deriving Show

data Checksum = Checksum String

materialize :: Checksum -> Key -> B ()
materialize (Checksum sum) (Key rp) = do
  let cacheFile = cacheDir </> sum
  let materializedFile = materializeDir </> show rp
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
    log mes = if not seeB then pure () else BLog (printf "B: %s" mes)

    xs1 = [ (key, S rp) | key@(Key rp) <- sources ]
    xs2 = [ (key, R rule i) | rule@Rule{targets} <- rules, (i,key) <- zip [0..] targets ]
    -- TODO: check no duplicate ways to build a key
    how :: Map Key RuleOrSource
    how = Map.fromList (xs1 ++ xs2)

    demand :: Key -> B Checksum
    demand key = do
      log $ printf "Demand: %s" (show key)
      case Map.lookup key how of
        Nothing -> do
          -- TODO: this needs to be a softer error
          error (printf "dont know how to build key: %s" (show key))
        Just rs ->
          case rs of
            S rp -> do
              log $ printf "Source: %s" (show key)
              checksum <- Execute (insertIntoCache rp)
              pure checksum

            R rule i -> do
              log $ printf "Consult: %s" (show rule)
              let Rule{deps,action,targets} = rule
              let Action{inputs,command,outputs} = action
              depSums <- mapM demand deps
              let witKey = WitnessKey
                    { command = command
                    , deps = makeStoreWit deps depSums inputs
                    }
              wks <- hashWitnessKey witKey
              --log $ printf "Compute WKS: %s" (show wks)
              verifyWitness wks >>= \case
                Just wit -> do
                  let Witness{val} = wit
                  let WitnessValue{wtargets} = val
                  pure (checksumOfKey outputs wtargets key)
                Nothing -> do
                  log $ printf "Build: %s" (show rule)
                  targetSums <- buildWithRule depSums rule
                  let val = WitnessValue
                        { wtargets = makeStoreWit targets targetSums outputs }
                  let wit = Witness { key = witKey, val }
                  --log $ printf "Witness: %s" (show rule)
                  saveWitness wks wit
                  let checksum = targetSums!!i
                  pure checksum

makeStoreWit :: [Key] -> [Checksum] -> SubStore -> StoreWit
makeStoreWit keys sums (SubStore m1) = do
  let xs = zip keys sums
  let m :: Map Key Checksum
      m = Map.fromList xs
  let look :: Key -> Checksum
      look k = maybe undefined id $ Map.lookup k m
  StoreWit (Map.fromList [ (rp,look key) | (rp,key) <- Map.toList m1 ])

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
  let witFile = witDir </> show wks
  XExists witFile >>= \case
    False -> pure Nothing
    True -> do
      contents <- XReadFile witFile
      pure $ Just (exportWitness contents)

existsCacheFile :: Checksum -> B Bool
existsCacheFile checksum = Execute (XExists (cacheFile checksum))

checksumOfKey :: SubStore -> StoreWit -> Key -> Checksum
checksumOfKey (SubStore m1) (StoreWit m2) sought = do
  let the = \case [] -> error "checksumOfKey/m1"; x:_ -> x
  let look rp = maybe (error "checksumOfKey/m2") id $ Map.lookup rp m2
  look (the [ rp | (rp, key) <- Map.toList m1, key == sought ])

saveWitness :: WitKeySum -> Witness -> B ()
saveWitness wks wit = do
  let witFile = witDir </> show wks
  Execute $ do
    XMakeDir (dirRelPath witFile)
    XWrite (importWitness wit ++ "\n") witFile

buildWithRule :: [Checksum] -> Rule -> B [Checksum]
buildWithRule depSums rule = do
  sandbox <- NewSandbox
  let Rule{deps,action} = rule
  let Action{command,inputs,outputs} = action
  Execute (XMakeDir sandbox)
  Execute (setupInputs sandbox inputs deps depSums)
  Execute (XRunCommandInDir sandbox command)
  targetSums <- Execute (cacheOutputs sandbox outputs)
  -- TODO: remove the sandbox
  pure targetSums

setupInputs :: RelPath -> SubStore -> [Key] -> [Checksum] -> X ()
setupInputs sandbox (SubStore m1) keys sums = do
  let m2 :: Map Key Checksum = Map.fromList (zip keys sums)
  let look :: Key -> Checksum
      look k = maybe undefined id $ Map.lookup k m2
  let
    setupDep :: RelPath -> Key -> X ()
    setupDep rp key = do
      let checksum = look key
      let inputFile = sandbox </> show rp
      XHardLink (cacheFile checksum) inputFile
  sequence_ [ setupDep fp key | (fp,key) <- Map.toList m1 ]

cacheOutputs :: RelPath -> SubStore -> X [Checksum]
cacheOutputs sandbox (SubStore m1) = do
  let
    cacheOutput :: RelPath -> X Checksum
    cacheOutput rp = do
      let outputFile = sandbox </> show rp
      insertIntoCache outputFile
  sequence [ cacheOutput fp | (fp,_) <- Map.toList m1 ]

insertIntoCache :: RelPath -> X Checksum
insertIntoCache rp = do
  checksum <- XMd5sum rp
  let file = cacheFile checksum
  XExists file >>= \case
    True -> pure ()
    False -> do
      XCopy rp file
      XMakeReadOnly file
  pure checksum

cacheFile :: Checksum -> RelPath
cacheFile (Checksum sum) = cacheDir </> sum

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
  let WitnessKey{command,deps} = key
  let WitnessValue{wtargets} = val
  let fromStore (StoreWit m) = [ (fp,sum) | (RelPath fp,Checksum sum) <- Map.toList m ]
  WIT { command, deps = fromStore deps, targets = fromStore wtargets }

fromQ :: QWitness -> Witness
fromQ WIT{command,deps,targets} = do
  let toStore xs = StoreWit (Map.fromList [ (RelPath fp,Checksum sum) | (fp,sum) <- xs ])
  let key = WitnessKey{command,deps = toStore deps}
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
  XMakeDir jengaDir
  XMakeDir cacheDir
  XMakeDir sandboxDir
  XMakeDir materializeDir -- TODO: delete everything materialized before
  XMakeDir witDir
  loop b 0 k0
  where
    k0 :: Int -> a -> X a
    k0 i a = do
      XLog (printf "ran %d actions" i)
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
  XGlob :: RelPath -> X [RelPath]
  XReadFile :: RelPath -> X String
  XRunCommandInDir :: RelPath -> String -> X ()
  XMd5sum :: RelPath -> X Checksum
  XHash :: String -> X Checksum
  XWrite :: String -> RelPath -> X ()
  XCopy :: RelPath -> RelPath -> X ()
  XHardLink :: RelPath -> RelPath -> X ()
  XRemovelink :: RelPath -> X ()
  XExists :: RelPath -> X Bool
  XMakeDir :: RelPath -> X ()
  XMakeReadOnly :: RelPath -> X ()

runX :: Config -> X a -> IO a
runX Config{seeA,seeX} = loop
  where
    log,logA :: String -> IO ()
    log mes = if not seeX then pure () else printf "X: %s\n" mes
    logA mes = if (not seeX && not seeA) then pure () else printf "A: %s\n" mes

    loop :: X a -> IO a
    loop x = case x of
      XRet a -> pure a
      XBind m f -> do a <- loop m; loop (f a)
      XLog s -> do putStrLn s
      XGlob (RelPath fp) -> do
        log $ printf "ls %s" fp
        xs <- listDirectory fp
        pure [ RelPath fp </> x | x <- xs ]
      XReadFile (RelPath p) -> do
        log $ printf "read %s" p
        readFile p
      XRunCommandInDir (RelPath dir) command -> do
        logA $ printf "cd %s; %s" dir command
        withCurrentDirectory dir (callCommand command)
      XMd5sum (RelPath fp) -> do
        let command = printf "md5sum %s" fp
        log command
        output <- readCreateProcess (shell command) ""
        let sum = case (splitOn " " output) of [] -> undefined; x:_ -> x
        pure (Checksum sum)
      XHash contents -> do
        let command = printf "echo '%s' | md5sum" contents
        --log command
        output <- readCreateProcess (shell command) ""
        let sum = case (splitOn " " output) of [] -> undefined; x:_ -> x
        pure (Checksum sum)
      XWrite contents (RelPath dest) -> do
        --log $ printf "echo '%s' > %s" contents dest
        log $ printf "write %s" dest
        writeFile dest contents
      XCopy (RelPath src) (RelPath dest) -> do
        let command = printf "cp %s %s" src dest
        log command
        callCommand command
      XHardLink (RelPath src) (RelPath dest) -> do
        log $ printf "ln %s %s" src dest
        createLink src dest
      XRemovelink (RelPath rp) -> do
        log $ printf "rm %s" rp
        removeLink rp
      XExists (RelPath fp) -> do
        --log $ printf "?%s" fp
        fileExist fp
      XMakeDir (RelPath fp) -> do
        --log $ printf "mkdir %s" fp
        createDirectoryIfMissing True fp
      XMakeReadOnly (RelPath fp) -> do
        let command = printf "chmod a-w %s" fp
        log command
        callCommand command
