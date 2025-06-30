
module UserMain (main) where

import Data.Char qualified as Char
import Data.List (intercalate)
import System.FilePath qualified as FP
import Text.Printf (printf)

import Interface(G(..),D(..),Rule(..),Key(..),Loc(..),(</>),dirLoc)
import Engine (engineMain)
import ElabSimpleMake qualified

main :: IO ()
main = engineMain $ \dirs -> do
  case dirs of
    [] -> GFail "UserMain: nothing to build"
    _ -> mapM_ dispatchAllConfigs dirs

dispatchAllConfigs :: Loc -> G ()
dispatchAllConfigs dir = do
  configNames <- listBaseNamesWithSuffix dir ".jenga"
  mapM_ (dispatch1 dir) configNames

dispatch1 :: Loc -> String -> G ()
dispatch1 dir fullName = do
  let func = dispatch (FP.takeFileName fullName)
  let config = Loc (fullName ++ ".jenga")
  configContents <- GReadKey (Key config)
  func dir config configContents

dispatch :: String -> (Loc -> Loc -> String -> G ())
dispatch = \case
  "cc-basic" -> configCCbasic -- no dep discovery; just for example1
  "cc" -> configCCdepDiscovery
  "simple-make" -> doSimpleMake
  name ->
    \_ _ _ -> GFail $ printf "unknown jenga config file: %s.jenga" name

doSimpleMake :: Loc -> Loc -> String -> G ()
doSimpleMake dir _configFile configContents = do
  ElabSimpleMake.elab dir configContents

-- TODO: CC stuff to own file
configCCbasic :: Loc -> Loc -> String -> G ()
configCCbasic dir config configContents = do
  main <- Key <$> parseSingleName config configContents
  GArtifact main
  xs <- listBaseNamesWithSuffix dir ".c"
  mapM_ setupCrule xs
  setupLinkRule main xs

configCCdepDiscovery :: Loc -> Loc -> String -> G ()
configCCdepDiscovery dir config configContents = do
  main <- Key <$> parseSingleName config configContents
  GArtifact main
  xs <- listBaseNamesWithSuffix dir ".c"
  mapM_ setupCruleAuto xs
  setupLinkRule main xs

parseSingleName :: Loc -> String -> G Loc
parseSingleName loc str =
  case lines str of
    [] -> GFail $ printf "parseSingleName(%s): no lines" (show loc)
    _:_:_ -> GFail $ printf "parseSingleName(%s): unexpected multiple lines" (show loc)
    [line] ->
      if any badChar line
      then GFail (printf "parseSingleName(%s): Bad name: '%s'" (show loc) line)
      else do
        let dir = dirLoc loc
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
    , depcom = do
        mapM_ DNeed obs
        pure $ printf "gcc %s -o %s" (baseKeys obs) (baseKey exe)
    }

setupCrule :: String -> G ()
setupCrule x = do
  let c = cKey x
  let o = oKey x
  GRule $ Rule
    { tag = printf "CC-%s" x
    , targets = [o]
    , depcom = do
        DNeed c
        pure $ printf "gcc -c %s -o %s" (baseKey c) (baseKey o)
    }

-- TODO: capture common pattern
cKey :: String -> Key
cKey x = Key (Loc (x++".c"))

oKey :: String -> Key
oKey x = Key (Loc (x++".o"))

dKey :: String -> Key
dKey x = Key (Loc (x++".d"))

setupCruleAuto :: String -> G ()
setupCruleAuto x = do
  let c = cKey x
  let o = oKey x
  let d = dKey x
  GRule $ Rule
    { tag = printf "cc-with-dep-discovery:%s.o" x
    , targets = [o]
    , depcom = do
        DNeed d
        deps <- readDepsFile d
        mapM_ DNeed deps
        pure $ printf "gcc -c %s -o %s" (baseKey c) (baseKey o)
    }
  GRule $ Rule
    { tag = printf "cc-with-dep-discovery:%s.d" x
    , targets = [d]
    , depcom = do
        DNeed c
        pure $ printf "gcc -MG -MM %s -MF %s" (baseKey c) (baseKey d)
    }

readDepsFile :: Key -> D [Key]
readDepsFile key = do
  let Key loc = key
  let dir = dirLoc loc
  names <- parseDepsFile <$> DReadKey key
  pure [ Key (dir </> name) | name <- names ]

parseDepsFile :: String -> [String]
parseDepsFile contents =
  case words contents of
    [] -> error (show ("parseDepsFile",contents))
    _:xs -> xs

----------------------------------------------------------------------
-- rule stdlib util code

listBaseNamesWithSuffix :: Loc -> String -> G [String]
listBaseNamesWithSuffix dir sought = do
  locs <- GGlob dir
  pure [ base
       | Loc x <- locs
       , FP.hasExtension x
       , (base,suf) <- [FP.splitExtensions x]
       , suf == sought
       ]

baseKeys :: [Key] -> String
baseKeys = intercalate " " . map baseKey

baseKey :: Key -> String
baseKey (Key (Loc fp)) = FP.takeFileName fp
