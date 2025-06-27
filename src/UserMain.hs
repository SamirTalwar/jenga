
module UserMain (main) where

import Data.Char qualified as Char
import Data.List (intercalate)
import Data.Map qualified as Map
import System.FilePath qualified as FP
import Text.Printf (printf)

import Interface(G(..),D(..),Rule(..),KeyMap(..),Key(..),Loc(..),(</>),dirLoc,baseLoc)
import Engine (engineMain)

main :: IO ()
main = engineMain $ \args -> do
  case args of
    [] -> GFail "UserMain: nothing to build"
    _ -> sequence_ [ build (Loc arg ) | arg <- args ]

build :: Loc -> G ()
build dir = do
  let config = dir </> "config"
  configContents <- readSourceDefaulting "default.exe" config
  main <- Key <$> parseSingleName config configContents
  GRoot main
  xs <- listBaseNamesWithSuffix dir ".c"
  mapM_ (GSource . cKey) xs
  mapM_ setupCrule xs
  setupLinkRule main xs
  pure ()

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
    , targets = locateKeys [exe]
    , depcom = do
        mapM_ need obs
        pure $ printf "gcc %s -o %s" (baseKeys obs) (baseKey exe)
    }

setupCrule :: String -> G ()
setupCrule x = do
  let c = cKey x
  let o = oKey x
  GRule $ Rule
    { tag = printf "CC-%s" x
    , targets = locateKeys [o]
    , depcom = do
        need c
        pure $ printf "gcc -c %s -o %s" (baseKey c) (baseKey o)
    }

locateKeys :: [Key] -> KeyMap
locateKeys ks = KeyMap (Map.fromList [ (k, baseLoc loc) | k@(Key loc) <- ks ])

cKey :: String -> Key
cKey x = Key (Loc (x++".c"))

oKey :: String -> Key
oKey x = Key (Loc (x++".o"))

need :: Key -> D ()
need key@(Key loc) = DNeed key (baseLoc loc)

----------------------------------------------------------------------
-- rule stdlib util code

readSourceDefaulting :: String -> Loc -> G String
readSourceDefaulting def path = do
  GExists path >>= \case
    False -> pure def
    True -> do
      let key = Key path
      GSource key
      GReadKey key

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

