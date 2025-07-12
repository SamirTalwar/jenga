module ElabC (macroC) where

import Interface (G(..),Rule(..),Action(..),D(..),Key(..),Loc)
import StdBuildUtils (dirKey,listBaseNamesWithSuffix,mkKey,baseKeys,baseKey,(</>))
import Text.Printf (printf)
import Data.List (intercalate)

macroC :: Key -> G ()
macroC exe = do
  xs <- listBaseNamesWithSuffix (dirKey exe) ".c"
  mapM_ setupCruleWithDeps xs
  setupLinkRule exe xs

setupLinkRule :: Key -> [Loc] -> G ()
setupLinkRule exe xs =
  if length xs == 0 then GFail (printf "setupLinkRule(%s):no objects" (show exe)) else do
  let obs = [ mkKey x ".o" | x <- xs ]
  GRule $ Rule
    { tag = printf "link:%s" (show exe)
    , dir = dirKey exe
    , hidden = False
    , targets = [exe]
    , depcom = do
        mapM_ DNeed obs
        pure $ bash $ printf "gcc %s -o %s" (baseKeys obs) (baseKey exe)
    }

setupCruleWithDeps :: Loc -> G ()
setupCruleWithDeps x = do
  let c = mkKey x ".c"
  let o = mkKey x ".o"
  let d = mkKey x ".d"
  GRule $ ccDepsRule d c
  GRule $ ccCompileRule o c $ do
    deps <- readDepsFile d
    mapM_ DNeed deps

readDepsFile :: Key -> D [Key]
readDepsFile key = do
  contents <- DReadKey key
  pure [ Key (dirKey key </> name) | name <- parseDepsFile contents ]

-- expect a single line like: "foo.o: foo.c foo.h bar.h"
parseDepsFile :: String -> [String]
parseDepsFile contents =
  case words contents of
    [] -> error (show ("parseDepsFile",contents))
    _:xs -> xs -- ignore the leading "foo.o: "

ccCompileRule :: Key -> Key -> D () -> Rule
ccCompileRule o c cDeps = Rule
  { tag = printf "cc:%s" (show o)
  , dir = dirKey o
  , hidden = False
  , targets = [o]
  , depcom = do
      cDeps
      readOpt (Key (dirKey c </> "cflags")) >>= \case
        Nothing -> pure $ bash $ printf "gcc -c %s -o %s" (baseKey c) (baseKey o)
        Just cflags ->
          pure $ bash $ printf "gcc %s -c %s -o %s" cflags (baseKey c) (baseKey o)
  }


readOpt :: Key -> D (Maybe String)
readOpt key = do
  DExistsKey key >>= \case
    True -> Just . looseNewlines <$> DReadKey key
    False -> pure Nothing

    where looseNewlines = intercalate " " . lines


ccDepsRule :: Key -> Key  -> Rule
ccDepsRule d c = Rule
  { tag = printf "cc:%s" (show d)
  , dir = dirKey d
  , hidden = False
  , targets = [d]
  , depcom = do
      DNeed c
      pure $ bash $ printf "gcc -MG -MM %s -MF %s" (baseKey c) (baseKey d)
  }

bash :: String -> Action
bash command = Action { hidden = False, command }
