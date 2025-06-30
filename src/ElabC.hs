module ElabC (elab,elabBasic) where

import Text.Printf (printf)
import Interface (G(..),Rule(..),Action(..),D(..),Key(..),Loc)
import StdBuildUtils (parseSingleName,dirKey,listBaseNamesWithSuffix,mkKey,baseKeys,baseKey,(</>))

elab,elabBasic :: Key -> G ()

elab = elabG setupCruleWithDeps
elabBasic = elabG setupCruleBasic

elabG :: (Loc -> G ()) -> Key -> G ()
elabG setupCC config = do
  configContents <- GReadKey config
  main <- Key <$> parseSingleName config configContents
  xs <- listBaseNamesWithSuffix (dirKey config) ".c"
  mapM_ setupCC xs
  setupLinkRule main xs
  GArtifact main

setupLinkRule :: Key -> [Loc] -> G ()
setupLinkRule exe xs =
  if length xs == 0 then GFail (printf "setupLinkRule(%s):no objects" (show exe)) else do
  let obs = [ mkKey x ".o" | x <- xs ]
  GRule $ Rule
    { tag = printf "link:%s" (show exe)
    , targets = [exe]
    , depcom = do
        mapM_ DNeed obs
        pure $ Bash (printf "gcc %s -o %s" (baseKeys obs) (baseKey exe))
    }

setupCruleBasic :: Loc -> G ()
setupCruleBasic x = do
  let c = mkKey x ".c"
  let o = mkKey x ".o"
  GRule $ ccCompileRule o c (DNeed c)

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
  , targets = [o]
  , depcom = do
      cDeps
      pure $ Bash (printf "gcc -c %s -o %s" (baseKey c) (baseKey o))
  }

ccDepsRule :: Key -> Key  -> Rule
ccDepsRule d c = Rule
  { tag = printf "cc:%s" (show d)
  , targets = [d]
  , depcom = do
      DNeed c
      pure $ Bash (printf "gcc -MG -MM %s -MF %s" (baseKey c) (baseKey d))
  }
