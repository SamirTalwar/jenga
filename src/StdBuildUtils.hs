module StdBuildUtils
  ( listBaseNamesWithSuffix
  , parseSingleName
  , mkKey, baseKey, baseKeys, dirKey
  , (</>), dirLoc
  ) where

import Data.Char qualified as Char
import Data.List (intercalate)
import Interface (G(..),Key(..),Loc(..))
import System.FilePath qualified as FP
import Text.Printf (printf)

parseSingleName :: Key -> String -> G Loc
parseSingleName key str =
  case lines str of
    [] -> GFail $ printf "parseSingleName(%s): no lines" (show key)
    _:_:_ -> GFail $ printf "parseSingleName(%s): unexpected multiple lines" (show key)
    [line] ->
      if any badChar line
      then GFail (printf "parseSingleName(%s): Bad name: '%s'" (show key) line)
      else do
        let dir = dirKey key
        pure (dir </> line)
      where
        badChar c = c == '/' || n < 33 || n > 126
          where n = Char.ord c

listBaseNamesWithSuffix :: Loc -> String -> G [Loc]
listBaseNamesWithSuffix dir sought = do
  locs <- GGlob dir
  pure [ Loc base
       | Loc x <- locs
       , FP.hasExtension x
       , (base,suf) <- [FP.splitExtensions x]
       , suf == sought
       ]

-- Key & Loc stuff

mkKey :: Loc -> String -> Key
mkKey (Loc fp) suffix = Key (Loc (fp++suffix))

baseKey :: Key -> String
baseKey (Key (Loc fp)) = FP.takeFileName fp

baseKeys :: [Key] -> String
baseKeys = intercalate " " . map baseKey

dirKey :: Key -> Loc
dirKey (Key loc) = dirLoc loc

dirLoc :: Loc -> Loc
dirLoc (Loc fp) = Loc (FP.takeDirectory fp)

(</>) :: Loc -> String -> Loc
(</>) (Loc dir) filename = Loc (FP.normalise (dir FP.</> filename))
