module StdBuildUtils
  ( listBaseNamesWithSuffix
  , mkKey, baseKey, baseKeys, dirKey
  , (</>), dirLoc
  ) where

import Data.List (intercalate)
import Interface (G(..),Key(..),Loc(..))
import System.FilePath qualified as FP

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
(</>) (Loc dir) filename0 =
  Loc (FP.normalise (dir FP.</> filename))
  where
    filename =
      case filename0 of
        '/':afterLeadingSlash -> afterLeadingSlash
        rel -> rel
