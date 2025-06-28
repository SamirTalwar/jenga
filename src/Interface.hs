
module Interface
  ( G(..)
  , D(..)
  , Rule(..)
  , Key(..)
  , Loc(..), (</>), dirLoc,
  ) where

import Control.Monad (ap,liftM)
import System.FilePath qualified as FP


-- G: (Rule) Generation monad

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
  GGlob :: Loc -> G [Loc]
  GExists :: Loc -> G Bool
  GReadKey :: Key -> G String

data Rule = Rule
  { tag :: String
  , targets :: [Key]
  , depcom :: D String -- TODO: better type than String?
  }

-- D: Dependency monad

instance Functor D where fmap = liftM
instance Applicative D where pure = DRet; (<*>) = ap
instance Monad D where (>>=) = DBind

data D a where
  DRet :: a -> D a
  DBind :: D a -> (a -> D b) -> D b
  DNeed :: Key -> D ()
  --DReadKey :: Key -> D String -- TODO: the reason we want monadic deps

-- Every target & dep is identified by a key
data Key = Key Loc deriving (Eq,Ord)

data Loc = Loc FilePath deriving (Eq,Ord) -- A relative file-path location

instance Show Key where show (Key loc) = show loc
instance Show Rule where show Rule{tag} = tag
instance Show Loc where show (Loc fp) = fp

(</>) :: Loc -> String -> Loc
(</>) (Loc dir) filename = Loc (dir FP.</> filename)

dirLoc :: Loc -> Loc
dirLoc (Loc fp) = Loc (FP.takeDirectory fp)
