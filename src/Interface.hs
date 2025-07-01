module Interface
  ( G(..)       -- The Generation monad; used to define rules and artifacts.
  , Rule(..)    -- A Rule with dynamic dependencies.
  , Action(..)  -- A user action which is run when a rule triggers.
  , D(..)       -- The Dependency monad.
  , Key(..)     -- An identifier for targets and dependencies.
  , Loc(..)     -- A relative file-path location.
  ) where

import Control.Monad (ap,liftM)

instance Functor G where fmap = liftM
instance Applicative G where pure = GRet; (<*>) = ap
instance Monad G where (>>=) = GBind

data G a where
  GRet :: a -> G a
  GBind :: G a -> (a -> G b) -> G b
  GLog :: String -> G ()
  GFail :: String -> G a
  GArtifact :: Key -> G ()
  GRule :: Rule -> G ()
  GGlob :: Loc -> G [Loc]
  GIsDirectory :: Loc -> G Bool
  GExists :: Loc -> G Bool
  GReadKey :: Key -> G String

data Rule = Rule
  { tag :: String
  , targets :: [Key]
  , depcom :: D Action
  }

data Action = Bash String

instance Functor D where fmap = liftM
instance Applicative D where pure = DRet; (<*>) = ap
instance Monad D where (>>=) = DBind

data D a where
  DRet :: a -> D a
  DBind :: D a -> (a -> D b) -> D b
  DLog :: String -> D ()
  DNeed :: Key -> D ()
  DReadKey :: Key -> D String

data Key = Key Loc deriving (Eq,Ord)

data Loc = Loc FilePath deriving (Eq,Ord)

instance Show Rule where show Rule{tag} = tag
instance Show Action where show (Bash command) = command
instance Show Key where show (Key loc) = show loc
instance Show Loc where show (Loc fp) = fp
