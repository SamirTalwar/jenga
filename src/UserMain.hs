module UserMain (main) where

import Control.Monad (forM_)
import SimpleMake qualified (elab)
import Engine (engineMain)
import Interface (G(..),Key(..),Loc(..))
import StdBuildUtils (listBaseNamesWithSuffix)
import Data.List.Ordered (nubSort)

main :: IO ()
main = engineMain $ \args -> do
  let args' = case args of [] -> ["."]; _ -> args
  xss <- mapM findStartingPointsFromTopLoc args'
  case nubSort (concat xss) of
    [] -> GFail "UserMain: nothing to build"
    dirs -> mapM_ dispatchAllConfigs dirs

-- TODO: simplify with find *.jenga

dispatchAllConfigs :: Loc -> G ()
dispatchAllConfigs dir = do
  configNames <- listBaseNamesWithSuffix dir ".jenga"
  forM_ configNames $ \(Loc fullName) -> do
    let config = Key (Loc (fullName ++ ".jenga"))
    SimpleMake.elab config


findStartingPointsFromTopLoc :: String -> G [Loc]
findStartingPointsFromTopLoc arg = do
  let loc = Loc arg
  GExists loc >>= \case
    False -> pure []
    True -> do
      GIsDirectory loc >>= \case
        False -> pure []
        True -> findSubdirsDeep loc

findSubdirsDeep :: Loc -> G [Loc]
findSubdirsDeep loc@(Loc fp) = do
  if blockName fp then pure [] else do
    subs <- findSubdirs loc
    subsubs <- mapM findSubdirsDeep subs
    pure (loc : concat subsubs)

blockName :: String -> Bool
blockName = \case
  ".git" -> True
  ".stack-work" -> True
  ".cache" -> True
  ",jenga" -> True
  _ -> False

findSubdirs :: Loc -> G [Loc]
findSubdirs loc = do
  locs <- GGlob loc
  blocs <- sequence [ do b <- GIsDirectory loc; pure (b,loc) | loc <- locs ]
  pure [ loc | (b,loc) <- blocs, b ]
