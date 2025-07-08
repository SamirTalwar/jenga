module UserMain (main) where

import MakeStyle qualified (elaborate)
import Engine (engineMain)
import Interface (G(..),Key(..),Loc(..))
import StdBuildUtils ((</>))
import Data.List.Ordered (nubSort)

main :: IO ()
main = engineMain $ \args -> do
  let args' = case args of [] -> ["."]; _ -> args
  xss <- mapM findStartingPointsFromTopLoc args'
  case nubSort (concat xss) of
    [] -> GFail "UserMain: nothing to build"
    dirs -> mapM_ elaborateConfig dirs

elaborateConfig :: Loc -> G ()
elaborateConfig dir = do
  let config = dir </> "build.jenga"
  GExists config >>= \case
    True -> MakeStyle.elaborate (Key config)
    False -> pure ()

-- TODO: Maybe use find
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
