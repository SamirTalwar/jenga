module UserMain (main) where

import Control.Monad (forM_)
import ElabSimpleMake qualified (elab)
import Engine (engineMain)
import Interface (G(..),Key(..),Loc(..))
import StdBuildUtils (listBaseNamesWithSuffix)

main :: IO ()
main = engineMain $ \dirs -> do
  case dirs of
    [] -> GFail "UserMain: nothing to build"
    _ -> mapM_ dispatchAllConfigs dirs

dispatchAllConfigs :: Loc -> G ()
dispatchAllConfigs dir = do
  configNames <- listBaseNamesWithSuffix dir ".jenga"
  forM_ configNames $ \(Loc fullName) -> do
    let config = Key (Loc (fullName ++ ".jenga"))
    ElabSimpleMake.elab config
