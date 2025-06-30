module UserMain (main) where

import Control.Monad (forM_)
import ElabC qualified
import ElabSimpleMake qualified
import Engine (engineMain)
import Interface (G(..),Key(..),Loc(..))
import System.FilePath qualified as FP
import Text.Printf (printf)

main :: IO ()
main = engineMain $ \dirs -> do
  case dirs of
    [] -> GFail "UserMain: nothing to build"
    _ -> mapM_ dispatchAllConfigs dirs

dispatchAllConfigs :: Loc -> G ()
dispatchAllConfigs dir = do
  configNames <- listBaseNamesWithSuffix dir ".jenga"
  forM_ configNames $ \fullName -> do
    let func = dispatch (FP.takeFileName fullName)
    let config = Key (Loc (fullName ++ ".jenga"))
    func config

dispatch :: String -> (Key -> G ())
dispatch = \case
  "cc" -> ElabC.elab
  "cc-basic" -> ElabC.elabBasic -- no dep discovery; just for example1
  "simple-make" -> ElabSimpleMake.elab
  name ->
    \_ -> GFail $ printf "unknown jenga config file: %s.jenga" name

listBaseNamesWithSuffix :: Loc -> String -> G [String]
listBaseNamesWithSuffix dir sought = do
  locs <- GGlob dir
  pure [ base
       | Loc x <- locs
       , FP.hasExtension x
       , (base,suf) <- [FP.splitExtensions x]
       , suf == sought
       ]
