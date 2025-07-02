module C where
import qualified A
main :: IO ()
main = do
  putStr "C["
  A.main
  putStr "]"
