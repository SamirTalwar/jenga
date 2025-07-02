module B where
import qualified A
main :: IO ()
main = do
  putStr "B["
  A.main
  putStr "]"
