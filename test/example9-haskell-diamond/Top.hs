module Top(main) where
import qualified B
import qualified C
main :: IO ()
main = do
  putStr "Top["
  B.main
  putStr ","
  C.main
  putStr "]\n"
