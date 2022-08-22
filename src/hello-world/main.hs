import System.Environment (getArgs)

main :: IO ()
main = do
  (arg : _) <- getArgs
  putStrLn arg
