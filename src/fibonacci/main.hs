import System.Environment (getArgs)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  (arg : _) <- getArgs
  let n = read arg :: Int
  print (fib n)
