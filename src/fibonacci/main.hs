fib :: Int -> Int
fib n | n <= 1 = 0
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = print (fib 36)
