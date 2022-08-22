def fib(n)
  n <= 1 ? n : fib(n - 1) + fib(n - 2)
end

n = ARGV[0].to_i
puts fib(n)