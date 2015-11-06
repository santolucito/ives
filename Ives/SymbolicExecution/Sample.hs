data Color = Red | Green | Blue

add :: Integer -> Integer -> Integer
add a b = a + b

times :: Integer -> Integer -> Integer
times a b = a * b

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
