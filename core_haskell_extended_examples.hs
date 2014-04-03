--Taken from 4620 and 4621 lecture notes
--Author: Joseph Manning
--H7-Haskell.pdf
--------------------------------------------------------------------------------
-- Examples : Function Definitions
--------------------------------------------------------------------------------
-- square n : the square of the number 'n'
square :: Num a => a -> a
square n = n * n
--------------------------------------------------------------------------------
-- add n1 n2 : the sum of the numbers 'n1' and 'n2'
add :: Num a => a -> a -> a
add n1 n2 = n1 + n2
--------------------------------------------------------------------------------
-- factorial n : the factorial of the non-negative integer 'n'
factorial :: Int -> Integer
factorial 0 = 1
factorial n = factorial ( n - 1 ) * n
--------------------------------------------------------------------------------
-- sum' ns : the sum of all elements in the numeric list 'ns'
sum' :: Num a => [ a ] -> a
sum' [] = 0
sum' ( n : ns ) = n + sum' ns
--------------------------------------------------------------------------------
-- length xs : the number of elements in the list 'xs'
length' :: [ a ] -> Int
length' [] = 0
length' ( _ : xs ) = 1 + length' xs
--------------------------------------------------------------------------------
-- allEqual xs : are all elements in the list 'xs' equal to one another ?
allEqual :: Eq a => [ a ] -> Bool
allEqual [] = True
allEqual [ _ ] = True
allEqual ( x1 : x2 : xs ) = x1 == x2 && allEqual ( x2 : xs )
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Examples : List Comprehensions
--------------------------------------------------------------------------------
-- primes : the infinite list of primes numbers : 2, 3, 5, 7, 11, 13, 17, ...
primes :: [ Int ]
primes = [ p | p <- [ 1 .. ], isPrime p ]
-- isPrime n : is the integer 'n' a prime number ?
isPrime :: Int -> Bool
isPrime n = factors n == [ 1, n ]
-- factors n : the list of factors of the positive integer 'n'
factors :: Int -> [ Int ]
factors n = [ f | f <- [ 1 .. n ], mod n f == 0 ]
--------------------------------------------------------------------------------
--core-to-full.pdf
f1 = [ 5, 7, 3 ]
f2 = 3.14159
f3 = -0.5
f4 = "Hello"
sign n | n > 0 = "positive"
       | n < 0 = "negative"
       | otherwise = "zero"
f5 = ( 1, 2, 3, 4 )
f6 = [ 1 .. 7 ]
f7 = [ n * n | n <- [ 1 .. 7 ], mod n 2 == 0 ]
f8 = [ (c,n) | c <- "ABC", n <- [ 1 .. 2 ] ]