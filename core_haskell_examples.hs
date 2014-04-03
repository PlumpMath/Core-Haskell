--Taken from 4620 lecture notes
--Author: Joseph Manning
--H1.simple.hs
---------------------------------------------------------------------- null
-- null xs : is list 'xs' empty ?
null = \xs -> xs == []
---------------------------------------------------------------------- length
-- length xs : the number of components in list 'xs'
length = \xs -> if null xs then
  0
  else
  1 + length ( tail xs )
---------------------------------------------------------------------- elem
-- elem x xs : does item 'x' occur in list 'xs' ?
elem = \x -> \xs -> not ( null xs )
  &&
  ( x == head xs || elem x ( tail xs ) )
---------------------------------------------------------------------- count
-- count x xs : the number of times that item 'x' occurs in list 'xs'
count = \x -> \xs -> if null xs then
  0
  else
  ( if x == head xs then 1 else 0 ) + count x ( tail xs )
---------------------------------------------------------------------- append
-- append xs ys : the list formed by joining lists 'xs' and 'ys', in that order
append = \xs -> \ys -> if null xs then
  ys
  else
  head xs : append ( tail xs ) ys
--------------------------------------------------------------------------------

--H2-map_filter.hs
--------------------------------------------------------------------------------
-- map f xs : the list formed by applying function 'f'
-- to each component of list 'xs'
map = \f -> \xs -> if null xs then
  []
  else
  f ( head xs ) : map f ( tail xs )
--------------------------------------------------------------------------------
-- doublelist ns : the list formed by doubling each number in list 'ns'
doublelist = map ( \n -> 2 * n )
--------------------------------------------------------------------------------
-- fliplist bs : the list formed by negating each boolean in list 'bs'
fliplist = map not
--------------------------------------------------------------------------------
-- filter p xs : the list formed by those components of list 'xs'
-- which satisfy predicate 'p'
filter = \p -> \xs -> if null xs then
  []
  else
  if p ( head xs ) then
  head xs : filter p ( tail xs )
  else
  filter p ( tail xs )
--------------------------------------------------------------------------------
-- positives ns : the list of positive numbers in list 'ns'
positives = filter ( \n -> n > 0 )
--------------------------------------------------------------------------------
-- multiples d ns : the list of numbers in list 'ns' which are divisible by 'd'
multiples = \d -> filter ( \n -> mod n d == 0 )
--------------------------------------------------------------------------------

--H3-foldr.pdf
---------------------------------------------------------------------- foldr
-- foldr f z xs : the result of appending item 'z' to the right end of list 'xs'
-- and then cumulatively applying the two-parameter function 'f'
-- from right to left on this augmented list
foldr = \f -> \z -> \xs -> if null xs then
  z
  else
  f ( head xs ) ( foldr f z ( tail xs ) )
---------------------------------------------------------------------- sum
-- sum ns : the sum of all items in the numeric list 'ns'
sum = foldr ( \n1 -> \n2 -> n1 + n2 ) 0
---------------------------------------------------------------------- product
-- product ns : the product of all items in the numeric list 'ns'
product = foldr ( \n1 -> \n2 -> n1 * n2 ) 1
---------------------------------------------------------------------- factorial
-- factorial n : the number 1 * 2 * ... * n for a non-negative integer 'n'
factorial = \n -> product ( range 1 n ) -- range : SEE Assignment #1
---------------------------------------------------------------------- and
-- and bs : do all components of the boolean list 'bs' equal 'True' ?
and = foldr ( \b1 -> \b2 -> b1 && b2 ) True
---------------------------------------------------------------------- or
-- or bs : does any component of the boolean list 'bs' equal 'True' ?
or = foldr ( \b1 -> \b2 -> b1 || b2 ) False
---------------------------------------------------------------------- all
-- all p xs : do all components of list 'xs' satisfy predicate 'p' ?
all = \p -> \xs -> and ( map p xs )
---------------------------------------------------------------------- any
-- any p xs : does any component of list 'xs' satisfy predicate 'p' ?
any = \p -> \xs -> or ( map p xs )
---------------------------------------------------------------------- elem
-- elem x xs : does item 'x' occur in list 'xs' ?
elem = \x -> any ( \e -> e == x )
--------------------------------------------------------------------------------
---------------------------------------------------------------------- length
-- length xs : the number of components in list 'xs'
length = foldr ( \x -> \acc -> acc + 1 ) 0
---------------------------------------------------------------------- map
-- map f xs : the list formed by applying function 'f'
-- to each component of list 'xs'
map = \f -> foldr ( \x -> \acc -> f x : acc ) []
---------------------------------------------------------------------- filter
-- filter p xs : the list formed by those components of list 'xs'
-- which satisfy predicate 'p'
filter = \p -> foldr ( \x -> \acc -> if p x then x : acc else acc ) []
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--H4-ListUtils.pdf
--------------------------------------------------------------------------------
-- take n xs : the list of the first 'n' components of 'xs',
-- or 'xs' itself if 'n' exceeds its length
take = \n -> \xs -> if n <= 0 || null xs then
  []
  else
  head xs : take ( n - 1 ) ( tail xs )
--------------------------------------------------------------------------------
-- drop n xs : the list 'xs' with the first 'n' components removed,
-- or the empty list if 'n' exceeds its length
drop = \n -> \xs -> if n <= 0 || null xs then
  xs
  else
  drop ( n - 1 ) ( tail xs )
--------------------------------------------------------------------------------
-- takeWhile p xs : the longest prefix of 'xs' whose components
-- all satisfy predicate 'p'
takeWhile = \p -> \xs -> if null xs || not ( p ( head xs ) ) then
  []
  else
  head xs : takeWhile p ( tail xs )
--------------------------------------------------------------------------------
-- dropWhile p xs : the longest suffix of 'xs' whose first component
-- does not satisfy predicate 'p'
dropWhile = \p -> \xs -> if null xs || not ( p ( head xs ) ) then
  xs
  else
  dropWhile p ( tail xs )
--------------------------------------------------------------------------------
-- zipWith f xs ys : the list formed by applying function 'f' to pairs
-- of corresponding components in lists 'xs' and 'ys',
-- stopping as soon as either list is exhausted
zipWith = \f -> \xs -> \ys -> if null xs || null ys then
  []
  else
  f ( head xs ) ( head ys )
  : zipWith f ( tail xs ) ( tail ys )
--------------------------------------------------------------------------------

--H5-InfiniteLists.pdf
--------------------------------------------------------------------------------
-- Generating Fibonacci Numbers : Exponential-Time and Linear-Time Algorithms
--------------------------------------------------------------------------------
-- fibsSlow : the infinite list of Fibonacci Numbers : 0, 1, 1, 2, 3, 5, 8, ...
fibsSlow = map fib ( from 1 )
-- fib n : the 'n'th Fibonacci number, for any positive integer 'n'
fib = \n -> if n == 1 then
  0
  else
  if n == 2 then
  1
  else
  fib ( n - 1 ) + fib ( n - 2 )
--------------------------------------------------------------------------------
-- fibsFast : the infinite list of Fibonacci Numbers : 0, 1, 1, 2, 3, 5, 8, ...
fibsFast = 0 : 1 : zipWith ( \f1 -> \f2 -> f1 + f2 ) fibsFast ( tail fibsFast )
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Generating Primes Numbers : Sieve of Eratosthenes
--------------------------------------------------------------------------------
-- primes : the infinite list of prime numbers : 2, 3, 5, 7, 11, 13, 17, ...
primes = sieve ( from 2 )
--------------------------------------------------------------------------------
-- sieve ns : the result of applying the Sieve of Eratosthenes to the list 'ns'
sieve = \ns -> head ns : sieve ( dropMultiples ( head ns ) ( tail ns ) )
--------------------------------------------------------------------------------
-- dropMultiples d ns : the numeric list 'ns' with all multiples of 'd' removed
dropMultiples = \d -> filter ( \n -> mod n d /= 0 )
--------------------------------------------------------------------------------

--H6-Accumulators.pdf
--------------------------------------------------------------------------------
-- sum ns : the sum of all items in the numeric list 'ns'
sum = sum' 0
-- sum' sumSoFar ns : the sum of the number 'sumSoFar'
-- and all items in the numeric list 'ns'
sum' = \sumSoFar -> \ns -> if null ns then
  sumSoFar
  else
  sum' ( sumSoFar + head ns ) ( tail ns )
--------------------------------------------------------------------------------
-- maxSlow ns : the maximum item in the non-empty numeric list 'ns'
-- ( the running time is exponential in the length of 'ns' )
maxSlow = \ns -> if null ( tail ns ) || head ns > maxSlow ( tail ns ) then
  head ns
  else
  maxSlow ( tail ns )
--------------------------------------------------------------------------------
-- maxFast ns : the maximum item in the non-empty numeric list 'ns'
-- ( the running time is linear in the length of 'ns' )
maxFast = \ns -> maxFast' ( head ns ) ( tail ns )
-- maxFast' maxSoFar ns : the bigger of the number 'maxSoFar'
-- and the maximum item in the numeric list 'ns'
maxFast' = \maxSoFar -> \ns -> if null ns then
  maxSoFar
  else
  maxFast' ( max maxSoFar ( head ns ) )
  ( tail ns )
--------------------------------------------------------------------------------
-- fibs : the infinite list of Fibonacci Numbers : 0, 1, 1, 2, 3, 5, 8, ...
fibs = fibs' 0 1
-- fibs' f1 f2 : the infinite list of Fibonacci Numbers
-- starting with the consecutive Fibonacci Numbers 'f1' and 'f2'
fibs' = \f1 -> \f2 -> f1 : fibs' f2 ( f1 + f2 )
--------------------------------------------------------------------------------
