module Main where

--1. n!
--naive
factorial n = if n <= 0
  then 1
  --Operator application has lower precedence than function application
  --notice the presence and absence of parenthesis below
  else n * factorial (n-1)

--using pattern matching
factorial' 0 = 1
factorial' n = n * factorial (n-1)

--tail recursive
--More about 'let' at: http://learnyouahaskell.com/syntax-in-functions#let-it-be
factorial'' n = let
    looper 0 acc = acc
    looper n acc = looper (n-1) n * acc
  in looper n 1

--2. Fibbonacci

--naive
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- tail recursive
fib' n = let
    -- first two params are the fib(m) and fib(m+1), based on which
    -- fib(m+2) can be determined in one step
    -- the third param is m itself
    -- notice how n is available in looper's body, without being explicitely
    -- sent as an argument
    looper fm fmpo m = if (n == m)
      then fm
      else looper fmpo (fm+fmpo) (m+1)
  in
    -- We start the recursion knowing fib(0) and fib(1)
    -- and we can return in one step f(0)
    looper 1 1 0

--3. Mergesort
msort [] = []
-- notice the '@' construct, in which case the left hand side (l)
-- is bound to the function parameter
-- notice the '_' pattern which matches anything, but it
-- will not be bound in the function body
msort l@([_]) = l
msort l = let
    merge [] l = l
    merge l [] = l
    merge aas@(a:as) bbs@(b:bs) = if a < b
      then a : merge as bbs
      else b : merge aas bs
    -- notice how pattern matching works on tuples
    -- learn more about this at:
    -- http://learnyouahaskell.com/starting-out#tuples
    -- and about splitAt function at:
    -- https://hackage.haskell.org/package/base-4.7.0.2/docs/Data-List.html
    (left, right) = splitAt (div (length l) 2) l
  in
    merge (msort left) (msort right)

--4. Insertion sort
isort [] = []
isort l@([_]) = l
isort (x:xs) = let
    insertInSorted e [] = [e]
    insertInSorted e xxs@(x:xs) = if (e <= x)
      then e : xxs
      else x : insertInSorted e xs
  in
    insertInSorted x (isort xs)

--5. Quicksort
qsort [] = []
qsort l@([_]) = l
qsort (x:xs) = let
    -- x, the first element of the list is the pivot
    lower [] = []
    lower (y:ys) = if (y <= x)
      then y : lower ys
      else lower ys
    -- strictly because we don't want to duplicate the pivot
    -- in the two partitions
    strictlyGreater [] = []
    strictlyGreater (y:ys) = if (y > x)
      then y : strictlyGreater ys
      else strictlyGreater ys
    -- notice how lower and strictlyGreater only slightly
    -- differ in implementation, we'll see how to fix this soon :)
  in
    qsort (lower xs) ++ [x] ++ qsort (strictlyGreater xs)

--6. List inversions
--TBD, still homework for some.
--For help, check: http://www.geeksforgeeks.org/counting-inversions/
