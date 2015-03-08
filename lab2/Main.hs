module Main where

--1. Define a higher-order function which prefixes [1,2,3] to a list received as a parameter - functional clojure
prefix = (++) [1,2,3]

--2. Define a higher-order function which receives a function and a number as parameters and applies the function twice on the number.
double_apply f a = f ( f a)

--3. Define a function which receives a binary operator and returns it applied on the reversed order of the parameters
inverted f a b = f b a

--4. Implement the fucntions 
--a. map
pp_map f [] = []
pp_map f (h:t) = ( f h ) : (pp_map f t)

--b. foldl
pp_foldl f acc [] = acc
pp_foldl f acc (h:t) = pp_foldl f (f acc h) t

--c. foldr
pp_foldr f acc [] = acc
pp_foldr f acc (h:t) = f h (pp_foldr f acc t) 

--d. filter
pp_filter f [] = []
pp_filter f (h:t) = if ( f h ) then h : pp_filter f t
			else pp_filter f t
--e. zipWith
pp_zipWith f [] _ = []
pp_zipWith f _ [] = []
pp_zipWith f (h1:t1) (h2:t2) = (f h1 h2 ) : (pp_zipWith f t1 t2 )

--f. composition of functions
composition f g = f ( g )

--5. Implement using foldl or foldr
--a. map
pp_map_fold f = foldr ((:).f)  []

--b. filter
pp_filter_fold f = foldr (\x acc -> if ( f x ) then x : acc else acc ) []
