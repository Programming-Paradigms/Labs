-- Part 1: High order functions

-- Declaring two matrixes to be used for experimenting.
m1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
m2 = [[1, 0, 0], [0, 1, 1], [1, 0, 1]]

-- 1. Implement a function that determines the i line of a matrix m.
matLine i m = head (drop i m)

-- 2. Implement a function that the determines the (i,j) element of a matrix m.
matElement i j m = matLine j (matLine i m)

-- 3. Implement the addition of two matrixes: m + m'.
matAddition m m' = zipWith (\l l' -> zipWith (+) l l') m m' 

-- 4. Implement the transpose of a matrix m.
matTranspose ([]:_) = []
matTranspose m = (map head m) : matTranspose (map tail m)

-- 5. Implement the multiplication of two matrixes: m x m'.
matMultiply m m' = map (\l -> map (\l' -> foldr (+) 0 (zipWith (*) l l')) (matTranspose m')) m
