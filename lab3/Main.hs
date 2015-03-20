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


--LUCRU CU IMAGINI

l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
l10="      ** *******          ** *******    "
l11="      ** ******           ** ******     "
l12="      ** **               ** **         "
l13="      ** **               ** **         "
l14="      ** **               ** **         "
l15=" **   ** **          **   ** **         "
l16="***   *  *          ***   *  *          "
l17=" ***    *            ***    *           "
l18="  ******              ******            "
l19="    ***                 ***             "

img = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]

--flip vertical
vertical_flip i = foldl (\x y -> y:x) [] i

--flip orizontal
horizontal_flip i = map (\x -> reverse x) i

--afisare
display m = putStr (foldr (++) "" (map (++ "\n") m))

--rotatie -90*
rotate_right i = vertical_flip (transpunere i)

--rotatie +90*
rotate_left i = transpunere (vertical_flip i)

--negative
negative i = map (\x -> map (\y -> if y == ' ' then '*' else ' ') x) i

--scale
scale i sz = map (\x -> map (\string -> concat $ replicate sz string) x) i 