-- Part 1: High order functions

-- Two matrixes to be used for experimenting.
m1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
m2 = [[1, 0, 0], [0, 1, 1], [1, 0, 1]]

-- Helper function that displays a matrix in a human-friendly manner.
matDisplay m = putStr (foldr (++) "" (map (\l -> foldr (++) "\n" (map (\e -> (show e) ++ " ") l)) m))

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


-- Part 2: Images as lists...

-- The PP logo as an image.
l1  = "        ***** **            ***** **    "
l2  = "     ******  ****        ******  ****   "
l3  = "    **   *  *  ***      **   *  *  ***  "
l4  = "   *    *  *    ***    *    *  *    *** "
l5  = "       *  *      **        *  *      ** "
l6  = "      ** **      **       ** **      ** "
l7  = "      ** **      **       ** **      ** "
l8  = "    **** **      *      **** **      *  "
l9  = "   * *** **     *      * *** **     *   "
l10 = "      ** *******          ** *******    "
l11 = "      ** ******           ** ******     "
l12 = "      ** **               ** **         "
l13 = "      ** **               ** **         "
l14 = "      ** **               ** **         "
l15 = " **   ** **          **   ** **         "
l16 = "***   *  *          ***   *  *          "
l17 = " ***    *            ***    *           "
l18 = "  ******              ******            "
l19 = "    ***                 ***             "

pp_logo = [l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16, l17, l18, l19]

-- Helper function that displays images in a more human-friently manner.
imgDisplay img = putStr (foldr (++) "" (map (++ "\n") img))

-- 1. Implement the following: 

-- a. imgHorizontalFlip <- flip the image horizontally;
imgHorizontalFlip img = map (\x -> reverse x) img

-- b. imgVerticalFlip <- flip the image vertically;
imgVerticalFlip img = foldl (\x y -> y:x) [] img

-- c. imgRotateLeft <- rotate the image 90 degrees to the left;
imgRotateLeft img = matTranspose (imgVerticalFlip img)

-- d. imgRotateRight <- rotate the image 90 degrees to the right;
imgRotateRight img = imgVerticalFlip (matTranspose img)

-- e. imgNegative <- * pixels become ' ' & ' ' pixels become *.
imgNegative img = map (\l -> map (\pixel -> if pixel == ' ' then '*' else ' ') l) img

-- 2. Scale of an image with x units.
-- multiply by x pixels horizontally
imgScaleHorizontal img x = map (\l -> foldr (++) "" (map (\pixel -> replicate x pixel) l)) img
-- multiply by x pixels verically
imgScaleVertical img x = foldr (++) [] (map (\l ->  (replicate x l)) img)
-- multiply by x * x the whole image
imgScale img x = imgScaleVertical (imgScaleHorizontal img x) x

-- 3. Unite two images horizontally.
imgUniteH img img' = zipWith (++) img img'

-- 4. Unite two images vertically.
imgUniteV img img' = img' ++ img 

-- 5. Crop from x to y pixel columns (horizontally) from image.
imgCropH img x y = map (\l -> (take x l) ++ (drop y l)) img 

-- 6. Crop from x to y pixel lines (vertically) from image.
imgCropV img x y = (take x img) ++ (drop y img) 

-- 7. Overlap two images: img img'. We assume that a ' ' pixel from img[x][y] it's am empty
-- pixel, therefore we overlapp it with the pixel from img'[x][y].
imgOverlapp img img' = zipWith (\l l' -> zipWith (\pixel pixel' -> if pixel == ' ' then pixel' else pixel) l l') img img'
