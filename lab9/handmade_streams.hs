build :: (a -> a) -> a -> [a]
build f a = a : build f (f a)

lselect :: (Num e, Ord e) => e -> [e] -> Maybe e
lselect _ [] = Nothing
lselect _ [_] = Nothing
lselect e (an:anpo:rest)
	| abs (an - anpo) < e = Just an
	| otherwise = lselect e (anpo:rest)

--Approximation generator
squareApprox x current = 0.5 * (current + x / current)

-- Square of x, with a precision e
squareOf x e = lselect e (build (squareApprox x) 1)

derivative f a e = let
		hStream = build (\h -> h/2) 1
	in
		lselect e (map (\h -> ((f (a+h)) - (f a)) / h) hStream)

--Stream of intermediate points used in approximation
--Every element of the stream is a list of the points used for computing
--an approximation.
--The next sample is obtained by dividing every previously computed interval in two
--smaller ones, of equal size.
samplePointsStream l = (foldr1 (++) (map (\(a,b) -> [a,(a+b)/2]) (zip l (tail l)))) ++ [last l]

integralOf f a b e = let
		sampleStream = build samplePointsStream [a,b]
		--For every list of points of the stream, compute the approximate integral
		--using the sum of rectangles formula
		listOfIntervalsStream = map (\l -> zip l (tail l)) sampleStream
		sumOfRectanglesStream = map (\sample ->
			foldl1 (+) (map (\(a,b) -> (b-a) * (f ((a+b)/2))) sample))
			listOfIntervalsStream
	in
		lselect e sumOfRectanglesStream
