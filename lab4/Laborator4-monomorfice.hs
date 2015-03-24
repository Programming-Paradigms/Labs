	--TDA-uri monomorfice
	--Autori : Marculescu Cristian si Gabriela Strezea

--Implementati TDA-ul: “lista de numere naturale”--

data IList = Void | Cons Integer IList

--Scrieti o functie care converteste valori ale TDA-ului vostru in liste din Haskell--

--convert :: IList -> [Integer]
convert Void = []
convert (Cons e l) = e : convert l

--Implementati TDA-ul “optiune”, care codifica valori optionale de tipul Integer, e.g. fie None, fie o valoare intreaga--

data Optiune = None | Smth Integer

--Scrieti un TDA care codifica un tuplu (inregistrare) format din urmatoarele campuri:--

data Student = No_grade String Integer [String] | With_grade String Integer [String] Integer deriving Show

--Scrieti o functie care primeste o lista de inregistrari, si le intoarce pe acelea pentru care varsta este mai mare ca 20

f ( No_grade _ varsta _ ) = varsta > 20
f ( With_grade _ varsta _ _ ) = varsta > 20

--age_filter :: [Student] -> [Student]
age_filter l = filter f l

--Scrieti o functie care primeste o lista de inregistrari, si le intoarce pe acelea care contin cel putin un prieten cu numele “Matei”--

fnume ( No_grade _ _ l ) = elem "Matei" l
fnume ( With_grade _ _ l _ ) = elem "Matei" l

--friends_filter :: [Student] -> [Student]
friends_filter l = filter fnume l

--Scrieti o functie care primeste o lista de inregistrari, si le intoarce pe acelea care contin campul “Nota PP”--

--has_note :: Student -> Bool
has_note (No_grade _ _ _ ) = False
has_note (With_grade _ _ _ _ ) = True

--grade_filter :: [Student] -> [Student]
grade_filter l = filter has_note l

--Scrieti o functie care primeste o lista de nume, o lista de varste, o lista ce contine liste de prieteni, si intoarce o lista de inregistrari corespunzatoare

combine :: String -> Integer -> [String] -> Student
combine name age friend = No_grade name age friend

build :: [String] -> [Integer] -> [[String]] -> [Student]
build namelist agelist friends = zipWith3 combine namelist agelist friends


















