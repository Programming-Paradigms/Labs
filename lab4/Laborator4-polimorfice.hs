	--TDA-uri polimorfice
	--Autori : Marculescu Cristian si Gabriela Strezea

--Implementati TDA-ul “Lista polimorfica”
data ListT t = Void | Cons t (ListT t) deriving Show

--Re-implementati o functie ce converteste Liste definite anterior, in liste (polimorfice) din Haskell

convertT :: [a] -> (ListT a)
convertT [] = Void
convertT (e:l) = Cons e (convertT l)

--Implementati foldl, foldr si map pentru TDA-ul creat

tda_foldl op acc Void = acc
tda_foldl op acc (Cons t l) = tda_foldl op ( op t acc ) l

tda_foldr op acc Void = acc
tda_foldr op acc (Cons t l) = op t (tda_foldr op acc l)

tda_map op = tda_foldr ((Cons).op) Void

--Implementati TDA-ul “Pereche”. Observatie: O pereche poate contine valori de orice tip


data Pair a b = Pair a b deriving Show

--Implementati o functie care primeste doua liste (reprezentate folosind TDA-ul anterior) si construieste o lista de perechi

build_pair_list :: (ListT a) -> (ListT b) -> ListT (Pair a b)

build_pair_list Void l = Void
build_pair_list (Cons a l1) (Cons b l2) = Cons (Pair a b)(build_pair_list l1 l2)

--Implementati TDA-ul “Arbore polimorfic”

data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

--Implementati o functie tmap care este echivalentul lui map pe arbori

--tmap :: (a -> b) -> Tree a -> Tree b

tmap op Nil = Nil
tmap op ( Node left a right ) = Node (tmap op left) (op a) (tmap op right)

--Implementati o functie tzipWith care este echivalentul lui zipWith pe arbori

tzipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c

tzipWith op Nil t = Nil
tzipWith op (Node left1 a right1) (Node left2 b right2) = Node (tzipWith op left1 left2) (op a b) (tzipWith op right1 right2) 

--Implementati foldT pe arbori

foldT op acc Nil = acc
foldT op acc (Node left a right) = op a ( op (foldT op acc left) (foldT op acc right) )















