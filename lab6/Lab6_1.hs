-- Utils
lift :: (a -> b -> c) -> ((Maybe a) -> (Maybe b) -> (Maybe c))
lift f =
	let
		liftedF Nothing _ = Nothing
		liftedF _ Nothing = Nothing
		liftedF (Just a) (Just b) = Just (f a b)
	in
		liftedF

whatif :: (Maybe a) -> b -> b -> b
whatif Nothing _ r = r
whatif (Just _) r  _ = r

chooseFirst :: (Maybe a) -> (Maybe a) -> (Maybe a)
chooseFirst Nothing x = x
chooseFirst x@(Just _) _ = x
chooseFirst _ _ = Nothing

unwrap (Just x) = x

data Expr a = Val a | Var String | (Expr a) :+: (Expr a) | (Expr a) :*: (Expr a) | P (Expr a) 

data Program a = String := (Expr a) | If (Expr a) (Program a) (Program a) |
	(Program a) :->: (Program a) | Print (Expr a) |
	Return (Expr a) | NoOp

data Context a = Void | Bind (String, a) (Context a) deriving Show

instance (Show a) => Show (Expr a) where
	show (Val a) = show a
	show (Var x) = show x
	show (x :+: y) = show x ++ " + " ++ show y
	show (x :*: y) = show x ++ " * " ++ show y
	show (P x) = "(" ++ show x ++ ")"

instance (Show a) => Show (Program a) where
	show (var := expr) = show var ++ " := " ++ show expr
	show (If cond ifb elseb) = "if " ++ show cond ++ " then {\n" ++ show ifb ++ "\n} else {\n"
		++ show elseb ++ "\n}"
	show (a :->: b) = show a ++ ";\n" ++ show b
	show (Print x) = show x
	show (Return x) = "return " ++ show x
	show NoOp = ""


eval :: (Num a) => (Expr a) -> (Context a) -> (Maybe a)
eval (Val a) _ = (Just a)
eval (Var _) Void = Nothing
eval e@(Var x) (Bind (y,v) rest) =
	if (x == y) then (Just v) else (eval e rest)
eval (a :+: b) ctx = (lift (+)) (eval a ctx) (eval b ctx)
eval (a :*: b) ctx = (lift (*)) (eval a ctx) (eval b ctx)
eval (P e) ctx = eval e ctx

progrEval :: (Num a) => (Expr a) -> (Program a) -> (Maybe a)
progrEval (Val a) _ = (Just a)
progrEval (Var x) p = valueOf x p
progrEval (a :+: b) ctx = (lift (+)) (progrEval a ctx) (progrEval b ctx)
progrEval (a :*: b) ctx = (lift (*)) (progrEval a ctx) (progrEval b ctx)
progrEval (P e) ctx = progrEval e ctx

type Result a = ((Maybe a), (Context a))

class Evaluable container where
	unbind :: (Num a) => (container a) -> (Context a) -> (Result a)
	unbind _ _ = (Nothing, Void)


instance Evaluable Expr where
	unbind e c = (eval e c, c)

--data Program a = String := (Expr a) | If (Expr a) (Program a) (Program a) |
--	(Program a) :->: (Program a) | Print (Expr a)

valueOf x (y := (Val v)) = if x == y then (Just v) else Nothing
valueOf x (p1 :->: p2) = chooseFirst (valueOf x p2) (valueOf x p1) 

pack :: (Num a) => (Program a) -> (Program a) -> (Program a)
pack (var := e) b = b :->: (var := (Val $ unwrap $ progrEval e b))
pack (If cond ifb elseb) b = whatif (progrEval cond b)
		(pack ifb b)
		(pack elseb b)
pack (pa :->: pb) b = pack pb . pack pa $ b
pack (Print _) b = b
pack p@(Return x) b = b :->: p
pack NoOp b = b


instance Evaluable Program where
	unbind (var := e) c = let
			v = eval e c
			unwrap (Just x) = x
		in whatif v
			(v, Bind (var, unwrap v) c) 
			(Nothing, c)

	unbind (If cond ifb elseb) c = whatif (eval cond c)
		(unbind ifb c)
		(unbind elseb c)

	unbind (pa :->: pb) c = let
			(ra, ca) = unbind pa c
			(rb, cb) = unbind pb ca
		in whatif ra
			(rb, cb)
			(Nothing, ca)

	unbind (Print e) c = (eval e c, c)



run :: (Num a) => Program a -> Maybe a
run p = fst (unbind p Void)

ctx = Bind ("a", 1) Void

dpt = (P ((Val 2) :+: (Val 3)))
apdpt = P (Var "a" :+: 
			(P ((Val 2) :+: (Val 3))))
p1 = 	("a" := (Val 2) :->:
		("b" := P (Var "a" :+: 
			(P ((Val 2) :+: (Val 3)))))) :->:
		(Print ((Var "a") :*: (Var "b"))) :->:
		(If (Var "null") (Print (Val 0)) (Print (Var "a")))