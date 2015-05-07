-----------------------------------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------------------------------

-- variables are strings
type Var = String
-- values may be anything
data Expr a = Val a | CVar Var | Plus (Expr a) (Expr a) | Mult (Expr a) (Expr a) | Par (Expr a)

{-- display an expression nicely --}
instance Show a => Show (Expr a) where
    show (Val a) = show a
    show (CVar a) = a
    show (e `Plus` e') = show e++" + "++show e'
    show (e `Mult` e') = show e++" * "++show e'
    show (Par e) = "("++show e++")"
    
e0 = (CVar "X") `Mult` (Par ((Val 5) `Plus` (Val 6)))
e1 = (Val 4) `Mult` (Par ((Val 5) `Plus` (Val 6)))

--an interpretation is a function which assigns for a variable, a truth-value
type Interpretation a = Var -> a

-- evale assigns to each interpretation and expression, the result of evaluating the latter
evale :: Num a => Interpretation a -> Expr a -> a
evale _ (Val a) = a                                  -- a constant value evaluates to itself, no matter the interpretation 
evale i (CVar x) = i x
evale i (e `Plus` e') = (evale i e) + (evale i e')
evale i (e `Mult` e') = (evale i e) * (evale i e')
evale i (Par e) = evale i e

itest "X" = 5
-- testing: evale itest e0


-----------------------------------------------------------------------------------------------
-- Programs
-----------------------------------------------------------------------------------------------
data Prog a = Is Var (Expr a) | If (Expr a) (Prog a) (Prog a) | S (Prog a) (Prog a) | Return Var

{- display a program nicely -}
instance Show a => Show (Prog a) where
    show (v `Is` e) = v++" = "++show e
    show (If e e' e'') = "if ("++show e++") then "++show e'++"\n else "++show e''
    show (S e e') = show e++";\n"++show e'
    show (Return v) = "return "++v
    
e3 = (("X" `Is` (Val 3)) `S` ("Y" `Is` (Val 4))) `S` (If ((Val 0) `Mult` (Val 5)) (Return "X") (Return "Y"))


-- unlike the classes which we have seen so far, Evaluable does not define a set of types (i.e. a property of types), but a _RELATION_ between a type-constructor with kind * => * and a type (i.e. type constructor with type *). The relation specifies that types "t a" (containers) may be evaluated with respect to interpretations of type "interpretation a". 
class (Num a) => Evaluable t a where
    eval :: (Num a) => Interpretation a -> t a -> a

-- this instance defines that Expr and the generic type "a" are members of the above relation    
instance (Num a) => Evaluable Expr a where
    eval = evale

-- this is a helper function for below. We need to update an interpretation as we process a program. The result is a new interpretation, but possibly a value, if the encountered program is a "return"    
data Perheaps a = Only a | Perheaps (Interpretation a)    
    
-- the same thing, for programs    
instance (Num a) => Evaluable Prog a where
    eval i prog = 
        -- this function takes an interpretation, a program, and builds a new interpretation by scanning the program
        let update :: Num a => Interpretation a -> Prog a -> Perheaps a
            update i (v `Is` expr) = Perheaps (\var -> if var == v then (eval i expr) else i var)  -- this line creates a new function which (*) returns the evaluation of expr if the called variable is var, and (i var) otherwise
            update i (If e e' e'') = if (eval i e) == 0 then update i e'' else update i e'
            update i (S e e') = 
                let (Perheaps i') = (update i e)      -- we want to update the interpretation by processing e first, and use the resulting interpretation to process e'
                in update i' e'                       -- we know the former call is a "Perheaps", because after return we can have no more instructions
            update i (Return v) = Only (i v)          -- this is a trick. When we find return, we know this is the final instruction (Programs are assumed to be correct). Thus, we build a constant interpretation function, which will allways return the value of v in i. We use this below:
            (Only result) = update i prog             -- a valid program must allways return a result
        in result

e4 = (("X" `Is` (Val 3)) `S` ("Y" `Is` ((CVar "X") `Mult` (Val 4)))) `S` (If ((CVar "Y") `Plus` (Val (-12))) (Return "X") (Return "Y"))
-- eval (\x->0) e4

{-
    X = 3;
    Y = X * 4;
    if Y + (-12) (!=0) then return X else return Y


-}

