-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-# LANGUAGE GADTs, KindSignatures #-}

{-

Defina una función typeProp :: Ty t → UProp → Prop t donde Prop t es el GADT que
definió para el deep embedding y Ty t es el siguiente tipo que representa tipos:

-}

data Ty :: * -> * where
    TInt  :: Ty Int
    TBool :: Ty Bool

-- Misma definición que la de 'Expr' en 'Ej2.hs' pero con el nombre cambiado.
data Prop   :: * -> * where
    Val     :: Int       -> Prop Int
    Eq      :: Prop Int  -> Prop Int  -> Prop Bool
    Lt      :: Prop Int  -> Prop Int  -> Prop Bool
    Not     :: Prop Bool -> Prop Bool
    And     :: Prop Bool -> Prop Bool -> Prop Bool
    Or      :: Prop Bool -> Prop Bool -> Prop Bool

deriving instance Show (Prop t)

-- Cambié los nombres de los constructores para que no clasheen con los de 'Prop'.
data UProp  :: * where
    UVal    :: Int      -> UProp
    ULt     :: UProp    -> UProp -> UProp
    UEq     :: UProp    -> UProp -> UProp
    UNot    :: UProp    -> UProp
    UParens :: UProp    -> UProp
    UAnd    :: UProp    -> UProp -> UProp
    UOr     :: UProp    -> UProp -> UProp

deriving instance Show UProp

typeProp :: Ty t -> UProp -> Prop t
typeProp TInt   (UVal n)        = Val n
typeProp TBool  (ULt e1 e2)     = Lt  (typeProp TInt e1) (typeProp TInt e2)
typeProp TBool  (UEq e1 e2)     = Eq  (typeProp TInt e1) (typeProp TInt e2)
typeProp TBool  (UNot e)        = Not (typeProp TBool e)
typeProp TBool  (UAnd e1 e2)    = And (typeProp TBool e1) (typeProp TBool e2)
typeProp TBool  (UOr e1 e2)     = Or  (typeProp TBool e1) (typeProp TBool e2)
typeProp t      (UParens e)     = typeProp t e

-- # Algunos casos de test

e1 :: Prop Bool
e1 = typeProp TBool (UAnd (ULt (UVal 1) (UVal 2)) (UNot (UEq (UVal 3) (UVal 4))))

e2 :: Prop Int
e2 = typeProp TInt (UVal 42)

main :: IO ()
main = do
    print e1
    print e2
