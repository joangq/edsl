-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-# LANGUAGE GADTs, KindSignatures #-}

{-

Implementar el EDSL como un deep embedding bien tipado en Haskell utilizando GADTs.

Definir la función eval que evalúa una expresión bien tipada de tipo t y retorna un valor de
ese tipo.

-}


data Expr   :: * -> * where
    Val     :: Int       -> Expr Int
    Eq      :: Expr Int  -> Expr Int  -> Expr Bool
    Lt      :: Expr Int  -> Expr Int  -> Expr Bool
    Not     :: Expr Bool -> Expr Bool
    And     :: Expr Bool -> Expr Bool -> Expr Bool
    Or      :: Expr Bool -> Expr Bool -> Expr Bool


eval :: Expr t -> t
eval (Val n)     = n
eval (Eq e1 e2)  = eval e1 == eval e2
eval (Lt e1 e2)  = eval e1 < eval e2
eval (Not e)     = not (eval e)
eval (And e1 e2) = eval e1 && eval e2
eval (Or e1 e2)  = eval e1 || eval e2

deriving instance Show (Expr t) -- Necesario para poder visualizarlo

-- # Algunos casos de test

e1 :: Expr Int
e1 = Val 4

e2 :: Expr Bool
e2 = Lt (Val 4) (Val 5)

e3 :: Expr Bool
e3 = Or (Not (Lt (Val 4) (Val 5))) (Eq (Val 4) (Val 5))

main :: IO ()
main = do
    print $ eval e1 == 4
    print $ eval e2 == True
    print $ eval e3 == False