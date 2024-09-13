-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Monad.State
import Control.Monad (mapM_)
import Data.Map (Map, lookup, insert, empty)

{-

Ahora la interpretación depende de un ambiente de variables en el que se le asocian valores
de verdad a las variables.

Extienda el EDSL definido en 2

-}

type ID = String

data Expr   :: * -> * where
    Val     :: Int       -> Expr Int
    Eq      :: Expr Int  -> Expr Int  -> Expr Bool
    Lt      :: Expr Int  -> Expr Int  -> Expr Bool
    Not     :: Expr Bool -> Expr Bool
    And     :: Expr Bool -> Expr Bool -> Expr Bool
    Or      :: Expr Bool -> Expr Bool -> Expr Bool

    -- Agrego la asignación de valor y la obtención de la variable
    Assign  :: ID        -> Expr Bool -> Expr ()
    Var     :: ID        -> Expr Bool

-- Ahora la evaluación depende de las variables,
-- por lo que requiero una mónada de estado.
type Environment = Map ID Bool

eval :: Expr t -> State Environment t
eval (Val n)     = return n
eval (Eq e1 e2)  = do
                    v1 <- eval e1
                    v2 <- eval e2
                    return (v1 == v2)

eval (Lt e1 e2)  = do
                    v1 <- eval e1
                    v2 <- eval e2
                    return (v1 < v2)

eval (Not e)    = do
                    v <- eval e
                    return (not v)

eval (And e1 e2) = do
                    v1 <- eval e1
                    v2 <- eval e2
                    return (v1 && v2)

eval (Or e1 e2)  = do
                    v1 <- eval e1
                    v2 <- eval e2
                    return (v1 || v2)

eval (Assign x e) = do
                        v <- eval e
                        modify (insert x v)


eval (Var x) = do
                    env <- get
                    case Data.Map.lookup x env of -- acá 'lookup' clasheaba con otra definición de mismo nombre.
                        Nothing  -> error $ "La variable '" ++ x ++ "' no está definida."
                        Just v -> return v

-- Como 'eval' devuelve un State, para ejecutar el estado hay que usar evalState.
evalWithState :: State Environment a -> a
evalWithState expr = evalState expr empty

-- # Algunos casos de test

e1 :: Expr Int
e1 = Val 4

e2 :: Expr Bool
e2 = Lt (Val 4) (Val 5)

e3 :: Expr Bool
e3 = Or (Not (Lt (Val 4) (Val 5))) (Eq (Val 4) (Val 5))

-- Acá ejecuto los casos previos más algunas interacciones adicionales
-- como asignarle el valor de una variable a otra y obtener el valor de una variable no definida.
main :: IO ()
main = do
    print $ evalWithState $ eval e1
    print $ evalWithState $ eval e2
    print $ evalWithState $ eval e3

    let program = do
            eval $ Assign "x" e3
            eval $ Assign "y" e2
            eval $ Assign "a" (Var "x")
            
            valX <- eval (Var "x" :: Expr Bool)
            valY <- eval (Var "y" :: Expr Bool)
            valA <- eval (Var "a" :: Expr Bool)
            valZ <- eval (Var "z" :: Expr Bool) -- intento obtener una variable no definida

            return [valX, valY, valA, valZ]

    result <- return $ evalWithState program

    mapM_ print result

