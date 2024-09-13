-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-

Ahora la interpretación depende de un ambiente de variables en el que se le asocian valores
de verdad a las variables.

Extienda el EDSL definido en 1

-}

import Control.Monad.State
import Control.Monad (liftM, liftM2, mapM_)
import Data.Map (Map, lookup, insert, empty)

type ID = String
class TExpr e where
    valT    :: Int      -> e Int
    eqT     :: e Int    -> e Int    -> e Bool
    ltT     :: e Int    -> e Int    -> e Bool
    notT    :: e Bool   -> e Bool
    andT    :: e Bool   -> e Bool   -> e Bool
    orT     :: e Bool   -> e Bool   -> e Bool

    -- Agrego la asignación de valor y la obtención de la variable
    assignT  :: ID      -> e Bool   -> e ()
    varT     :: ID      -> e Bool

-- Ahora la evaluación depende de las variables,
-- por lo que requiero una mónada de estado.

type Environment = Map ID Bool
newtype TEval a = TE { runTE :: State Environment a }

instance TExpr TEval where
    valT x                  = TE $ return x
    eqT     (TE x) (TE y)   = TE $ liftM2 (==)  x y
    ltT     (TE x) (TE y)   = TE $ liftM2 (<)   x y
    notT    (TE x)          = TE $ liftM  (not) x
    andT    (TE x) (TE y)   = TE $ liftM2 (&&)  x y
    orT     (TE x) (TE y)   = TE $ liftM2 (||)  x y
    assignT x      (TE e)   = TE $ do
                                    v <- e
                                    modify (insert x v)

    varT x = TE $ do
                    env <- get
                    case Data.Map.lookup x env of
                        Nothing  -> error $ "La variable '" ++ x ++ "' no está definida."
                        Just v -> return v

-- # Algunos casos de test

e1 :: TExpr e => e Int
e1 = valT 4

e2 :: TExpr e => e Bool
e2 = ltT (valT 4) (valT 5)

e3 :: TExpr e => e Bool
e3 = orT (notT (ltT (valT 4) (valT 5))) (eqT (valT 4) (valT 5))

{-
    Como las funciones de TEval devuelven un TE, para ejecutar
    el estado hay que usar evalState.
-}
evalWithState :: State Environment a -> a
evalWithState e = evalState e empty

-- Acá ejecuto los casos previos más algunas interacciones adicionales
-- como asignarle el valor de una variable a otra y obtener el valor de una variable no definida.
main :: IO ()
main = do
    print $ evalWithState $ runTE e1
    print $ evalWithState $ runTE e2
    print $ evalWithState $ runTE e3

    let program = do
            runTE $ assignT "x" e3
            runTE $ assignT "y" e2
            runTE $ assignT "a" (varT "x" :: TEval Bool)

            valX <- runTE (varT "x" :: TEval Bool)
            valY <- runTE (varT "y" :: TEval Bool)
            valA <- runTE (varT "a" :: TEval Bool)
            valZ <- runTE (varT "z" :: TEval Bool) -- intento obtener una variable no definida

            return [valX, valY, valA, valZ]

    result <- return $ evalWithState program

    mapM_ print result