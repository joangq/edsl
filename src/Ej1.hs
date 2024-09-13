-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{- 

Implementar el EDSL como un shallow embedding bien tipado en Haskell siguiendo el
enfoque tagless-final. Interprete los tipos integer y boolean como tipos de Haskell.

-}

class TExpr e where
    valT    :: Int      -> e Int
    eqT     :: e Int    -> e Int    -> e Bool
    ltT     :: e Int    -> e Int    -> e Bool
    notT    :: e Bool   -> e Bool
    andT    :: e Bool   -> e Bool   -> e Bool
    orT     :: e Bool   -> e Bool   -> e Bool

data TEval a = TE a
    deriving Show

instance TExpr TEval where
    valT                    = TE
    eqT     (TE x) (TE y)   = TE $ x == y
    ltT     (TE x) (TE y)   = TE $ x < y
    notT    (TE x)          = TE $ not x
    andT    (TE x) (TE y)   = TE $ x && y
    orT     (TE x) (TE y)   = TE $ x || y

-- # Algunos casos de test

e1 :: TExpr e => e Int
e1 = valT 4

e2 :: TExpr e => e Bool
e2 = ltT (valT 4) (valT 5)

e3 :: TExpr e => e Bool
e3 = orT (notT (ltT (valT 4) (valT 5))) (eqT (valT 4) (valT 5))

{-

Para ver los resultados desde GHCI se puede ejecutar:

>>> e1 :: TEval Int
>>> TE 4

>>> e2 :: TEval Bool
>>> TE True

>>> e3 :: TEval Bool
>>> TE False

Alternativamente, se puede utilizar la función
'main' definida debajo.

-}


-- Necesario para poder comparar resultados
-- deriving instance Eq a => Eq (TEval a)

main :: IO ()
main = do
    print $ (e1 :: TEval Int)
    print $ (e2 :: TEval Bool)
    print $ (e3 :: TEval Bool)
    -- A partir de acá se necesita que TEval admita comparaciones de igualdad (Eq)
    {-
    print $ e1 == TE 4
    print $ e2 == TE True
    print $ e3 == TE False
    -}