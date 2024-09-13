-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-# LANGUAGE GADTs, KindSignatures #-}

{-

Ahora la interpretación depende de un ambiente de variables en el que se le asocian valores
de verdad a las variables.

Extienda el tipo definido en 3.a

-}

type ID = String

class TExpr e where
    valT    :: Int      -> e Int
    eqT     :: e Int    -> e Int    -> e Bool
    ltT     :: e Int    -> e Int    -> e Bool
    notT    :: e Bool   -> e Bool
    andT    :: e Bool   -> e Bool   -> e Bool
    orT     :: e Bool   -> e Bool   -> e Bool
    assignT :: ID      -> e Bool   -> e ()
    varT    :: ID      -> e Bool

data TPP a = TP String

instance Show (TPP a) where
    show (TP x) = x

instance TExpr TPP where
    valT                    = TP . show
    eqT     (TP x) (TP y)   = TP $ "(" ++ x ++ " = " ++ y ++ ")"
    ltT     (TP x) (TP y)   = TP $ "(" ++ x ++ " < " ++ y ++ ")"
    notT    (TP x)          = TP $ "~" ++ x
    andT    (TP x) (TP y)   = TP $ "(" ++ x ++ " /\\ " ++ y ++ ")"
    orT     (TP x) (TP y)   = TP $ "(" ++ x ++ " \\/ " ++ y ++ ")"
    assignT x (TP y)        = TP $ x ++ " := " ++ y
    varT    x               = TP x

-- # Algunos casos de test

e1 :: TExpr e => e Int
e1 = valT 4

e2 :: TExpr e => e Bool
e2 = ltT (valT 4) (valT 5)

e3 :: TExpr e => e Bool
e3 = orT (notT (ltT (valT 4) (valT 5))) (eqT (valT 4) (valT 5))

e4 :: TExpr e => e ()
e4 = assignT "x" e3

e5 :: TExpr e => e ()
e5 = assignT "y" (varT "x")

main :: IO ()
main = do
    putStrLn $ show (e1 :: TPP Int)
    putStrLn $ show (e2 :: TPP Bool)
    putStrLn $ show (e3 :: TPP Bool)
    putStrLn $ show (e4 :: TPP ())
    putStrLn $ show (e5 :: TPP ())