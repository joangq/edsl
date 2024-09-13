-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-

Definir una nueva interpretación para 1 que retorne un String que represente el
programa en esa sintaxis.

-}

class TExpr e where
    valT    :: Int      -> e Int
    eqT     :: e Int    -> e Int    -> e Bool
    ltT     :: e Int    -> e Int    -> e Bool
    notT    :: e Bool   -> e Bool
    andT    :: e Bool   -> e Bool   -> e Bool
    orT     :: e Bool   -> e Bool   -> e Bool

data TPP a = TP String

instance Show (TPP a) where
    show (TP x) = x -- Necesario para poder visualizarlo

instance TExpr TPP where
    valT                    = TP . show
    eqT     (TP x) (TP y)   = TP $ "(" ++ x ++ " = " ++ y ++ ")"
    ltT     (TP x) (TP y)   = TP $ "(" ++ x ++ " < " ++ y ++ ")"
    notT    (TP x)          = TP $ "~" ++ x
    andT    (TP x) (TP y)   = TP $ "(" ++ x ++ " /\\ " ++ y ++ ")"
    orT     (TP x) (TP y)   = TP $ "(" ++ x ++ " \\/ " ++ y ++ ")"

-- # Algunos casos de test

e1 :: TExpr e => e Int
e1 = valT 4

e2 :: TExpr e => e Bool
e2 = ltT (valT 4) (valT 5)

e3 :: TExpr e => e Bool
e3 = orT (notT (ltT (valT 4) (valT 5))) (eqT (valT 4) (valT 5))

main :: IO ()
main = do
    putStrLn $ show (e1 :: TPP Int)
    putStrLn $ show (e2 :: TPP Bool)
    putStrLn $ show (e3 :: TPP Bool)