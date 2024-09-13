-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-# LANGUAGE GADTs, KindSignatures #-}

{-

Definir una nueva interpretación para 2 que retorne un String que represente el
programa en esa sintaxis.

-}

data Expr   :: * -> * where
    Val     :: Int          -> Expr Int
    Eq      :: Expr Int     -> Expr Int     -> Expr Bool
    Lt      :: Expr Int     -> Expr Int     -> Expr Bool
    Not     :: Expr Bool    -> Expr Bool
    And     :: Expr Bool    -> Expr Bool    -> Expr Bool
    Or      :: Expr Bool    -> Expr Bool    -> Expr Bool

evalPrettyPrint :: Expr t -> String
evalPrettyPrint (Val n)     = show n
evalPrettyPrint (Eq e1 e2)  = "(" ++ evalPrettyPrint e1 ++ " = " ++ evalPrettyPrint e2 ++ ")"
evalPrettyPrint (Lt e1 e2)  = "(" ++ evalPrettyPrint e1 ++ " < " ++ evalPrettyPrint e2 ++ ")"
evalPrettyPrint (Not e)     = "~" ++ evalPrettyPrint e
evalPrettyPrint (And e1 e2) = "(" ++ evalPrettyPrint e1 ++ " /\\ " ++ evalPrettyPrint e2 ++ ")"
evalPrettyPrint (Or e1 e2)  = "(" ++ evalPrettyPrint e1 ++ " \\/ " ++ evalPrettyPrint e2 ++ ")"

-- # Algunos casos de test

e1 :: Expr Int
e1 = Val 4

e2 :: Expr Bool
e2 = Lt (Val 4) (Val 5)

e3 :: Expr Bool
e3 = Or (Not (Lt (Val 4) (Val 5))) (Eq (Val 4) (Val 5))

main :: IO ()
main = do
    putStrLn $ evalPrettyPrint e1
    putStrLn $ evalPrettyPrint e2
    putStrLn $ evalPrettyPrint e3