-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-# LANGUAGE GADTs, KindSignatures #-}

type ID = String

data Expr   :: * -> * where
    Val     :: Int       -> Expr Int
    Eq      :: Expr Int  -> Expr Int  -> Expr Bool
    Lt      :: Expr Int  -> Expr Int  -> Expr Bool
    Not     :: Expr Bool -> Expr Bool
    And     :: Expr Bool -> Expr Bool -> Expr Bool
    Or      :: Expr Bool -> Expr Bool -> Expr Bool
    Assign  :: ID        -> Expr Bool -> Expr ()
    Var     :: ID        -> Expr Bool

evalPrettyPrint :: Expr t -> String
evalPrettyPrint (Val b)             = show b
evalPrettyPrint (Eq e1 e2)          = "(" ++ evalPrettyPrint e1 ++ " = " ++ evalPrettyPrint e2 ++ ")"
evalPrettyPrint (Lt e1 e2)          = "(" ++ evalPrettyPrint e1 ++ " < " ++ evalPrettyPrint e2 ++ ")"
evalPrettyPrint (Not e)             = "~" ++ evalPrettyPrint e
evalPrettyPrint (And e1 e2)         = "(" ++ evalPrettyPrint e1 ++ " /\\ " ++ evalPrettyPrint e2 ++ ")"
evalPrettyPrint (Or e1 e2)          = "(" ++ evalPrettyPrint e1 ++ " \\/ " ++ evalPrettyPrint e2 ++ ")"
evalPrettyPrint (Assign x e)        = x ++ " := " ++ evalPrettyPrint e
evalPrettyPrint (Var x)             = x

-- # Algunos casos de test

e1 :: Expr Int
e1 = Val 4

e2 :: Expr Bool
e2 = Lt (Val 4) (Val 5)

e3 :: Expr Bool
e3 = Or (Not (Lt (Val 4) (Val 5))) (Eq (Val 4) (Val 5))

e4 :: Expr ()
e4 = Assign "x" e3

e5 :: Expr ()
e5 = Assign "y" (Var "x")

main :: IO ()
main = do
    putStrLn $ evalPrettyPrint e1
    putStrLn $ evalPrettyPrint e2
    putStrLn $ evalPrettyPrint e3
    putStrLn $ evalPrettyPrint e4
    putStrLn $ evalPrettyPrint e5
