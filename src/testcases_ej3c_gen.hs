-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

import System.Random (randomRIO) -- cabal install --lib random
import Control.Monad (replicateM)
import System.Environment (getArgs)

{-

Genera un árbol de sintaxis aleatorio para la siguiente gramática:

prop ::= term '\/' prop -> Or
      | term

term ::= factor '/\' term -> And
      | factor

factor ::= '~' factor -> Not
        | '(' prop ')'
        | '(' prop '=' prop ')' -> Eq
        | '(' prop '<' prop ')' -> Lt
        | Nat -> Val

Se corresponde con el siguiente tipo:

data UProp :: * where
    Val    :: Int -> UProp
    Lt     :: UProp -> UProp -> UProp
    Eq     :: UProp -> UProp -> UProp
    Not    :: UProp -> UProp
    And    :: UProp -> UProp -> UProp
    Or     :: UProp -> UProp -> UProp

(Ver Ej3c.hs)

-}

randomAST :: Int -> IO (String, String, Int)

randomAST 0 = do
    value <- randomRIO (0, 9)
    let valueStr = show (value :: Int)
    return (valueStr, "(Val " ++ valueStr ++ ")", 1)

randomAST depth = do
    let astMap = [descendLt, descendEq, descendNot, descendAnd, descendOr]
    func <- (astMap !!) <$> randomRIO (0, length astMap - 1) -- elijo una función al azar
    func (depth - 1)

-- # Ramas del árbol de sintaxis

descendLt depth = do
    (left, leftAST, leftCount) <- randomAST depth
    (right, rightAST, rightCount) <- randomAST depth
    return ("(" ++ left ++ " < " ++ right ++ ")", "(Lt " ++ leftAST ++ " " ++ rightAST ++ ")", leftCount + rightCount + 1)

descendEq depth = do
    (left, leftAST, leftCount) <- randomAST depth
    (right, rightAST, rightCount) <- randomAST depth
    return ("(" ++ left ++ " = " ++ right ++ ")", "(Eq " ++ leftAST ++ " " ++ rightAST ++ ")", leftCount + rightCount + 1)

descendNot depth = do
    (inner, innerAST, innerCount) <- randomAST depth
    return ("~" ++ inner, "(Not " ++ innerAST ++ ")", innerCount + 1)

descendParens depth = do
    (inner, innerAST, innerCount) <- randomAST depth
    return ("(" ++ inner ++ ")", "(Parens " ++ innerAST ++ ")", innerCount + 1)

descendAnd depth = do
    (left, leftAST, leftCount) <- randomAST depth
    (right, rightAST, rightCount) <- randomAST depth
    return ("(" ++ left ++ " /\\ " ++ right ++ ")", "(And " ++ leftAST ++ " " ++ rightAST ++ ")", leftCount + rightCount + 1)

descendOr depth = do
    (left, leftAST, leftCount) <- randomAST depth
    (right, rightAST, rightCount) <- randomAST depth
    return ("(" ++ left ++ " \\/ " ++ right ++ ")", "(Or " ++ leftAST ++ " " ++ rightAST ++ ")", leftCount + rightCount + 1)

-- # Utilidades para generar árboles de sintaxis aleatorios

randomWalk :: Int -> IO (String, String, Int)
randomWalk depth = randomAST depth

randomWalkN' :: Int -> Int -> IO (String, String)
randomWalkN' depth n = do
    (str, ast, count) <- randomWalk depth
    if count == n
        then return (str, ast)
        else randomWalkN' depth n


{-

Para generar árboles con una cantidad de nodos N,
se busca con una profundidad máxima de floor(log2(N+0.1))

-}
closestPowerOfTwo :: Int -> Int
closestPowerOfTwo = floor . logBase 2 . (0.1 +) . fromIntegral

randomWalkN :: Int -> IO (String, String)
randomWalkN n = randomWalkN' (closestPowerOfTwo n) n

{- Esto está para poder ser llamado por consola
    Compilado con: 
        ghc --make testcases_ej3c_gen.hs -no-keep-hi-files -no-keep-o-files
    
    Uso:
        testcases_ej3c_gen <n>
        
        Donde <n> es la cantidad de nodos target del árbol de sintaxis aleatorio.
-}

main :: IO ()
main = do
    args <- getArgs
    let n = read (args !! 0) :: Int
    (str, ast) <- randomWalkN n
    putStrLn str
    putStrLn ast
    return ()