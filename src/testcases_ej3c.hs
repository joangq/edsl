-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

import Ej3c

{-
    Espera que en el módulo Ej3c se encuentre definida una función
    
    parse :: String -> Maybe UProp

    Junto con el tipo UProp y sus respectivos constructores.
-}

import Prelude hiding ((<*>),(<$>),(<*),(*>),(<$))
import Data.Char
import Data.Maybe

-- # Casos de prueba

--  "∼(((5 = 5) \\/ ∼(2 < 1)) /\\ ((3 < 4) \\/ (7 = 7)))"
s1 = "(5=5)"
e1 = Eq (Val 5) (Val 5)

s2 = "~(2<1)"
e2 = Not (Lt (Val 2) (Val 1))

s3 = "(3<4)"
e3 = Lt (Val 3) (Val 4)

s4 = "(7=7)"
e4 = Eq (Val 7) (Val 7)

s5 = "(" ++ s1 ++ "\\/" ++ s2 ++ ")"
e5 = Or e1 e2

s6 = "(" ++ s3 ++ "\\/" ++ s4 ++ ")"
e6 = Or e3 e4

s7 = "(" ++ s5 ++ "/\\" ++ s6 ++ ")"
e7 = And e5 e6

testCases :: [(String, UProp)]

testCases =
    ("(1\\/2)",  Or (Val 1) (Val 2)) :
    ("(1\\/2)",  Or (Val 1) (Val 2)) :
    ("(1/\\2)",  And (Val 1) (Val 2)) :
    ("~(1\\/2)", Not (Or (Val 1) (Val 2))) :
    ("(1<2)", Lt (Val 1) (Val 2)) :
    ("(1=2)", Eq (Val 1) (Val 2)) :
    ("~(1<2)", Not (Lt (Val 1) (Val 2))) :
    (s1, e1) :
    (s2, e2) :
    (s3, e3) :
    (s4, e4) :
    (s5, e5) :
    (s6, e6) :
    (s7, e7) :
    ("(1<2)", Lt (Val 1) (Val 2)) :
    ("(1<(1<2))", Lt (Val 1) (Lt (Val 1) (Val 2))) :
    ("((1<2)<(1<2))", Lt (Lt (Val 1) (Val 2)) (Lt (Val 1) (Val 2))) :

    -- A partir de este punto, el resto de casos fueron generados aleatoriamente
    -- (Ver testcases_ej3c_gen.hs)

    ("~(~(8/\\5)<~(9/\\3))", (Not (Lt (Not (And (Val 8) (Val 5))) (Not (And (Val 9) (Val 3)))))) :
    ("(~~(6=1)<~~(0\\/8))", (Lt (Not (Not (Eq (Val 6) (Val 1)))) (Not (Not (Or (Val 0) (Val 8)))))) :
    ("~(~(2/\\3)/\\(~2=(1/\\2)))", (Not (And (Not (And (Val 2) (Val 3))) (Eq (Not (Val 2)) (And (Val 1) (Val 2)))))) :
    ("~(((5\\/7)=(7<4))<~(9<2))", (Not (Lt (Eq (Or (Val 5) (Val 7)) (Lt (Val 7) (Val 4))) (Not (Lt (Val 9) (Val 2)))))) :
    ("~((~6<(5/\\1))<(~5/\\(5<8)))", (Not (Lt (Lt (Not (Val 6)) (And (Val 5) (Val 1))) (And (Not (Val 5)) (Lt (Val 5) (Val 8)))))) :
    ("~((~3=(5\\/7))=((9/\\2)<(4<0)))", (Not (Eq (Eq (Not (Val 3)) (Or (Val 5) (Val 7))) (Lt (And (Val 9) (Val 2)) (Lt (Val 4) (Val 0)))))) :
    ("~(((6/\\0)\\/(1\\/4))<((6=5)\\/(7=0)))", (Not (Lt (Or (And (Val 6) (Val 0)) (Or (Val 1) (Val 4))) (Or (Eq (Val 6) (Val 5)) (Eq (Val 7) (Val 0)))))) :
    ("(~((3=1)/\\(3=0))\\/(~~4/\\~(7<8)))", (Or (Not (And (Eq (Val 3) (Val 1)) (Eq (Val 3) (Val 0)))) (And (Not (Not (Val 4))) (Not (Lt (Val 7) (Val 8)))))) :
    ("((~~4<((2<7)\\/~5))/\\~(~2=(1/\\8)))", (And (Lt (Not (Not (Val 4))) (Or (Lt (Val 2) (Val 7)) (Not (Val 5)))) (Not (Eq (Not (Val 2)) (And (Val 1) (Val 8)))))) :
    ("(~~(1\\/4)\\/((~0<(6=9))<((0<4)/\\~3)))", (Or (Not (Not (Or (Val 1) (Val 4)))) (Lt (Lt (Not (Val 0)) (Eq (Val 6) (Val 9))) (And (Lt (Val 0) (Val 4)) (Not (Val 3)))))) :
    ("(~((8<3)/\\~9)\\/(((0=9)/\\(0\\/6))\\/~(5\\/8)))", (Or (Not (And (Lt (Val 8) (Val 3)) (Not (Val 9)))) (Or (And (Eq (Val 0) (Val 9)) (Or (Val 0) (Val 6))) (Not (Or (Val 5) (Val 8)))))) :
    ("((((2\\/8)=(0/\\6))=~(2=7))<~((8=1)=(4\\/8)))", (Lt (Eq (Eq (Or (Val 2) (Val 8)) (And (Val 0) (Val 6))) (Not (Eq (Val 2) (Val 7)))) (Not (Eq (Eq (Val 8) (Val 1)) (Or (Val 4) (Val 8)))))) :
    ("(~(~1=(0\\/9))=(((8<1)\\/(3\\/7))=(~1\\/(1\\/2))))", (Eq (Not (Eq (Not (Val 1)) (Or (Val 0) (Val 9)))) (Eq (Or (Lt (Val 8) (Val 1)) (Or (Val 3) (Val 7))) (Or (Not (Val 1)) (Or (Val 1) (Val 2)))))) :
    ("(~((9/\\0)/\\(3/\\2))\\/(((3<6)=~6)<((8\\/7)<(8\\/0))))", (Or (Not (And (And (Val 9) (Val 0)) (And (Val 3) (Val 2)))) (Lt (Eq (Lt (Val 3) (Val 6)) (Not (Val 6))) (Lt (Or (Val 8) (Val 7)) (Or (Val 8) (Val 0)))))) :
    ("(((~4\\/(9\\/8))=((3/\\4)=(7=0)))\\/(~~3/\\(~8\\/~8)))", (Or (Eq (Or (Not (Val 4)) (Or (Val 9) (Val 8))) (Eq (And (Val 3) (Val 4)) (Eq (Val 7) (Val 0)))) (And (Not (Not (Val 3))) (Or (Not (Val 8)) (Not (Val 8)))))) :
    ("((~(7<9)=((7/\\4)\\/(8\\/0)))\\/(((5<5)\\/(2<2))<~(3<0)))", (Or (Eq (Not (Lt (Val 7) (Val 9))) (Or (And (Val 7) (Val 4)) (Or (Val 8) (Val 0)))) (Lt (Or (Lt (Val 5) (Val 5)) (Lt (Val 2) (Val 2))) (Not (Lt (Val 3) (Val 0)))))) :
    ("((~(1/\\7)=((9\\/4)\\/~7))/\\(((8/\\9)<~0)\\/((5/\\1)\\/(2/\\3))))", (And (Eq (Not (And (Val 1) (Val 7))) (Or (Or (Val 9) (Val 4)) (Not (Val 7)))) (Or (Lt (And (Val 8) (Val 9)) (Not (Val 0))) (Or (And (Val 5) (Val 1)) (And (Val 2) (Val 3)))))) :
    ("((~~1\\/((6=0)=(4/\\3)))\\/(((2/\\0)=(0=0))=((2\\/1)\\/(7<2))))", (Or (Or (Not (Not (Val 1))) (Eq (Eq (Val 6) (Val 0)) (And (Val 4) (Val 3)))) (Eq (Eq (And (Val 2) (Val 0)) (Eq (Val 0) (Val 0))) (Or (Or (Val 2) (Val 1)) (Lt (Val 7) (Val 2)))))) :
    ("(((~9=(8/\\4))=(~8=(9/\\1)))/\\((~7\\/(6/\\4))<((8/\\2)<(7/\\8))))", (And (Eq (Eq (Not (Val 9)) (And (Val 8) (Val 4))) (Eq (Not (Val 8)) (And (Val 9) (Val 1)))) (Lt (Or (Not (Val 7)) (And (Val 6) (Val 4))) (Lt (And (Val 8) (Val 2)) (And (Val 7) (Val 8)))))) :
    ("((((7/\\7)<~3)\\/((4<4)=(9<9)))=(((6/\\8)=(0=6))/\\((7<0)<~7)))", (Eq (Or (Lt (And (Val 7) (Val 7)) (Not (Val 3))) (Eq (Lt (Val 4) (Val 4)) (Lt (Val 9) (Val 9)))) (And (Eq (And (Val 6) (Val 8)) (Eq (Val 0) (Val 6))) (Lt (Lt (Val 7) (Val 0)) (Not (Val 7)))))) :
    []

-- # Funciones para usar los casos de test

testSingleCase' :: (String, UProp) -> (String, Bool)
testSingleCase' x@(s, e) = case parse s of
    Just e' -> (s, e == e')
    Nothing -> (s, False)

testSingleCase :: (String, UProp) -> String
testSingleCase x@(s, e) = resultStr ++ diag

    where result = parse s
          e' = fromMaybe (Val $ -1) result

          resultStr = if isNothing result || e /= e' 
                        then "\x1b[31m[  FAIL  ]\x1b[0m"
                        else "\x1b[32m[   OK   ]\x1b[0m" 

          diag = " Case: \x1b[33m" ++ show s ++ "\x1b[0m" ++
                 " Expected: " ++ show e++ 
                 " Got: " ++ (if isNothing result then "Nothing" else show e')

{- 
    Esto está para poder imprimir el caso de test mientras se lo está ejecutando.
    Usa '\r' al principio para que sea reemplazado por la otra línea.
-}
testSingleCaseFirstPrint :: (String, UProp) -> IO ()
testSingleCaseFirstPrint x@(s, e) = do
    putStr ("\r \x1b[94m[TESTING]\x1b[0m Case: \x1b[33m" ++ show s ++ "\x1b[0m")
    putStr $ ("\r" ++ (testSingleCase x) ++ "\n")

-- Si no quiere usarse testSingleCaseFirstPrint, puede utilizarse `mapM_ putStrLn $ testAllCases testCases`
testAllCases :: [(String, UProp)] -> [String]
testAllCases xs = map testSingleCase xs

main :: IO ()
main = do
    -- mapM_ putStrLn $ testAllCases testCases
    mapM_ testSingleCaseFirstPrint testCases