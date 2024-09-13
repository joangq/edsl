-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.
{-
   NOTA: El siguiente parser aplicativo no procesa whitespaces.
         Es decir, espera que la sintaxis sea del estilo: "(1=2)\/(3<4)"
         y no del estilo: "(1 = 2) \/ (3 < 4)"

         Adicionalmente, devuelve el resultado que más haya podido ser parseado.
         Es decir, de los posibles resultados, selecciona la tupla con el menor largo
         de string en su segunda componente.

         Este archivo es un módulo para poder ser importado en otro archivo `testcases_ej3c.hs`
         donde ejecuto algunos casos de test.
--}


{-# LANGUAGE GADTs, KindSignatures #-}

module Ej3c where

import Prelude hiding ((<*>),(<$>),(<*),(*>),(<$))
import Data.Char
import Data.Maybe
import Data.List (maximumBy)
import Data.Ord (comparing)

{-

Dada la siguiente gramática que describe una sintaxis concreta para el lenguaje:

prop ::= term '\/' prop 
      | term

term ::= factor '/\' term 
      | factor

factor ::= '~' factor 
        | '(' prop ')'
        | '(' prop '=' prop ')' 
        | '(' prop '<' prop ')' 
        | Nat

Escribir un parser del lenguaje utilizando los combinadores vistos en el curso (puede
optar por usar los combinadores aplicativos o monádicos) y que retorne el valor de tipo
UProp correspondiente.

-}

data UProp :: * where
    Val    :: Int -> UProp
    Lt     :: UProp -> UProp -> UProp
    Eq     :: UProp -> UProp -> UProp
    Not    :: UProp -> UProp
    And    :: UProp -> UProp -> UProp
    Or     :: UProp -> UProp -> UProp

deriving instance Eq UProp -- Necesite esto para poder hacer casos de test (ver `testcases_ej3c.hs`)

instance Show UProp where
    show (Val n)      = show n
    show (Lt e1 e2)   = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (Eq e1 e2)   = "(" ++ show e1 ++ " = " ++ show e2 ++ ")"
    show (Not e)      = "~" ++ show e
    show (And e1 e2)  = "(" ++ show e1 ++ " /\\ " ++ show e2 ++ ")"
    show (Or e1 e2)   = "(" ++ show e1 ++ " \\/ " ++ show e2 ++ ")"

-- # Definiciones y parsers básicos

type Parser s a = [s] -> [(a, [s])]

parseFail  :: Parser s a
parseFail  = \cs -> []

parseSucceed    :: a -> Parser s a
parseSucceed a  =  \cs -> [(a,cs)]

parseSymbols :: Eq s => [s] -> Parser s [s]
parseSymbols s = \cs -> case cs of
                    [] -> []
                    _ -> if take (length s) cs == s
                            then [(s, drop (length s) cs)]
                            else []

infixl 3 <|>
infixl 4 <*> 
infixl 4 <* 
infixl 4 *> 
infixl 4 <$> 
infixl 4 <$

(<|>) ::  Parser s a -> Parser s a -> Parser s a
p <|> q = \cs -> p cs ++ q cs

(<*>) ::  Parser s (a -> b) -> Parser s a -> Parser s b
(p <*> q) cs = [ (f a, cs'')  |  (f , cs')   <- p cs
                              ,  (a , cs'')  <- q cs']

f <$> p  = parseSucceed f <*> p

p <* q   = (\ x _ -> x) <$> p <*> q

p *> q   = (\ _ y -> y) <$> p <*> q

a <$ q   = parseSucceed a <* q

p `opt` v = p <|> parseSucceed v

parseIf    :: (s -> Bool) -> Parser s s
parseIf p  = \cs ->  case cs of
                   []         -> []
                   (c : cs')  -> if  p c
                                     then [(c,cs')]
                                     else []

parseMany :: Parser s a -> Parser s [a]
parseMany p =  (:) <$> p <*> parseMany p
           <|>
           parseSucceed []

parseSome :: Parser s a -> Parser s [a]
parseSome p =  (:) <$> p <*> parseMany p

horner :: [Int] -> Int
horner = foldl (\n d -> n*10 + d) 0

parseDigit :: Parser Char Char
parseDigit = parseIf isDigit

digit :: Parser Char Int
digit = d2int <$> parseDigit
  where d2int d = ord(d) - ord('0')

digits :: Parser Char [Int]
digits = parseSome digit

number :: Parser Char Int
number = horner <$> digits

-- # Parsers de la gramática

parseProp :: Parser Char UProp
parseProp = parseOr 
  <|> parseTerm

parseOr :: Parser Char UProp
parseOr = (\e1 _ e2 -> Or e1 e2) 
  <$> parseTerm 
  <*> parseSymbols "\\/"
  <*> parseProp

parseTerm :: Parser Char UProp
parseTerm = parseAnd 
  <|> parseFactor

parseAnd :: Parser Char UProp
parseAnd = (\e1 _ e2 -> And e1 e2) 
  <$> parseFactor 
  <*> parseSymbols "/\\"
  <*> parseTerm

parseFactor :: Parser Char UProp
parseFactor = parseNat 
  <|> parseNot
  <|> parseParens 
  <|> parseEq 
  <|> parseLt 

parseNat :: Parser Char UProp
parseNat = Val <$> number

parseParens :: Parser Char UProp
parseParens = (\_ e _ -> e) 
  <$> parseSymbols "("
  <*> parseProp
  <*> parseSymbols ")"

parseEq :: Parser Char UProp
parseEq = (\_ e1 _ e2 _ -> Eq e1 e2) 
  <$> parseSymbols "(" 
  <*> parseProp 
  <*> parseSymbols "="
  <*> parseProp 
  <*> parseSymbols ")"

parseLt :: Parser Char UProp
parseLt = (\_ e1 _ e2 _ -> Lt e1 e2) 
  <$> parseSymbols "(" 
  <*> parseProp 
  <*> parseSymbols "<" 
  <*> parseProp 
  <*> parseSymbols ")"

parseNot :: Parser Char UProp
parseNot = (\_ e -> Not e) 
  <$> parseSymbols "~"
  <*> parseFactor

-- # Interfaz para interactuar con el parser

-- Esto fue especialmente necesario para testearlo con facilidad

{- 
  Para poder testear algunas cosas, era más sencillo eliminar los espacios.
  El problema de hacerlo así es que permite cortar los operadores a la mitad.
  Por ejemplo:
    1 /\ 2 === 1/\2 === 1/    \2
-}

-- removeAllWhitespace :: String -> String
-- removeAllWhitespace = filter (not . isSpace)
-- parse' = parseProp . removeAllWhitespace

parse' = parseProp

{- 
  Hago esto en vez de devolver la cabeza de la lista porque
  según fui experimentando con el orden de las alternativas
  en los parsers más complejos, el resultado 'más parseado'
  no me quedaba necesariamente al principio.
-}
returnMostParsed :: [(a, [s])] -> (a, [s])
returnMostParsed = maximumBy $ comparing $ length . snd

parse :: String -> Maybe UProp
parse s = case parse' s of
    []      -> Nothing
    result  -> Just $ fst $ returnMostParsed result