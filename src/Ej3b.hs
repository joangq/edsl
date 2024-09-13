-- Autor: Joan Imanol Gonzalez Quiroga - email: jgquiroga@dc.uba.ar - Facultad de Ciencias Exactas y Naturales, Universidad de Buenos Aires.

{-# LANGUAGE GADTs, KindSignatures #-}

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

Definir un tipo UProp, de kind ∗, que represente el árbol de sintaxis abstracta no tipado
del lenguaje.

-}

data UProp :: * where
    Val    :: Int -> UProp
    Lt     :: UProp -> UProp -> UProp
    Eq     :: UProp -> UProp -> UProp
    Not    :: UProp -> UProp
    Parens :: UProp -> UProp
    And    :: UProp -> UProp -> UProp
    Or     :: UProp -> UProp -> UProp