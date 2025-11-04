{
module Lex (Token(..), lexer) where
import Data.Char (isSpace)
}

%wrapper "basic"

-- Definición de expresiones regulares reutilizables.
-- $digit: cualquier dígito del 0 al 9
-- $alpha: cualquier letra mayúscula o minúscula
$digit    = 0-9
$alpha    = [a-zA-Z]


-- A partir de aquí se definen las reglas de tokenización.
-- Cada línea tiene la forma:
--   patrón  { acción }
-- Donde 'patrón' es una expresión regular y 'acción' devuelve un token.
tokens :-
    -- Ignora espacios en blanco, tabuladores y saltos de línea.
    $white+                        ;
-- Valores booleanos    
"#t"                           { \_ -> TokenBool True }
"#f"                           { \_ -> TokenBool False }
 -- Operaciones matemáticas y lógicas
"sqrt"                         { \_ -> TokenSqrt }
"expt"                         { \_ -> TokenExpt }
"not"                          { \_ -> TokenNot }
 -- Estructuras de control
"if"                           { \_ -> TokenIf }
"cond"                         { \_ -> TokenCond }
 -- Funciones sobre pares
"fst"                          { \_ -> TokenFst }
"snd"                          { \_ -> TokenSnd }
"pair"                         { \_ -> TokenPair}
 -- Expresiones de enlace (binding)
"let"                          { \_ -> TokenLet }
"let*"                         { \_ -> TokenLetStar }
"letrec"                       { \_ -> TokenLetRec }
"lambda"                       { \_ -> TokenLambda }
"in"                           { \_ -> TokenIn }
 -- Operaciones sobre listas
"head"                         { \_ -> TokenHead }
"tail"                         { \_ -> TokenTail }
   -- Operaciones aritméticas de incremento/decremento
"add1"                         { \_ -> TokenAdd1 }
"sub1"                         { \_ -> TokenSub1 }
  -- Operadores relacionales
"<="                           { \_ -> TokenMenorIgual }
">="                           { \_ -> TokenMayorIgual }
"!="                           { \_ -> TokenDistinto }
"<"                            { \_ -> TokenMenor }
">"                            { \_ -> TokenMayor }
"="                            { \_ -> TokenEq }
 -- Operadores aritméticos
"+"                            { \_ -> TokenSuma }
"-"                            { \_ -> TokenResta }
"*"                            { \_ -> TokenMult }
"/"                            { \_ -> TokenDiv }
 -- Símbolos de agrupación y separación
"("                            { \_ -> TokenPA }
")"                            { \_ -> TokenPC }
"["                            { \_ -> TokenCA }
"]"                            { \_ -> TokenCC }
","                            { \_ -> TokenComa }
-- Números enteros: una o más cifras consecutivas
$digit+                        { \s -> TokenNum (read s) }
 -- Identificadores: comienzan con una letra y pueden contener letras o dígitos
$alpha [$alpha $digit]*        { \s -> TokenVar s }


{
-- Definición del tipo de dato 'Token' que representa todos los tipos de tokens posibles.
-- Cada constructor corresponde a una palabra reservada, símbolo u operador reconocido.
data Token
 = TokenNum Int  -- Números enteros
    | TokenBool Bool  -- Valores booleanos (#t, #f)
    | TokenVar String  -- Variables o identificadores
    -- Operadores aritméticos
    | TokenSuma
    | TokenResta
    | TokenMult
    | TokenDiv
    -- Funciones matemáticas y lógicas
    | TokenSqrt
    | TokenExpt
    | TokenNot
      -- Símbolos de agrupación
    | TokenPA
    | TokenPC
    | TokenCA
    | TokenCC
    | TokenComa
    -- Palabras clave y operadores relacionales
    | TokenIn
    | TokenEq
    | TokenMayor
    | TokenMenor
    | TokenMayorIgual
    | TokenMenorIgual
    | TokenDistinto
     -- Estructuras de control y funciones predefinidas
    | TokenIf
    | TokenFst
    | TokenSnd
    | TokenLet
    | TokenLetStar
    | TokenLetRec 
    | TokenLambda
    | TokenHead
    | TokenTail
    | TokenPair 
    | TokenCond     
    | TokenAdd1
    | TokenSub1
    -- Permite mostrar y comparar tokens fácilmente
    deriving (Show, Eq)
    

-- Normaliza cualquier espacios en blanco Unicode a ' ' para que $white+ lo consuma
normalizeSpaces :: String -> String
normalizeSpaces = map (\c -> if isSpace c then '\x20' else c)

-- Alias: Alex define alexScanTokens (String -> [Token])
lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}