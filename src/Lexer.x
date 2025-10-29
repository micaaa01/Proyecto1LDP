{
-- Código Haskell que se inserta al inicio del archivo generado
module Lexer where
}

%wrapper "basic"

$digit    = 0-9
$alpha    = [a-zA-Z]



tokens :-

    $white+                        ;
"#t"                           { \_ -> TokenBool True }
"#f"                           { \_ -> TokenBool False }
"sqrt"                         { \_ -> TokenSqrt }
"expt"                         { \_ -> TokenExpt }
"not"                          { \_ -> TokenNot }
"if"                           { \_ -> TokenIf }
"fst"                          { \_ -> TokenFst }
"snd"                          { \_ -> TokenSnd }
"let"                          { \_ -> TokenLet }
"let*"                         { \_ -> TokenLetStar }
"lambda"                       { \_ -> TokenLambda }
"in"                           { \_ -> TokenIn }
"head"                         { \_ -> TokenHead }
"tail"                         { \_ -> TokenTail }


"<="                           { \_ -> TokenMenorIgual }
">="                           { \_ -> TokenMayorIgual }
"!="                           { \_ -> TokenDistinto }
"<"                            { \_ -> TokenMenor }
">"                            { \_ -> TokenMayor }
"="                            { \_ -> TokenEq }

"+"                            { \_ -> TokenSuma }
"-"                            { \_ -> TokenResta }
"*"                            { \_ -> TokenMult }
"/"                            { \_ -> TokenDiv }
"("                            { \_ -> TokenPA }
")"                            { \_ -> TokenPC }
"["                            { \_ -> TokenCA }
"]"                            { \_ -> TokenCC }
","                            { \_ -> TokenComa }

$digit+                        { \s -> TokenNum (read s) }
$alpha [$alpha $digit]*        { \s -> TokenVar s }


{
data Token
 = TokenNum Int
    | TokenBool Bool
    | TokenVar String
    | TokenSuma
    | TokenResta
    | TokenMult
    | TokenDiv
    | TokenSqrt
    | TokenExpt
    | TokenNot
    | TokenPA
    | TokenPC
    | TokenCA
    | TokenCC
    | TokenComa
    | TokenIn
    | TokenEq
    | TokenMayor
    | TokenMenor
    | TokenMayorIgual
    | TokenMenorIgual
    | TokenDistinto
    | TokenIf
    | TokenFst
    | TokenSnd
    | TokenLet
    | TokenLetStar
    | TokenLambda
    | TokenHead
    | TokenTail
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer = alexScanTokens
}

