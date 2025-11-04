{
module Grammars where

import Lex (Token(..),lexer)

}

%name parse
%tokentype { Token }
%error { parseError }

%token 
    int             { TokenNum $$ }     
      bool            { TokenBool $$ }
      var             { TokenVar $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMult }
      '/'             { TokenDiv }
      "sqrt"          { TokenSqrt }
      "expt"          { TokenExpt }
      "not"           { TokenNot }
      '('             { TokenPA }
      ')'             { TokenPC }
      '['             { TokenCA }
      ']'             { TokenCC }
      ','             { TokenComa }
      '='             { TokenEq }
      '<'             { TokenMenor }      
      '>'             { TokenMayor }  
      '<='            { TokenMenorIgual }
      '>='            { TokenMayorIgual }
      '!='            { TokenDistinto }
      "if"            { TokenIf }
      "fst"           { TokenFst }
      "snd"           { TokenSnd }
      "let"           { TokenLet }
      "let*"          { TokenLetStar }
      "letrec"        { TokenLetRec }
      "lambda"        { TokenLambda }
      "head"          { TokenHead }
      "tail"          { TokenTail }
      "pair"          { TokenPair }
      "cond"          { TokenCond }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }

%%

-- * Reglas principales de la gramática (símbolo inicial: SASA).
-- Cada producción construye un valor del tipo de dato 'SASA', que representa
-- el árbol sintáctico abstracto (AST) de una expresión del lenguaje.

SASA 
      : int                                                 { NumS $1 }             -- Números enteros
      | bool                                                { BooleanS $1 }         -- Valores booleanos
      | var                                                 { IdS $1 }              -- Identificadores
      | '(' "not" SASA ')'                                  { NotS $3 }             -- Negación lógica
      | '(' '+' ExpList ')'                                 { AddListS $3 }         -- Suma de lista
      | '(' '-' ExpList ')'                                 { SubListS $3 }         -- Resta de lista
      | '(' '*' ExpList ')'                                 { MulListS $3 }         -- Multiplicación de listas
      | '(' '/' ExpList ')'                                 { DivListS $3 }         -- División de lista
      | '(' '=' ExpList ')'                                 { EqListS $3 }          -- Igualdad
      | '(' '<' ExpList ')'                                 { LtListS $3 }          -- Menor que
      | '(' '>' ExpList ')'                                 { GtListS $3 }          -- Mayor que
      | '(' '<=' ExpList ')'                                { LeListS $3 }          -- Menor o igual que
      | '(' '>=' ExpList ')'                                { GeListS $3 }          -- Mayor o igual que
      | '(' '!=' ExpList ')'                                { NeListS $3 }          -- Disinto
      | '(' "lambda" '(' VarList ')' SASA ')'               { FunListS $4 $6 }      -- Función lambda
      | '(' "sqrt" SASA ')'                                 { SqrtS $3 }            -- Raíz cuadrada
      | '(' "expt" ExpList ')' { ExptListS $3 }                                     -- Exponenciación
      | '(' "pair" SASA SASA ')'     { PairS $3 $4}                                 -- Pares (pair x y)
      | '(' "fst" SASA ')'                                  { FstS $3 }             -- Primer elemento del par
      | '(' "snd" SASA ')'                                  { SndS $3 }             -- Segundo elemento del par
      | '(' SASA ',' SASA ')'                               { PairS $2 $4 }         -- Par con coma (x, y)
      | '(' "if" SASA SASA SASA ')'                         { IfS $3 $4 $5 }        -- Condicional if
     -- | '(' "let"    '(' var SASA ')' SASA ')'              { LetS    $4 $5 $7 }
      | '(' "letrec" '(' var SASA ')' SASA ')'              { LetRecS $4 $5 $7 }
      | '(' "let" '(' Bindings ')' SASA ')'                 { LetS $4 $6 }          -- Expresion let
      | '(' "let*" '(' Bindings ')' SASA ')'                { LetStarS $4 $6 }      -- Expresión let* (Secuencial)
      | '[' ListItems ']'                                   { ListS $2 }            -- Lista con elmentos       
      | '[' ']'                                             { ListS [] }            -- Lista vacía
      | '(' "head" SASA ')'                                 { HeadS $3 }            -- Primer elemento de la lista
      | '(' "tail" SASA ')'                                 { TailS $3 }            -- Resto de la lista
      | '(' "cond" CondClauses ')'                          { CondS $3 }            -- Expresión condicional múltiple
      | '(' SASA SASAList ')'                               { foldl AppS $2 $3 }    -- Aplicación de función
      | '(' "add1" SASA ')'                                 { AddListS [$3, NumS 1] }     -- Incremento en 1
      | '(' "sub1" SASA ')'                                 { SubListS [$3, NumS 1] }     -- Decremento en 1


-- | Lista de expresiones (usada en operaciones aritméticas o comparaciones)    
ExpList
      : SASA                        { [$1] }
      | SASA ExpList                { $1 : $2 }

-- | Lista de variables, usada por lambda.
VarList
      : var                         { [$1] }
      | var VarList                 { $1 : $2 }

-- | Elementos dentro de listas [a, b, c]
ListItems
      : SASA                        { [$1] }
      | SASA ',' ListItems          { $1 : $3 }

-- | Clausula individual de condicional 'cond'
-- Puede ser [condición resultado] o [else resultado]
CondClause
    : '[' SASA SASA ']'             { ($2, $3) }
    | '[' var SASA ']' 
        { case $2 of
            "else" -> (BooleanS True, $3)
            _      -> error "Se esperaba 'else'"
        }

-- | Lista de cláusulas condicionales
CondClauses
    : CondClause                    { [$1] }
    | CondClause CondClauses        { $1 : $2 }

-- | Lista de expresiones aplicadas a una función      
SASAList
  : SASA                            { [$1] }
  | SASA SASAList                   { $1 : $2 }

-- | Lista de asociaciones (bindings) en un let
Bindings
      : Binding                     { [$1] }
      | Binding Bindings            { $1 : $2 }

-- | Asociación (var, valor)
Binding
      : '(' var SASA ')'            { ($2, $3) }

{

-- | Tipo de dato principal del AST.
-- Representa todas las construcciones posibles del lenguaje SASA.
data SASA 
 = NumS Int                              -- Número entero
      | BooleanS Bool                         -- Booleano (#t o #f)
      | IdS String                            -- Identificador
      | NotS SASA                             -- Negación lógica
      | AddListS [SASA]                       -- Suma de lista de expresiones
      | SubListS [SASA]                       -- Resta
      | MulListS [SASA]                       -- Multiplicación
      | DivListS [SASA]                       -- División
      | EqListS [SASA]                        -- Igualdad
      | LtListS [SASA]                        -- Menor que
      | GtListS [SASA]                        -- Mayor que
      | LeListS [SASA]                        -- Menor o igual
      | GeListS [SASA]                        -- Mayor o igual
      | NeListS [SASA]                        -- Distinto
      | FunListS [String] SASA                -- Función lambda con lista de parámetros
      | SqrtS SASA                            -- Raíz cuadrada
      | ExptS SASA SASA                       -- Exponenciación binaria
      | ExptListS [SASA]                      -- Exponenciación n-aria
      | FstS SASA                             -- Primer elemento de un par
      | SndS SASA                             -- Segundo elemento de un par
      | PairS SASA SASA                       -- Par de valores
      | IfS SASA SASA SASA                    -- Condicional if
      | FunS String SASA                      -- Definición de función
      | AppS SASA SASA                        -- Aplicación de función
     -- | LetS String SASA SASA
      | LetS [(String, SASA)] SASA            -- Expresión let
      | LetRecS String SASA SASA
      | LetStarS [(String, SASA)] SASA        -- Expresión let* (evaluación secuencial)
      | ListS [SASA]                          -- Listas
      | HeadS SASA                            -- Primer elemento de lista
      | TailS SASA                            -- Resto de lista
      | CondS [(SASA, SASA)]                  -- Expresión condicional múltiple
      deriving (Show, Eq)

     
-- | Manejador de errores sintácticos.
-- Se ejecuta si la entrada no coincide con ninguna producción válida.
 
parseError :: [Token] -> a
parseError _ = error "Error de parseo"
}