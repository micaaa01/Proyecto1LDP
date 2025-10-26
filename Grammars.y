{
module Grammars where
import Lex (Token(..), lexer)

}

%name calc
%tokentype { Token }
%error { parseError }

--declaramos todos los tokens posibles, cambiarle el nombre a los que ya existen
-- para hacer las funioes variadicas ls podemos ver como una lista de variables

%token
      int             { TokenNum $$ }
      bool            { TokenBool $$ }
      var             { TokenVar $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
       '*'             { TokenMul }
      '/'             { TokenDiv }
      "sqrt"          { TokenSqrt }
      "expt"          { TokenExpt }
      "not"           { TokenNot }
      '('             { TokenPA }
      ')'             { TokenPC }
      '['             { TokenCA }
      ']'             { TokenCC }
      ','             { TokenComa }
      in              { TokenIn }
      '='             { TokenEq }
      '<'             { TokenMayor }      
      '>'             { TokenMenor }  
      '<='            { TokenMenorIgual }
      '>='            {TokenMayorIgual }
      '!='            { TokenDistinto }
      if              { TokenIf }
      "fst"           { TokenFst }
      "snd"           { TokenSnd }
      let             { TokenLet }
      let*            { TokenLetVar}
      lambda          { TokenLambda }


      %%

      SASA : int                      { Num $1 }
          | bool                     { Boolean $1 }
          | var                      { Id $1 }
          | '(' '+' SASA SASA ')'      { Add $3 $4 }
          | '(' '-' SASA SASA ')'      { Sub $3 $4 }
          | '(' "not" SASA ')'        { Not $3 }
          | '(' '*' SASA SASA ')'      { Mul $3 $4 }
          | '(' '/' SASA SASA ')'      { Div $3 $4 }
          | '(' "sqrt" SASA ')'       { Sqrt $3 }
          | '(' "expt" SASA SASA ')'   { Expt $3 $4 }
          | '(' "fst" SASA ')'        { Fst $3 } 
          | '(' "snd" SASA ')'.       { Snd $3 }
          | '(' if SASA SASA SASA ')'   { If $3 $4 $5 }
          | '(' lambda '(' var ')' SASA ')'   { Fun $4 $6 }  
          | '(' SASA SASA ')'                  { App $2 $3 } 
          | '(' let '(' var SASA ')' SASA ')'    { LetS $4 $5 $7 }
          | '(' letrec '(' var SASA ')' SASA ')' { LetRecS $4 $5 $7 }
          | '(' letcc var SASA ')'               { LetCCS $3 $4 }
          | '(' if SASA SASA SASA ')'           { IfS $3 $4 $5 }
          | '(' lambda '(' var ')' SASA ')'      { FunS $4 $6 }
          | '(' SASA SASA ')'                    { AppS $2 $3 }
      
      parseError :: [Token] -> a
      parseError _ = error "Parse error"
    
    -------------------------------------revisar bien este segmento de código
    %%

      Exp   : let var '=' Exp in Exp  { Let $2 $4 $6 }
      | Exp1                    { Exp1 $1 }

      Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

      Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

      Factor
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }



      data Exp
      = Let String Exp Exp
      | Exp1 Exp1
      deriving Show

      data Exp1
      = Plus Exp1 Term
      | Minus Exp1 Term
      | Term Term
      deriving Show

      data Term
      = Times Term Factor
      | Div Term Factor
      | Factor Factor
      deriving Show

      data Factor
      = Int Int
      | Var String
      | Brack Exp
      deriving Show

      -----------------------------------------------------------------

      data SASA = NumS Int
               | BooleanS Bool
               | IdS String
               | AddS SASA SASA
               | SubS SASA SASA
               | NotS SASA
               | MulS SASA SASA
               | DivS SASA SASA
               | SqrtS SASA
               | ExptS SASA SASA
               | FstS SASA
               | SndS SASA
               | IfS SASA SASA SASA
               | FunS String SASA
               | AppS SASA SASA
               | LetS String SASA SASA
               | LetRecS String SASA SASA
               | LetCCS String SASA
               | IfS SASA SASA SASA
               | FunS String SASA
               | AppS SASA SASA
                deriving (Show, Eq)
    


      data Token = TokenNum Int
                   |TokenBool Bool
                   |TokenVar String
                   |TokenSuma
                   |TokenResta
                   |TokenMult
                   |TokenDiv
                   |TokenSqrt
                   |TokenExprt
                   |TokenNot
                   |TokenPA
                   |TokenPC
                   |TokenCA
                   |TokenCC
                   |TokenComa
                   |TokenIn
                   |TokenEq
                   |TokenMayor
                   |TokenMenor
                   |TokenMayorIgual
                   |TokenMenorIgual
                   |TokenDistinto
                   |TokenIf
                   |TokenFst
                   |TokenSnd
                   |TokenLet
                   |TokenLetVar
                   |TokenLambda
                   deriving (Show, Eq)
        -- revisar bien los tokens
