{
module Grammar where
import Lexer (Token(..))
}

%name parser
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
      "in"            { TokenIn }
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
      "lambda"        { TokenLambda }

%%

SASA 
      : int                          { NumS $1 }
      | bool                         { BooleanS $1 }
      | var                          { IdS $1 }
      | '(' '+' SASA SASA ')'          { AddS $3 $4 }
      | '(' '-' SASA SASA ')'          { SubS $3 $4 }
      | '(' "not" SASA ')'            { NotS $3 }
      | '(' '*' SASA SASA ')'          { MulS $3 $4 }
      | '(' '/' SASA SASA ')'          { DivS $3 $4 }
      | '(' '=' SASA SASA ')'        { EqS $3 $4 }
      | '(' '<' SASA SASA ')'        { LtS $3 $4 }
      | '(' '>' SASA SASA ')'        { GtS $3 $4 }
      | '(' '<=' SASA SASA ')'       { LeS $3 $4 }
      | '(' '>=' SASA SASA ')'       { GeS $3 $4 }
      | '(' '!=' SASA SASA ')'       { NeS $3 $4 }
      | '(' '+' ExpList ')'   { AddListS $3 }
      | '(' '-' ExpList ')'   { SubListS $3 }
      | '(' '*' ExpList ')'   { MulListS $3 }
      | '(' '/' ExpList ')'   { DivListS $3 }
      | '(' '=' ExpList ')'   { EqListS $3 }
      | '(' '<' ExpList ')'   { LtListS $3 }
      | '(' '>' ExpList ')'   { GtListS $3 }
      | '(' "sqrt" SASA ')'           { SqrtS $3 }
      | '(' "expt" SASA SASA ')'       { ExptS $3 $4 }
      | '(' "fst" SASA ')'            { FstS $3 } 
      | '(' "snd" SASA ')'            { SndS $3 }
      | '(' "if" SASA SASA SASA ')'     { IfS $3 $4 $5 }
      | '(' "let" '(' Bindings ')' SASA ')'    { LetS $4 $6 }
      | '(' "let*" '(' Bindings ')' SASA ')'   { LetStarS $4 $6 }
      | '(' "lambda" '(' var ')' SASA ')' { FunS $4 $6 }  
      | '(' SASA SASA ')'              { AppS $2 $3 }

--Esta es un auxiliar      
ExpList
      : SASA                  { [$1] }
      | SASA ExpList          { $1 : $2 }


Bindings
      : Binding                    { [$1] }
      | Binding Bindings            { $1 : $2 }

Binding
      : '(' var SASA ')'             { ($2, $3) }




{
data SASA 
      = NumS Int
      | BooleanS Bool
      | IdS String
      | AddS SASA SASA
      | SubS SASA SASA
      | NotS SASA
      | MulS SASA SASA
      | DivS SASA SASA
      | EqS SASA SASA
      | LtS SASA SASA
      | GtS SASA SASA
      | LeS SASA SASA
      | GeS SASA SASA
      | NeS SASA SASA
      | AddListS [SASA]
      | SubListS [SASA]
      | MulListS [SASA]
      | DivListS [SASA]
      | EqListS [SASA]
      | LtListS [SASA]
      | GtListS [SASA]
      | SqrtS SASA
      | ExptS SASA SASA
      | FstS SASA
      | SndS SASA
      | IfS SASA SASA SASA
      | FunS String SASA
      | AppS SASA SASA
      | LetS [(String, SASA)] SASA
      | LetStarS [(String, SASA)] SASA
      deriving (Show, Eq)
     
    
parseError :: [Token] -> a
parseError _ = error "Error de parseo"
}
