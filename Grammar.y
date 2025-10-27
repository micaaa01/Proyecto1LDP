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
      : int                          { Num $1 }
      | bool                         { Boolean $1 }
      | var                          { Id $1 }
      | '(' '+' SASA SASA ')'          { Add $3 $4 }
      | '(' '-' SASA SASA ')'          { Sub $3 $4 }
      | '(' "not" SASA ')'            { Not $3 }
      | '(' '*' SASA SASA ')'          { Mul $3 $4 }
      | '(' '/' SASA SASA ')'          { Div $3 $4 }
      | '(' "sqrt" SASA ')'           { Sqrt $3 }
      | '(' "expt" SASA SASA ')'       { Expt $3 $4 }
      | '(' "fst" SASA ')'            { Fst $3 } 
      | '(' "snd" SASA ')'            { Snd $3 }
      | '(' "if" SASA SASA SASA ')'     { If $3 $4 $5 }
      | '(' "let" '(' Bindings ')' SASA ')'    { Let $4 $6 }
      | '(' "let*" '(' Bindings ')' SASA ')'   { LetStar $4 $6 }
      | '(' "lambda" '(' var ')' SASA ')' { Fun $4 $6 }  
      | '(' SASA SASA ')'              { App $2 $3 }

Bindings
      : Binding                    { [$1] }
      | Binding Bindings            { $1 : $2 }

Binding
      : '(' var SASA ')'             { ($2, $3) }

--Operadores variadicos
--suma, resta, etc
Exp : '(' '+' ExpList ')'   { AddList $3 }
     | '(' '-' ExpList ')'   { SubList $3 }
     | '(' '*' ExpList ')'   { MulList $3 }
     | '(' '/' ExpList ')'   { DivList $3 }

ExpList
  : Exp                     { [$1] }
  | Exp ExpList             { $1 : $2 }

-- igual, mayor y menor que
Exp : '(' '=' ExpList ')'   { EqList $3 }
     | '(' '<' ExpList ')'  { LtList $3 }
     | '(' '>' ExpList ')'  { GtList $3 }

-- listas y operciones
Exp : '[' ListItems ']'     { List $2 }
     | '(' "head" Exp ')'   { Head $3 }
     | '(' "tail" Exp ')'   { Tail $3 }

ListItems
  : Exp                     { [$1] }
  | Exp ',' ListItems       { $1 : $3 }

-- condicional
Exp : '(' "cond" '[' Clauses ']' ')'   { Cond $3 }

Clauses
  : Clause                            { [$1] }
  | Clause Clauses                    { $1 : $2 }

Clause
  : '[' Exp Exp ']'                   { ($2, $3) }  -- guarda, cuerpo
  | '[' "else" Exp ']'                { (BoolTrue, $3) }  -- usa un ASA especial para else

-- lambda variadicos
Exp : '(' "lambda" '(' Params ')' Exp ')'   { FunMany $4 $6 }
     | '(' Exp ExpList ')'                  { AppMany $2 $3 }

Params
  : var                     { [$1] }
  | var Params              { $1 : $2 }

AppList
  : Exp                     { [$1] }
  | Exp AppList             { $1 : $2 }



{
data SASA 
      = Num Int
      | Boolean Bool
      | Id String
      | Add SASA SASA
      | Sub SASA SASA
      | Not SASA
      | Mul SASA SASA
      | Div SASA SASA
      | Sqrt SASA
      | Expt SASA SASA
      | Fst SASA
      | Snd SASA
      | If SASA SASA SASA
      | Fun String SASA
      | App SASA SASA
      | Let (String, SASA) SASA
      | LetStar [(String, SASA)] SASA
      deriving (Show, Eq)
       --variadicos
      --suma,resta, etc
      | AddList [SASA]
      | SubList [SASA]
      | MulList [SASA]
      | DivList [SASA]
      -- igual, mayor y menor que
      | EqList [SASA]    
      | LtList [SASA]          
      | GtList [SASA]
      -- listas y operciones
      | List [SASA]
      | Head SASA
      | Tail SASA
      -- condicional
      | Cond [(SASA, SASA)]
      -- lambda
      | FunMany [String] SASA
      | AppMany SASA [SASA]




    
parseError :: [Token] -> a
parseError _ = error "Error de parseo"
}
