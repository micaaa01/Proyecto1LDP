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

ASA 
      : int                          { Num $1 }
      | bool                         { Boolean $1 }
      | var                          { Id $1 }
      | '(' '+' ASA ASA ')'          { Add $3 $4 }
      | '(' '-' ASA ASA ')'          { Sub $3 $4 }
      | '(' "not" ASA ')'            { Not $3 }
      | '(' '*' ASA ASA ')'          { Mul $3 $4 }
      | '(' '/' ASA ASA ')'          { Div $3 $4 }
      | '(' "sqrt" ASA ')'           { Sqrt $3 }
      | '(' "expt" ASA ASA ')'       { Expt $3 $4 }
      | '(' "fst" ASA ')'            { Fst $3 } 
      | '(' "snd" ASA ')'            { Snd $3 }
      | '(' "if" ASA ASA ASA ')'     { If $3 $4 $5 }
      | '(' "let" '(' Bindings ')' ASA ')'    { Let $4 $6 }
      | '(' "let*" '(' Bindings ')' ASA ')'   { LetStar $4 $6 }
      | '(' "lambda" '(' var ')' ASA ')' { Fun $4 $6 }  
      | '(' ASA ASA ')'              { App $2 $3 }

Bindings
      : Binding                    { [$1] }
      | Binding Bindings            { $1 : $2 }

Binding
      : '(' var ASA ')'             { ($2, $3) }

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
data ASA 
      = Num Int
      | Boolean Bool
      | Id String
      | Add ASA ASA
      | Sub ASA ASA
      | Not ASA
      | Mul ASA ASA
      | Div ASA ASA
      | Sqrt ASA
      | Expt ASA ASA
      | Fst ASA
      | Snd ASA
      | If ASA ASA ASA
      | Fun String ASA
      | App ASA ASA
      | Let (String, ASA) ASA
      | LetStar [(String, ASA)] ASA
      deriving (Show, Eq)
       --variadicos
      --suma,resta, etc
      | AddList [ASA]
      | SubList [ASA]
      | MulList [ASA]
      | DivList [ASA]
      -- igual, mayor y menor que
      | EqList [ASA]    
      | LtList [ASA]          
      | GtList [ASA]
      -- listas y operciones
      | List [ASA]
      | Head ASA
      | Tail ASA
      -- condicional
      | Cond [(ASA, ASA)]
      -- lambda
      | FunMany [String] ASA
      | AppMany ASA [ASA]




    
parseError :: [Token] -> a
parseError _ = error "Error de parseo"
}
