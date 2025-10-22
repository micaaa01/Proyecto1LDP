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
    
parseError :: [Token] -> a
parseError _ = error "Error de parseo"
}
