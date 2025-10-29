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
      "head"          { TokenHead }
      "tail"          { TokenTail }
      "pair"          { TokenPair }
      "cond"          { TokenCond }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }


%%

SASA 
      : int                                                 { NumS $1 }
      | bool                                                { BooleanS $1 }
      | var                                                 { IdS $1 }
      | '(' "not" SASA ')'                                  { NotS $3 }
      | '(' '+' ExpList ')'                                 { AddListS $3 }
      | '(' '-' ExpList ')'                                 { SubListS $3 }
      | '(' '*' ExpList ')'                                 { MulListS $3 }
      | '(' '/' ExpList ')'                                 { DivListS $3 }
      | '(' '=' ExpList ')'                                 { EqListS $3 }
      | '(' '<' ExpList ')'                                 { LtListS $3 }
      | '(' '>' ExpList ')'                                 { GtListS $3 }
      | '(' '<=' ExpList ')'                                { LeListS $3 }
      | '(' '>=' ExpList ')'                                { GeListS $3 }
      | '(' '!=' ExpList ')'                                { NeListS $3 }
      | '(' "lambda" '(' VarList ')' SASA ')'               { FunListS $4 $6 }
      | '(' "sqrt" SASA ')'                                 { SqrtS $3 }
      | '(' "expt" ExpList ')' { ExptListS $3 }
      | '(' "pair" SASA SASA ')'     { PairS $3 $4}
      | '(' "fst" SASA ')'                                  { FstS $3 } 
      | '(' "snd" SASA ')'                                  { SndS $3 }
      | '(' SASA ',' SASA ')'                               { PairS $2 $4 }
      | '(' "if" SASA SASA SASA ')'                         { IfS $3 $4 $5 }
      | '(' "let" '(' Bindings ')' SASA ')'                 { LetS $4 $6 }
      | '(' "let*" '(' Bindings ')' SASA ')'                { LetStarS $4 $6 }
      | '[' ListItems ']'                                   { ListS $2 }
      | '[' ']'                                             { ListS [] }
      | '(' "head" SASA ')'                                 { HeadS $3 }
      | '(' "tail" SASA ')'                                 { TailS $3 }
      | '(' "cond" CondClauses ')'                          { CondS $3 }
      | '(' SASA SASAList ')'                               { foldl AppS $2 $3 }
      | '(' "add1" SASA ')'                                 { AddListS [$3, NumS 1] }
      | '(' "sub1" SASA ')'                                 { SubListS [$3, NumS 1] }


--Esta es un auxiliar      
ExpList
      : SASA                        { [$1] }
      | SASA ExpList                { $1 : $2 }

VarList
      : var                         { [$1] }
      | var VarList                 { $1 : $2 }

ListItems
      : SASA                        { [$1] }
      | SASA ',' ListItems          { $1 : $3 }


CondClause
    : '[' SASA SASA ']'             { ($2, $3) }
    | '[' var SASA ']' 
        { case $2 of
            "else" -> (BooleanS True, $3)
            _      -> error "Se esperaba 'else'"
        }

CondClauses
    : CondClause                    { [$1] }
    | CondClause CondClauses        { $1 : $2 }

      
SASAList
  : SASA                            { [$1] }
  | SASA SASAList                   { $1 : $2 }



Bindings
      : Binding                     { [$1] }
      | Binding Bindings            { $1 : $2 }

Binding
      : '(' var SASA ')'            { ($2, $3) }




{
data SASA 
      = NumS Int
      | BooleanS Bool
      | IdS String
      | NotS SASA
      | AddListS [SASA]
      | SubListS [SASA]
      | MulListS [SASA]
      | DivListS [SASA]
      | EqListS [SASA]
      | LtListS [SASA]
      | GtListS [SASA]
      | LeListS [SASA]
      | GeListS [SASA]
      | NeListS [SASA]
      | FunListS [String] SASA
      | SqrtS SASA
      | ExptS SASA SASA
      | ExptListS [SASA]
      | FstS SASA
      | SndS SASA
      | PairS SASA SASA
      | IfS SASA SASA SASA
      | FunS String SASA
      | AppS SASA SASA
      | LetS [(String, SASA)] SASA
      | LetStarS [(String, SASA)] SASA
      | ListS [SASA]
      | HeadS SASA
      | TailS SASA
      | CondS [(SASA, SASA)]
      deriving (Show, Eq)
     
    
parseError :: [Token] -> a
parseError _ = error "Error de parseo"


}
