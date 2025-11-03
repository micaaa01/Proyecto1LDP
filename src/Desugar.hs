module Desugar where
import Grammar (SASA(..))

data Op
  = AddOp 
  | SubOp 
  | MulOp 
  | DivOp
  | AndOp
  | EqOp 
  | NeOp 
  | LtOp 
  | LeOp 
  | GtOp 
  | GeOp
  | ExptOp
  deriving (Show, Eq)

data ASA = Num Int
                | Boolean Bool
                | Id String
                | BinOp Op ASA ASA
--                | Add ASA ASA
--                | Sub ASA ASA
--                | Mul ASA ASA
--                | Div ASA ASA
--                | Eq ASA ASA
--                | Lt ASA ASA
--                | Gt ASA ASA
--                | Le ASA ASA
--                | Ge ASA ASA
--                | Ne ASA ASA
                | Sqrt ASA
--                | Expt ASA ASA
                | Not ASA
                | Pair ASA ASA
                | Fst ASA
                | Snd ASA 
                | If ASA ASA ASA
                | Fun String ASA
                | App ASA ASA
                deriving (Show, Eq)

desugar :: SASA -> ASA
desugar (NumS n)                     = Num n
desugar (BooleanS b)                 = Boolean b
desugar (IdS x)                      = Id x
desugar (NotS e)                     = Not (desugar e)
desugar (AddListS es)                = foldl1 (BinOp AddOp) (map desugar es)
desugar (SubListS es)                = foldl1 (BinOp SubOp) (map desugar es)
desugar (MulListS es)                = foldl1 (BinOp MulOp) (map desugar es)
desugar (DivListS es)                = foldl1 (BinOp DivOp) (map desugar es)
desugar (EqListS es) = chainCompare EqOp (map desugar es)
desugar (LtListS es) = chainCompare LtOp (map desugar es)
desugar (GtListS es) = chainCompare GtOp (map desugar es)
desugar (LeListS es) = chainCompare LeOp (map desugar es)
desugar (GeListS es) = chainCompare GeOp (map desugar es)
desugar (NeListS es) = chainCompare NeOp (map desugar es)

desugar (FunListS [] body)           = desugar body
desugar (FunListS (x:xs) body)       = Fun x (desugar (FunListS xs body))
desugar (SqrtS e)                    = Sqrt (desugar e)
desugar (ExptS e1 e2)                = BinOp ExptOp (desugar e1) (desugar e2)
desugar (ExptListS es)               = foldr1 (BinOp ExptOp) (map desugar es)
desugar (PairS e1 e2)                = Pair (desugar e1) (desugar e2)
desugar (FstS e)                     = Fst (desugar e)
desugar (SndS e)                     = Snd (desugar e)
desugar (IfS c t f)                  = If (desugar c) (desugar t) (desugar f)
desugar (FunS x body)                = Fun x (desugar body)
desugar (AppS e1 e2)                 = App (desugar e1) (desugar e2)
desugar (LetS [] body)               = desugar body
desugar (LetS [(x, e)] body)         = App (Fun x (desugar body)) (desugar e)
desugar (LetS ((x,e):rest) body)     = desugar (LetS [(x,e)] (LetS rest body))
desugar (LetStarS [] body)           = desugar body
desugar (LetStarS ((x,e):rest) body) = desugar (LetS [(x,e)] (LetStarS rest body))
desugar (ListS [])                   = Id "nil"
desugar (ListS (x:xs))               = App (App (Id "cons") (desugar x)) (desugar (ListS xs))
desugar (HeadS e)                    = App (Id "head") (desugar e)
desugar (TailS e)                    = App (Id "tail") (desugar e)
desugar (CondS clauses)              = desugarCond clauses

desugarCond :: [(SASA, SASA)] -> ASA
desugarCond []                       = error "cond sin cláusulas"
desugarCond [(g, e)]                 = desugar e  
desugarCond ((g, e):rest)            = If (desugar g) (desugar e) (desugarCond rest)

chainCompare :: Op -> [ASA] -> ASA
chainCompare _ [] = Boolean True
chainCompare _ [_] = Boolean True
chainCompare op (x:y:rest) =
  let cmp = BinOp op x y
  in case rest of
       []     -> cmp
       (_:_)  -> BinOp AndOp cmp (chainCompare op (y:rest))
