module Desugar where
import Grammar (SASA(..))

data ASA = Num Int
                | Boolean Bool
                | Id String
                | Add ASA ASA
                | Sub ASA ASA
                | Mul ASA ASA
                | Div ASA ASA
                | Sqrt ASA
                | Expt ASA ASA
                | Not ASA
                | Fst ASA
                | Snd ASA
                | LetRec String ASA ASA   --No se que hace este
                | LetCC String ASA   --No se que hace este 
                | If ASA ASA ASA
                | Fun String ASA
                | App ASA ASA
                deriving (Show, Eq)

desugar :: SASA -> ASA
desugar (NumS n)                = Num n
desugar (BooleanS b)            = Boolean b
desugar (IdS x)                 = Id x
desugar (NotS e)                = Not (desugar e)
desugar (AddListS es)           = foldl1 Add (map desugar es)
desugar (SubListS es)           = foldl1 Sub (map desugar es)
desugar (MulListS es)           = foldl1 Mul (map desugar es)
desugar (DivListS es)           = foldl1 Div (map desugar es)
desugar (EqListS es)            = foldl1 (\a b -> App (App (Id "=") a) b) (map desugar es)
desugar (LtListS es)            = foldl1 (\a b -> App (App (Id "<") a) b) (map desugar es)
desugar (GtListS es)            = foldl1 (\a b -> App (App (Id ">") a) b) (map desugar es)
desugar (LeListS es)            = foldl1 (\a b -> App (App (Id "<=") a) b) (map desugar es)
desugar (GeListS es)            = foldl1 (\a b -> App (App (Id ">=") a) b) (map desugar es)
desugar (NeListS es)            = foldl1 (\a b -> App (App (Id "!=") a) b) (map desugar es)
desugar (FunListS [] body)      = desugar body
desugar (FunListS (x:xs) body)  = Fun x (desugar (FunListS xs body))
desugar (SqrtS e)               = Sqrt (desugar e)
desugar (ExptS e1 e2)           = Expt (desugar e1) (desugar e2)
desugar (ExptListS es)          = foldr1 Expt (map desugar es)
desugar (FstS e)                = Fst (desugar e)
desugar (SndS e)                = Snd (desugar e)
desugar (PairS e1 e2)           = App (App (Id "pair") (desugar e1)) (desugar e2)
desugar (IfS c t f)             = If (desugar c) (desugar t) (desugar f)
desugar (FunS x body)           = Fun x (desugar body)
desugar (AppS e1 e2)            = App (desugar e1) (desugar e2)
desugar (LetS [] body)          = desugar body
desugar (LetS [(x, e)] body)    = App (Fun x (desugar body)) (desugar e)
desugar (LetS ((x,e):rest) body) = desugar (LetS [(x,e)] (LetS rest body))
desugar (LetStarS [] body)      = desugar body
desugar (LetStarS ((x,e):rest) body) = desugar (LetS [(x,e)] (LetStarS rest body))
desugar (ListS []) = Id "nil"
desugar (ListS (x:xs))          = App (App (Id "cons") (desugar x)) (desugar (ListS xs))
desugar (HeadS e)               = App (Id "head") (desugar e)
desugar (TailS e)               = App (Id "tail") (desugar e)
desugar (CondS clauses)         = desugarCond clauses

desugarCond :: [(SASA, SASA)] -> ASA
desugarCond []              = error "cond sin cláusulas"
desugarCond [(g, e)]        = desugar e  -- última cláusula (else)
desugarCond ((g, e):rest)   = If (desugar g) (desugar e) (desugarCond rest)

