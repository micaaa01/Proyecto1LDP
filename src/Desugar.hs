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
desugar (NumS n)             = Num n
desugar (BooleanS b)         = Boolean b
desugar (IdS x)              = Id x
desugar (AddS e1 e2)         = Add (desugar e1) (desugar e2)
desugar (SubS e1 e2)         = Sub (desugar e1) (desugar e2)
desugar (NotS e)             = Not (desugar e)
desugar (MulS e1 e2)         = Mul (desugar e1) (desugar e2)
desugar (DivS e1 e2)         = Div (desugar e1) (desugar e2)
desugar (EqS e1 e2) = App (App (Id "=") (desugar e1)) (desugar e2)
desugar (LtS e1 e2) = App (App (Id "<") (desugar e1)) (desugar e2)
desugar (GtS e1 e2) = App (App (Id ">") (desugar e1)) (desugar e2)
desugar (LeS e1 e2) = App (App (Id "<=") (desugar e1)) (desugar e2)
desugar (GeS e1 e2) = App (App (Id ">=") (desugar e1)) (desugar e2)
desugar (NeS e1 e2) = App (App (Id "!=") (desugar e1)) (desugar e2)
desugar (AddListS es) = foldl1 Add (map desugar es)
desugar (SubListS es) = foldl1 Sub (map desugar es)
desugar (MulListS es) = foldl1 Mul (map desugar es)
desugar (DivListS es) = foldl1 Div (map desugar es)
desugar (EqListS es) = foldl1 (\a b -> App (App (Id "=") a) b) (map desugar es)
desugar (LtListS es) = foldl1 (\a b -> App (App (Id "<") a) b) (map desugar es)
desugar (GtListS es) = foldl1 (\a b -> App (App (Id ">") a) b) (map desugar es)
desugar (SqrtS e)            = Sqrt (desugar e)
desugar (ExptS e1 e2)        = Expt (desugar e1) (desugar e2)
desugar (FstS e)             = Fst (desugar e)
desugar (SndS e)             = Snd (desugar e)
desugar (IfS c t f)          = If (desugar c) (desugar t) (desugar f)
desugar (FunS x body)        = Fun x (desugar body)
desugar (AppS e1 e2)         = App (desugar e1) (desugar e2)
desugar (LetS [] body) = desugar body
desugar (LetS [(x, e)] body) = App (Fun x (desugar body)) (desugar e)
desugar (LetS ((x,e):rest) body) = desugar (LetS [(x,e)] (LetS rest body))
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x,e):rest) body) = desugar (LetS [(x,e)] (LetStarS rest body))
