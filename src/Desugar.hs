module Desugar where
import Grammar (SASA(..))

data ASA = Num Int
                | BooleanS Bool
                | IdS String
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
desugar (Num n)             = Num n
desugar (Boolean b)         = Boolean b
desugar (Id x)              = Id x
desugar (Add e1 e2)         = Add (desugar e1) (desugar e2)
desugar (Sub e1 e2)         = Sub (desugar e1) (desugar e2)
desugar (Not e)             = Not (desugar e)
desugar (Mul e1 e2)         = Mul (desugar e1) (desugar e2)
desugar (Div e1 e2)         = Div (desugar e1) (desugar e2)
desugar (Sqrt e)            = Sqrt (desugar e)
desugar (Expt e1 e2)        = Expt (desugar e1) (desugar e2)
desugar (Fst e)             = Fst (desugar e)
desugar (Snd e)             = Snd (desugar e)
desugar (If c t f)          = If (desugar c) (desugar t) (desugar f)
desugar (Fun x body)        = Fun x (desugar body)
desugar (App e1 e2)         = App (desugar e1) (desugar e2)
desugar (Let [] body) = desugar body
desugar (Let [(x, e)] body) = App (Fun x (desugar body)) (desugar e)
desugar (Let ((x,e):rest) body) = desugar (Let [(x,e)] (Let rest body))
desugar (LetStar [] body) = desugar body
desugar (LetStar ((x,e):rest) body) = desugar (Let [(x,e)] (LetStar rest body))
