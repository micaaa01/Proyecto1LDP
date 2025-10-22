module Desugar where
import Grammar (ASA(..))

data SASA = NumS Int
                | BooleanS Bool
                | IdS String
                | AddS SASA SASA
                | SubS SASA SASA
                | MulS SASA SASA
                | DivS SASA SASA
                | SqrtS SASA
                | ExptS SASA SASA
                | NotS SASA
                | FstS SASA
                | SndS SASA
                | LetRecS String SASA SASA   --No se que hace este
                | LetCCS String SASA   --No se que hace este 
                | IfS SASA SASA SASA
                | FunS String SASA
                | AppS SASA SASA
                deriving (Show, Eq)

desugar :: ASA -> SASA
desugar (Num n)             = NumS n
desugar (Boolean b)         = BooleanS b
desugar (Id x)              = IdS x
desugar (Add e1 e2)         = AddS (desugar e1) (desugar e2)
desugar (Sub e1 e2)         = SubS (desugar e1) (desugar e2)
desugar (Not e)             = NotS (desugar e)
desugar (Mul e1 e2)         = MulS (desugar e1) (desugar e2)
desugar (Div e1 e2)         = DivS (desugar e1) (desugar e2)
desugar (Sqrt e)            = SqrtS (desugar e)
desugar (Expt e1 e2)        = ExptS (desugar e1) (desugar e2)
desugar (Fst e)             = FstS (desugar e)
desugar (Snd e)             = SndS (desugar e)
desugar (If c t f)          = IfS (desugar c) (desugar t) (desugar f)
desugar (Fun x body)        = FunS x (desugar body)
desugar (App e1 e2)         = AppS (desugar e1) (desugar e2)
desugar (Let [] body) = desugar body
desugar (Let [(x, e)] body) = AppS (FunS x (desugar body)) (desugar e)
desugar (Let ((x,e):rest) body) = desugar (Let [(x,e)] (Let rest body))
desugar (LetStar [] body) = desugar body
desugar (LetStar ((x,e):rest) body) = desugar (Let [(x,e)] (LetStar rest body))
