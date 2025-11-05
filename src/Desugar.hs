module Desugar where

import Grammars

-- | Operadores primitivos después del desugar
data Op
  = AddOp | SubOp | MulOp | DivOp
  | AndOp
  | EqOp | NeOp | LtOp | LeOp | GtOp | GeOp
  | ExptOp   | NullOp
  deriving (Show, Eq)
-- AST "desugared"
data ASA
  = Num Int
  | Boolean Bool
  | Id String
  | BinOp Op ASA ASA
  | Sqrt ASA
  | Not ASA
  | UnOp Op ASA
  | Pair ASA ASA
  | Fst ASA
  | Snd ASA
  | If ASA ASA ASA
  | Fun String ASA       -- lambda unaria (núcleo)
  | App ASA ASA          -- aplicación binaria (núcleo)
  | Closure String ASA [(String, ASA)]
  | Expr ASA [(String, ASA)] 
  deriving (Show, Eq)

-- Traducción de la sintaxis superficial (SASA del parser) al ASA
desugar :: SASA -> ASA
desugar (IdS i)              = Id i
desugar (NumS n)             = Num n
desugar (BooleanS b)         = Boolean b
desugar (NotS e)             = Not (desugar e)
desugar (NullS e)            = UnOp NullOp (desugar e)
-- operaciones n-arias (listas)
desugar (AddListS es)  = foldl1 (BinOp AddOp) (map desugar es)
desugar (SubListS es)  = foldl1 (BinOp SubOp) (map desugar es)
desugar (MulListS es)  = foldl1 (BinOp MulOp) (map desugar es)
desugar (DivListS es)  = foldl1 (BinOp DivOp) (map desugar es)

desugar (EqListS es)   = chainCompare EqOp (map desugar es)
desugar (LtListS es)   = chainCompare LtOp (map desugar es)
desugar (GtListS es)   = chainCompare GtOp (map desugar es)
desugar (LeListS es)   = chainCompare LeOp (map desugar es)
desugar (GeListS es)   = chainCompare GeOp (map desugar es)
desugar (NeListS es)   = chainCompare NeOp (map desugar es)
-- sqrt / expt
desugar (SqrtS e)      = Sqrt (desugar e)
desugar (ExptS a b)    = BinOp ExptOp (desugar a) (desugar b)
desugar (ExptListS es) = foldr1 (BinOp ExptOp) (map desugar es)

-- pares y accesos
desugar (PairS a b)    = Pair (desugar a) (desugar b)
desugar (FstS e)       = Fst (desugar e)
desugar (SndS e)       = Snd (desugar e)

--desugar (LetS p v c)         = App (Fun p (desugar c)) (desugar v)
desugar (LetS [] body) = desugar body
desugar (LetS [(x,e)] body) = App (Fun x (desugar body)) (desugar e)
desugar (LetS ((x,e):rest) body) =
  let params = map fst ((x,e):rest)
      exprs  = map (desugar . snd) ((x,e):rest)
      funBody = foldr Fun (desugar body) params
      appBody = foldl App funBody exprs
  in appBody
desugar (LetRecS p v c) =
  desugar (LetS [(p, AppS (IdS "Z") (FunS p v))] c)
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x,e):rest) body) =
  desugar (LetS [(x,e)] (LetStarS rest body))
desugar (IfS  c t e)         = If  (desugar c) (desugar t) (desugar e)
desugar (CondS clauses) = desugarCond clauses

desugar (FunS p c)           = Fun p (desugar c)
desugar (AppS f a)           = App (desugar f) (desugar a)

desugar (FunListS [] body)           = desugar body
desugar (FunListS (x:xs) body)       = Fun x (desugar (FunListS xs body))

-- listas: cons/nil
desugar (ListS [])     = Id "nil"
desugar (ListS (x:xs)) = App (App (Id "cons") (desugar x)) (desugar (ListS xs))
desugar (HeadS e)      = App (Id "head") (desugar e)
desugar (TailS e)      = App (Id "tail") (desugar e)


desugarCond :: [(SASA, SASA)] -> ASA
desugarCond [] = error "cond sin cláusulas"
desugarCond [(g,e)] = desugar e
desugarCond ((g,e):rest) = If (desugar g) (desugar e) (desugarCond rest)


-- comparaciones encadenadas
chainCompare :: Op -> [ASA] -> ASA
chainCompare _ []  = Boolean True
chainCompare _ [_] = Boolean True
chainCompare op (x:y:rest) =
  let cmp = BinOp op x y
  in case rest of
       [] -> cmp
       _  -> BinOp AndOp cmp (chainCompare op (y:rest))