module Desugar where
import Grammar (SASA(..))

-- | Operadores primitivos después del desugar
data Op
  = AddOp | SubOp | MulOp | DivOp
  | AndOp
  | EqOp | NeOp | LtOp | LeOp | GtOp | GeOp
  | ExptOp
  deriving (Show, Eq)

-- | AST después del desugar (ASA)
data ASA
  = Num Int
  | Boolean Bool
  | Id String
  | BinOp Op ASA ASA
  | Sqrt ASA
  | Not ASA
  | Pair ASA ASA
  | Fst ASA
  | Snd ASA
  | If ASA ASA ASA
  | Fun String ASA       -- lambda unaria (núcleo)
  | App ASA ASA          -- aplicación binaria (núcleo)
  | FunList [String] ASA -- lambda n-aria
  | AppList ASA [ASA]    -- aplicación n-aria
  deriving (Show, Eq)

-- | Desugar principal (SASA -> ASA)
desugar :: SASA -> ASA
desugar (NumS n)       = Num n
desugar (BooleanS b)   = Boolean b
desugar (IdS x)        = Id x
desugar (NotS e)       = Not (desugar e)

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

-- funciones n-arias
desugar (FunListS vars body) = FunList vars (desugar body)

-- sqrt / expt
desugar (SqrtS e)      = Sqrt (desugar e)
desugar (ExptS a b)    = BinOp ExptOp (desugar a) (desugar b)
desugar (ExptListS es) = foldr1 (BinOp ExptOp) (map desugar es)

-- pares y accesos
desugar (PairS a b)    = Pair (desugar a) (desugar b)
desugar (FstS e)       = Fst (desugar e)
desugar (SndS e)       = Snd (desugar e)

-- if
desugar (IfS c t f)    = If (desugar c) (desugar t) (desugar f)

-- función y aplicación unarias
desugar (FunS x body)  = Fun x (desugar body)
desugar (AppS a b)     = App (desugar a) (desugar b)

-- let: paralelo -> AppList (FunList vars body) args
desugar (LetS binds body) =
  let (vars, exprs) = unzip binds
      body' = desugar body
      args' = map desugar exprs
  in AppList (FunList vars body') args'

-- let*: secuencial (se genera anidado)
desugar (LetStarS [] body) = desugar body
desugar (LetStarS ((x,e):rest) body) = desugar (LetS [(x,e)] (LetStarS rest body))

-- listas: cons/nil
desugar (ListS [])     = Id "nil"
desugar (ListS (x:xs)) = App (App (Id "cons") (desugar x)) (desugar (ListS xs))
desugar (HeadS e)      = App (Id "head") (desugar e)
desugar (TailS e)      = App (Id "tail") (desugar e)

-- cond -> if anidados
desugar (CondS clauses) = desugarCond clauses

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

-- lowerASA: convierte FunList/AppList a Fun/App anidados (opcional)
lowerASA :: ASA -> ASA
lowerASA (Num n)        = Num n
lowerASA (Boolean b)    = Boolean b
lowerASA (Id x)         = Id x
lowerASA (BinOp o a b)  = BinOp o (lowerASA a) (lowerASA b)
lowerASA (Sqrt a)       = Sqrt (lowerASA a)
lowerASA (Not a)        = Not (lowerASA a)
lowerASA (Pair a b)     = Pair (lowerASA a) (lowerASA b)
lowerASA (Fst a)        = Fst (lowerASA a)
lowerASA (Snd a)        = Snd (lowerASA a)
lowerASA (If c t f)     = If (lowerASA c) (lowerASA t) (lowerASA f)
lowerASA (Fun x body)   = Fun x (lowerASA body)
lowerASA (App f a)      = App (lowerASA f) (lowerASA a)
lowerASA (FunList vars body) = foldr Fun (lowerASA body) vars
lowerASA (AppList f args) = foldl App (lowerASA f) (map lowerASA args)
