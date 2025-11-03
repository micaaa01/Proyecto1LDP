-- Este módulo implementa la fase de *desazucarado* (“desugaring”) del lenguaje SASA.
-- Convierte las construcciones sintácticas de alto nivel (como listas, let*, cond, etc.)
-- en combinaciones más básicas del lenguaje núcleo, denominadas ASA.

module Desugar where
import Grammar (SASA(..))

-- | Tipo de dato 'Op' representa las operaciones binarias primitivas.
-- Estas operaciones son los operadores básicos admitidos por el lenguaje
-- después del proceso de desugar.
data Op
  = AddOp    -- ^ Suma (+)
  | SubOp    -- ^ Resta (-)
  | MulOp    -- ^ Multiplicación (*)
  | DivOp    -- ^ División (/)
  | AndOp    -- ^ Conjunción lógica (and)
  | EqOp     -- ^ Igualdad (=)
  | NeOp     -- ^ Distinto (!=)
  | LtOp     -- ^ Menor que (<)
  | LeOp     -- ^ Menor o igual (<=)
  | GtOp     -- ^ Mayor que (>)
  | GeOp     -- ^ Mayor o igual (>=)
  | ExptOp   -- ^ Exponenciación (expt)
  deriving (Show, Eq)

--  Tipo de dato 'ASA' (Abstract Syntax After desugaring)
-- Representa las expresiones del lenguaje SASA en su forma desazucarada,
-- más simple y adecuada para evaluación.
data ASA  = Num Int                    -- ^ Constante numérica
    | Boolean Bool               -- ^ Valor booleano
    | Id String                  -- ^ Identificador o variable
    | BinOp Op ASA ASA           -- ^ Operación binaria genérica
    | Sqrt ASA                   -- ^ Raíz cuadrada
    | Not ASA                    -- ^ Negación lógica
    | Pair ASA ASA               -- ^ Par de valores
    | Fst ASA                    -- ^ Primer elemento del par
    | Snd ASA                    -- ^ Segundo elemento del par
    | If ASA ASA ASA             -- ^ Condicional if
    | Fun String ASA             -- ^ Abstracción lambda (función)
    | App ASA ASA                -- ^ Aplicación de función
    deriving (Show, Eq)
              
-- Función principal de desazucarado.
-- Convierte una expresión del tipo 'SASA' (AST del parser)
-- en una expresión del tipo 'ASA' (AST simplificado).
desugar :: SASA -> ASA
desugar (NumS n)                     = Num n
desugar (BooleanS b)                 = Boolean b
desugar (IdS x)                      = Id x
desugar (NotS e)                     = Not (desugar e)
-- Operaciones aritméticas y comparativas
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
-- Funciones (λx₁ x₂ ... xn. body) -> anidado en Fun x₁ (Fun x₂ (... body))
desugar (FunListS [] body)           = desugar body
desugar (FunListS (x:xs) body)       = Fun x (desugar (FunListS xs body))
-- Raíz cuadrada y exponenciación
desugar (SqrtS e)                    = Sqrt (desugar e)
desugar (ExptS e1 e2)                = BinOp ExptOp (desugar e1) (desugar e2)
desugar (ExptListS es)               = foldr1 (BinOp ExptOp) (map desugar es)
-- Pares
desugar (PairS e1 e2)                = Pair (desugar e1) (desugar e2)
desugar (FstS e)                     = Fst (desugar e)
desugar (SndS e)                     = Snd (desugar e)
-- Condicional
desugar (IfS c t f)                  = If (desugar c) (desugar t) (desugar f)
-- Función simple y aplicación
desugar (FunS x body)                = Fun x (desugar body)
desugar (AppS e1 e2)                 = App (desugar e1) (desugar e2)
-- Expresiones let (convertidas a aplicaciones de función)
desugar (LetS [] body)               = desugar body
desugar (LetS [(x, e)] body)         = App (Fun x (desugar body)) (desugar e)
desugar (LetS ((x,e):rest) body)     = desugar (LetS [(x,e)] (LetS rest body))
-- let* (evaluación secuencial de asignaciones)
desugar (LetStarS [] body)           = desugar body
desugar (LetStarS ((x,e):rest) body) = desugar (LetS [(x,e)] (LetStarS rest body))
-- Listas (evaluación secuencial de asignaciones)
desugar (ListS [])                   = Id "nil"
desugar (ListS (x:xs))               = App (App (Id "cons") (desugar x)) (desugar (ListS xs))
-- Funciones sobre listas
desugar (HeadS e)                    = App (Id "head") (desugar e)
desugar (TailS e)                    = App (Id "tail") (desugar e)
-- cond -> if anidados
desugar (CondS clauses)              = desugarCond clauses

-- Traduce una lista de cláusulas de 'cond' en una estructura de if anidados.
-- Ejemplo:
-- > (cond [p1 e1] [p2 e2] [else e3])
-- se convierte en:
-- > if p1 then e1 else if p2 then e2 else e3
desugarCond :: [(SASA, SASA)] -> ASA
desugarCond []                       = error "cond sin cláusulas"
desugarCond [(g, e)]                 = desugar e  
desugarCond ((g, e):rest)            = If (desugar g) (desugar e) (desugarCond rest)

-- Evalúa comparaciones encadenadas como:
-- > (< 1 2 3)
-- que se desazucara a:
-- > (and (< 1 2) (< 2 3))
chainCompare :: Op -> [ASA] -> ASA
chainCompare _ [] = Boolean True
chainCompare _ [_] = Boolean True
chainCompare op (x:y:rest) =
  let cmp = BinOp op x y
  in case rest of
       []     -> cmp
       (_:_)  -> BinOp AndOp cmp (chainCompare op (y:rest))
