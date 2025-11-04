module Interp where

import Desugar

type Env = [(String, ASA)]

-- Importante: En esta versión del intérprete usamos evaluación ansiosa junto con el 
-- combinador Z y la semántica de paso pequeño. En este contexto, durante la evaluación 
-- de una función recursiva, el cuerpo puede contener variables (como ‘f’ en Z) que 
-- deben mantenerse accesibles dentro de su propio entorno, sin propagarse al contexto 
-- externo. 
--
-- Para preservar correctamente esas ligaduras, realizamos dos ajustes fundamentales:
--
-- • Al aplicar una cerradura, el cuerpo se devuelve envuelto en una expresión con 
--   entorno: ExprV b env''. De esta forma, la evaluación del cuerpo ocurre dentro 
--   del ambiente extendido que contiene la variable recursiva, evitando la pérdida 
--   de ligaduras y asegurando que las referencias a ‘f’ o ‘r’ sean válidas.
--
-- • Dado que ahora el cuerpo se representa como una ExprV, el sistema debe saber 
--   cómo avanzar dentro de ella. Para ello se añade una regla que permite reducir 
--   un paso en el entorno interno de la expresión, manteniendo intacto el ambiente 
--   del contexto. Esto garantiza que la evaluación siga siendo ansiosa, pero que 
--   cada función recursiva conserve su propio entorno léxico independiente.


smallStep :: ASA -> Env -> (ASA, Env)
smallStep (Id "nil") env = (Id "nil", env)
smallStep (Id i) env  = (lookupEnv i env, env)       
smallStep (Num n) env = (Num n, env)
smallStep (Boolean b) env = (Boolean b, env)
smallStep (BinOp op v1 v2) env
  | isValue v1 && isValue v2 = (evalBinOp op v1 v2, env)
smallStep (BinOp op v1 e2) env
  | isValue v1 = 
      let (e2', env') = smallStep e2 env
      in (BinOp op v1 e2', env')
smallStep (BinOp op e1 e2) env =
  let (e1', env') = smallStep e1 env
  in (BinOp op e1' e2, env')

smallStep (Sqrt (Num n)) env =
    let r = sqrt (fromIntegral n :: Double)
    in (Num (floor r), env)
smallStep (Sqrt e) env =
    let (e', env') = smallStep e env
    in (Sqrt e', env')

-- not
smallStep (Not (Boolean b)) env = (Boolean (not b), env)
smallStep (Not e) env =
    let (e', env') = smallStep e env
    in (Not e', env')

smallStep (Pair v1 v2) env 
    | isValue v1 && isValue v2 = (Pair v1 v2 , env)
smallStep (Pair v1 e2) env 
    | isValue v1 = let (e2', env') = smallStep e2 env
                    in (Pair v1 e2', env')
smallStep (Pair e1 e2) env = 
    let (e1', env') = smallStep e1 env
    in (Pair e1' e2, env')
smallStep (Fst (Pair v1 v2)) env 
    |isValue v1 && isValue v2 = (v1, env)
smallStep (Fst e) env =
    let (e', env') = smallStep e env
    in (Fst e', env')
smallStep (Snd (Pair v1 v2)) env 
    |isValue v1 && isValue v2 = (v2, env)
smallStep (Snd e) env =
    let (e', env') = smallStep e env
    in (Snd e', env')   

-- If
smallStep (If (Boolean True) eThen eElse) env =
    (eThen, env)
smallStep (If (Boolean False) eThen eElse) env =
    (eElse, env)
smallStep (If cond eThen eElse) env =
    let (cond', env') = smallStep cond env
    in (If cond' eThen eElse, env')


smallStep (App (App (Id "cons") v1) v2) env
  | isValue v1 && isValue v2 = (Pair v1 v2, env)
smallStep (App (App (Id "cons") v1) e2) env
  | isValue v1 =
      let (e2', env') = smallStep e2 env
      in (App (App (Id "cons") v1) e2', env')
smallStep (App (App (Id "cons") e1) e2) env =
    let (e1', env') = smallStep e1 env
    in (App (App (Id "cons") e1') e2, env')
smallStep (App (Id "head") arg) env
  | isValue arg =
      case arg of
        Pair v _ -> (v, env)
        _        -> error "head: argumento no es una lista"
  | otherwise =
      let (arg', env') = smallStep arg env
      in (App (Id "head") arg', env')
smallStep (App (Id "tail") arg) env
  | isValue arg =
      case arg of
        Pair _ v2 -> (v2, env)
        _         -> error "tail: argumento no es una lista"
  | otherwise =
      let (arg', env') = smallStep arg env
      in (App (Id "tail") arg', env')

-- Funciones: convertir Fun -> Closure con entorno léxico actual
smallStep (Fun p body) env =
    (Closure p body env, env)

-- Aplicación (call-by-value)
smallStep (App e1 e2) env
  -- Caso 1: ambos son valores -> aplicar clausura
  | isValue e1 && isValue e2 =
      case e1 of
        Closure param body env' ->
          let envInner = (param, e2) : env'   -- EXTENDER el entorno de la clausura
          in (Expr body envInner, env)        -- devolver Expr(body, envInner) pero conservar el env exterior
        _ -> error "Intento de aplicar algo que no es una clausura"

  -- Caso 2: función es valor, argumento no -> reducir argumento (ansioso)
  | isValue e1 =
      let (e2', env') = smallStep e2 env
      in (App e1 e2', env')

  -- Caso 3: reducir la función
  | otherwise =
      let (e1', env') = smallStep e1 env
      in (App e1' e2, env')
-- Reducir dentro de una Expr (evaluar un paso en su entorno interno).
-- Conservamos el entorno exterior (outerEnv) intacto.
smallStep (Expr e innerEnv) outerEnv
  | isValue e = (e, outerEnv)   -- cuando la expresión interna ya es valor, vuelve al contexto exterior
  | otherwise =
      let (e', newInnerEnv) = smallStep e innerEnv
      in (Expr e' newInnerEnv, outerEnv)








-- Interpretación total
interp :: ASA -> Env -> ASA
interp e env
  | isValue e = e
  | otherwise =
      let (e', env') = smallStep e env
      in interp e' env'

isValue (Num _) = True
isValue (Boolean _) = True
isValue (Closure _ _ _) = True   -- los cierres son valores
isValue (Pair v1 v2) = isValue v1 && isValue v2
isValue (Id "nil") = True
isValue _ = False


lookupEnv :: String -> Env -> ASA
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j, v) : env)
  | i == j    = v
  | otherwise = lookupEnv i env

numN :: ASA -> Int
numN (Num n) = n

boolN :: ASA -> Bool
boolN (Boolean b) = b
boolN _ = False

closureP :: ASA -> String
closureP (Closure p _ _) = p

closureC :: ASA -> ASA
closureC (Closure _ c _) = c

closureE :: ASA -> Env
closureE (Closure _ _ e) = e

evalBinOp :: Op -> ASA -> ASA -> ASA
-- aritmética
evalBinOp AddOp (Num x) (Num y) = Num (x + y)
evalBinOp SubOp (Num x) (Num y) = Num (x - y)
evalBinOp MulOp (Num x) (Num y) = Num (x * y)
evalBinOp DivOp (Num x) (Num y) 
    | y==0    = error "Division entre cero"
    | otherwise = Num (x `div` y)
evalBinOp ExptOp (Num x) (Num y) = Num (x ^ y)
-- comparaciones
evalBinOp EqOp (Num n1) (Num n2) = Boolean (n1 == n2)
evalBinOp EqOp (Boolean b1) (Boolean b2) = Boolean (b1 == b2)
evalBinOp EqOp (Pair a1 b1) (Pair a2 b2) =
  Boolean (isTrue (evalBinOp EqOp a1 a2) && isTrue (evalBinOp EqOp b1 b2))
evalBinOp NeOp  x y = Boolean (x /= y)
evalBinOp LtOp  (Num x) (Num y) = Boolean (x < y)
evalBinOp LeOp  (Num x) (Num y) = Boolean (x <= y)
evalBinOp GtOp  (Num x) (Num y) = Boolean (x > y)
evalBinOp GeOp  (Num x) (Num y) = Boolean (x >= y)
-- lógicos
evalBinOp AndOp (Boolean x) (Boolean y) = Boolean (x && y)
evalBinOp AndOp (Boolean x) e
    | not x = Boolean False
    | otherwise = e
evalBinOp _ _ _ = error "Operación binaria inválida"

isTrue :: ASA -> Bool
isTrue (Boolean True) = True
isTrue _ = False