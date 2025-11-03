module Eval where
import Desugar (ASA (..), Op(..))

-- Valores internos (para entorno)
data Value
    = VNum Int
    | VBool Bool
    | VClosure String ASA Env           -- closure unario
    | VClosureN [String] ASA Env        -- closure n-ario
    | VPair Value Value
    deriving (Show, Eq)

type Env = [(String, Value)]

smallStep :: ASA -> Env -> (ASA, Env)

-- variables / nil
smallStep (Id "nil") env = (Id "nil", env)
smallStep (Id i) env = case lookupEnv i env of
        Just v  -> (fromValue v, env)
        Nothing -> error ("Variable no encontrada: " ++ i)

-- constantes
smallStep (Num n) env = (Num n, env)
smallStep (Boolean b) env = (Boolean b, env)

-- operación binaria: evaluar operando izquierdo/derecho
smallStep (BinOp op v1 v2) env
  | isValue v1 && isValue v2 = (evalBinOp op v1 v2, env)
smallStep (BinOp op v1 e2) env
  | isValue v1 =
      let (e2', env') = smallStep e2 env
      in (BinOp op v1 e2', env')
smallStep (BinOp op e1 e2) env =
  let (e1', env') = smallStep e1 env
  in (BinOp op e1' e2, env')

-- sqrt
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

-- pares / cons semantics (App (App (Id "cons") a) b) -> Pair
smallStep (Pair v1 v2) env 
    | isValue v1 && isValue v2 = (Pair v1 v2 , env)
smallStep (Pair v1 e2) env 
    | isValue v1 = let (e2', env') = smallStep e2 env
                    in (Pair v1 e2', env')
smallStep (Pair e1 e2) env = 
    let (e1', env') = smallStep e1 env
    in (Pair e1' e2, env')

smallStep (Fst (Pair v1 v2)) env 
    | isValue v1 && isValue v2 = (v1, env)
smallStep (Fst e) env =
    let (e', env') = smallStep e env
    in (Fst e', env')

smallStep (Snd (Pair v1 v2)) env 
    | isValue v1 && isValue v2 = (v2, env)
smallStep (Snd e) env =
    let (e', env') = smallStep e env
    in (Snd e', env')   

-- if
smallStep (If (Boolean True) eThen eElse) env = (eThen, env)
smallStep (If (Boolean False) eThen eElse) env = (eElse, env)
smallStep (If cond eThen eElse) env =
    let (cond', env') = smallStep cond env
    in (If cond' eThen eElse, env')

-- funciones literales (son valores)
smallStep (Fun x body) env = (Fun x body, env)
smallStep (FunList vars body) env = (FunList vars body, env)

-- head/tail sobre listas representadas por Pair / nil
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

-- cons: App (App (Id "cons") a) b -> Pair or evaluate components
smallStep (App (App (Id "cons") v1) v2) env
  | isValue v1 && isValue v2 = (Pair v1 v2, env)
smallStep (App (App (Id "cons") v1) e2) env
  | isValue v1 =
      let (e2', env') = smallStep e2 env
      in (App (App (Id "cons") v1) e2', env')
smallStep (App (App (Id "cons") e1) e2) env =
    let (e1', env') = smallStep e1 env
    in (App (App (Id "cons") e1') e2, env')


smallStep (App (FunList vars body) arg) env
  | isValue arg = (AppList (FunList vars body) [arg], env)
  | otherwise =
      let (arg', env') = smallStep arg env
      in (App (FunList vars body) arg', env')

smallStep (AppList f args) env
  | not (isValue f) =
      let (f', env') = smallStep f env
      in (AppList f' args, env')

-- Si la función es valor pero hay argumentos no-valor: reducir primer argumento no-valor (izq->der)
smallStep (AppList f args) env
  | isValue f && any (not . isValue) args =
      let (before, a:after) = break (not . isValue) args
          (a', env') = smallStep a env
          args' = before ++ (a' : after)
      in (AppList f args', env')

-- Si f es valor y todos args son valores: aplicamos
smallStep (AppList f args) env
  | isValue f && all isValue args =
      case f of
        FunList vars body ->
          let nVars = length vars
              nArgs = length args
              appliedVals = map (\a -> toValueWithEnv a env) args
          in if nArgs == nVars
             then let newEnv = zip vars appliedVals ++ env
                  in (body, newEnv)
             else if nArgs < nVars
                  then let (applied, restVars) = splitAt nArgs vars
                           newEnv = zip applied appliedVals ++ env
                       in (FunList restVars body, newEnv)
                  else error "Aplicación con más argumentos que parámetros (AppList FunList)"
        Fun x body ->
          -- función unaria literal con exactamente 1 argumento
          case args of
            [arg] -> let val = toValueWithEnv arg env
                         newEnv = (x, val) : env
                     in (body, newEnv)
            _ -> error "Aplicación de Fun literal con número de argumentos distinto de 1"
        Id fName ->
          case lookupEnv fName env of
            Just (VClosure x body envDef) ->
                if length args /= 1 then error "Aridad incorrecta al aplicar closure unario (Id)"
                else let val = toValueWithEnv (head args) env
                         newEnv = (x, val) : envDef
                     in (body, newEnv)
            Just (VClosureN vars body envDef) ->
                let nVars = length vars
                    nArgs = length args
                    appliedVals = map (\a -> toValueWithEnv a env) args
                in if nArgs == nVars
                   then let newEnv = zip vars appliedVals ++ envDef
                        in (body, newEnv)
                   else if nArgs < nVars
                        then let (applied, restVars) = splitAt nArgs vars
                                 newEnv = zip applied appliedVals ++ envDef
                             in (FunList restVars body, newEnv)
                        else error "Aplicación con más argumentos que parámetros (Id closure)"
            Just v ->
                let fASA = fromValue v
                in (AppList fASA args, env)
            Nothing -> error ("Variable no encontrada (AppList Id ...): " ++ fName)
        _ -> error ("AppList: función no válida: " ++ show f)

smallStep (App (Id f) v2) env
  | isValue v2 =
      case lookupEnv f env of
        Just (VClosure x body envDef) ->
            let env' = (x, toValueWithEnv v2 env) : envDef
            in (body, env')
        Just (VClosureN vars body envDef) ->
            case vars of
              (v:vs) ->
                let newEnv = (v, toValueWithEnv v2 env) : envDef
                in if null vs
                   then (body, newEnv)          -- applied all, return body
                   else (FunList vs body, newEnv) -- partial application: return FunList and environment with partial binds
              [] -> error "Closure sin parámetros"
        Just val ->
            let fASA = fromValue val
            in (App fASA v2, env)
        Nothing -> error ("Variable no encontrada (en App (Id ...)): " ++ f)

-- Aplicación de función literal unaria (Fun)
smallStep (App (Fun x body) v2) env
  | isValue v2 =
      let env' = (x, toValueWithEnv v2 env) : env
      in (body, env')

smallStep (App (FunList vars body) v2) env
  | isValue v2 = (AppList (FunList vars body) [v2], env)
  | otherwise = let (v2', env') = smallStep v2 env in (App (FunList vars body) v2', env')

-- Aplicación normal: evaluar argumento o función
smallStep (App v1 e2) env
    | isValue v1 =
        let (e2', env') = smallStep e2 env
        in (App v1 e2', env')
smallStep (App e1 e2) env =
    let (e1', env') = smallStep e1 env
    in (App e1' e2, env')

-- fallback
smallStep e env = error ("No implementado: " ++ show e)

lookupEnv :: String -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y, v):ys)
    | x == y    = Just v
    | otherwise = lookupEnv x ys

-- Eval (loop hasta valor)
eval :: ASA -> ASA
eval e = fst (loop e [])
    where 
        loop expresion env
            | isValue expresion = (expresion, env)
            | otherwise   = let (e', env') = smallStep expresion env
                            in loop e' env'

-- ¿qué es valor ahora?
isValue :: ASA -> Bool
isValue (Num _ ) = True
isValue (Boolean _) = True
isValue (Fun _ _) = True
isValue (FunList _ _) = True
isValue (Pair v1 v2) = isValue v1 && isValue v2
isValue (Id "nil") = True
isValue _ = False

-- conversiones ASA <-> Value (para guardar en entorno)
toValueWithEnv :: ASA -> Env -> Value
toValueWithEnv (Num n) _       = VNum n
toValueWithEnv (Boolean b) _   = VBool b
toValueWithEnv (Pair a b) env  = VPair (toValueWithEnv a env) (toValueWithEnv b env)
toValueWithEnv (Fun x body) env = VClosure x body env
toValueWithEnv (FunList vars body) env = VClosureN vars body env
toValueWithEnv _ _             = error "toValueWithEnv: no convertible"

toValue :: ASA -> Value
toValue a = toValueWithEnv a []

fromValue :: Value -> ASA
fromValue (VNum n) = Num n
fromValue (VBool b) = Boolean b
fromValue (VPair v1 v2) = Pair (fromValue v1) (fromValue v2)
fromValue (VClosure x body _) = Fun x body
fromValue (VClosureN vars body _) = FunList vars body

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
