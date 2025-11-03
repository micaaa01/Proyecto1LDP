module Eval where
import Desugar (ASA (..), Op(..))

data Value
    = VNum Int
    | VBool Bool
    | VClosure String ASA Env
    | VPair Value Value
    deriving (Show, Eq)

type Env = [(String, Value)]

smallStep :: ASA -> Env -> (ASA, Env) 
smallStep (Id "nil") env = (Id "nil", env)
smallStep (Id i) env = case lookupEnv i env of
        Just v  -> (fromValue v, env)
        Nothing -> error ("Variable no encontrada: " ++ i)

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
smallStep (If (Boolean True) eThen eElse) env =
    (eThen, env)
smallStep (If (Boolean False) eThen eElse) env =
    (eElse, env)
smallStep (If cond eThen eElse) env =
    let (cond', env') = smallStep cond env
    in (If cond' eThen eElse, env')

smallStep (Fun x body) env = (Fun x body, env)

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

smallStep (App (Fun x body) v2) env
    | isValue v2 =
        let closureEnv = (x, toValue v2) : env
        in (body, closureEnv)
smallStep (App v1 e2) env
    | isValue v1 =
        let (e2', env') = smallStep e2 env
        in (App v1 e2', env')
smallStep (App e1 e2) env =
    let (e1', env') = smallStep e1 env
    in (App e1' e2, env')

smallStep e env = error ("No implementado: " ++ show e)


lookupEnv :: String -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y, v):ys)
    | x == y    = Just v
    | otherwise = lookupEnv x ys


eval :: ASA -> ASA
eval e = fst (loop e [])
    where 
        loop expresion env
            | isValue expresion = (expresion, env)
            | otherwise   = let (e', env') = smallStep expresion env
                            in loop e' env'

isValue :: ASA -> Bool
isValue (Num _ ) = True
isValue (Boolean _) = True
isValue (Fun _ _) = True
isValue (Pair v1 v2) = isValue v1 && isValue v2
isValue (Id "nil") = True
isValue _ = False

toValue :: ASA -> Value
toValue (Num n) = VNum n
toValue (Boolean b) = VBool b
toValue (Pair v1 v2) = VPair (toValue v1) (toValue v2)
toValue (Fun x body) = VClosure x body []
toValue _ = error "No se puede convertir ASA a Value"

fromValue :: Value -> ASA
fromValue (VNum n) = Num n
fromValue (VBool b) = Boolean b
fromValue (VPair v1 v2) = Pair (fromValue v1) (fromValue v2)
fromValue (VClosure x body _) = Fun x body

evalBinOp :: Op -> ASA -> ASA -> ASA
-- Operaciones aritmeticas
evalBinOp AddOp (Num x) (Num y) = Num (x + y)
evalBinOp SubOp (Num x) (Num y) = Num (x - y)
evalBinOp MulOp (Num x) (Num y) = Num (x * y)
evalBinOp DivOp (Num x) (Num y) 
    | y==0    = error "Division entre cero"
    | otherwise = Num (x `div`y)
evalBinOp ExptOp (Num x) (Num y) = Num (x ^ y)
-- Comparaciones
evalBinOp EqOp (Num n1) (Num n2) = Boolean (n1 == n2)
evalBinOp EqOp (Boolean b1) (Boolean b2) = Boolean (b1 == b2)
evalBinOp EqOp (Pair a1 b1) (Pair a2 b2) =
  Boolean (isTrue (evalBinOp EqOp a1 a2) && isTrue (evalBinOp EqOp b1 b2))
evalBinOp NeOp  x y = Boolean (x /= y)
evalBinOp LtOp  (Num x) (Num y) = Boolean (x < y)
evalBinOp LeOp  (Num x) (Num y) = Boolean (x <= y)
evalBinOp GtOp  (Num x) (Num y) = Boolean (x > y)
evalBinOp GeOp  (Num x) (Num y) = Boolean (x >= y)
-- Operaciones logicos
evalBinOp AndOp (Boolean x) (Boolean y) = Boolean (x && y)
evalBinOp AndOp (Boolean x) e
    |not x = Boolean False
    | otherwise = e
evalBinOp _ _ _ = error "Operación binaria inválida"

isTrue :: ASA -> Bool
isTrue (Boolean True) = True
isTrue _ = False