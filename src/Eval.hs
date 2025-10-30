module Eval where
import Desugar (ASA (..), Op(..))

type Env = [(String, ASA)]

smallStep :: ASA -> Env -> (ASA, Env) 
smallStep (Id i) env = (lookupEnv i env, env)
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


lookupEnv :: String -> Env -> ASA
lookupEnv x [] = error ("Variable no encontrada: " ++ x)
lookupEnv x ((y, v):ys)
    |x == y      =  v
    |otherwise   = lookupEnv x ys

eval :: ASA -> ASA
eval e = fst (loop e [])
    where 
        loop exp env
            | isValue exp = (exp, env)
            | otherwise   = let (e', env') = smallStep exp env
                            in loop e' env'

isValue :: ASA -> Bool
isValue (Num _ ) = True
isValue (Boolean _) = True
isValue (Fun _ _) = True
isValue (Pair v1 v2) = isValue v1 && isValue v2
isValue _ = False

evalBinOp :: Op -> ASA -> ASA -> ASA
evalBinOp AddOp (Num x) (Num y) = Num (x + y)
evalBinOp SubOp (Num x) (Num y) = Num (x - y)
evalBinOp MulOp (Num x) (Num y) = Num (x * y)
evalBinOp DivOp (Num x) (Num y) = Num (x `div` y)
evalBinOp ExptOp (Num x) (Num y) = Num (x ^ y)
evalBinOp EqOp  x y = Boolean (x == y)
evalBinOp NeOp  x y = Boolean (x /= y)
evalBinOp LtOp  (Num x) (Num y) = Boolean (x < y)
evalBinOp LeOp  (Num x) (Num y) = Boolean (x <= y)
evalBinOp GtOp  (Num x) (Num y) = Boolean (x > y)
evalBinOp GeOp  (Num x) (Num y) = Boolean (x >= y)
evalBinOp _ _ _ = error "Operación binaria inválida"
