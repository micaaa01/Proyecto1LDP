module Eval where
import Desugar (ASA (..))

type Env = [(String, ASA)]

smallStep :: ASA -> Env -> (ASA, Env) 
smallStep (Id i) env = (lookupEnv i env, env)
smallStep (Num n) env = (Num n, env)
smallStep (Boolean b) env = (Boolean b, env)
smallStep (Add (Num n) (Num m)) env = (Num (n+m), env)
smallStep (Add (Num n) d) env =  
    let (d', env') = smallStep d env
    in (Add (Num n) d', env')
smallStep (Add i d) env =
    let (i', env') = smallStep i env
    in (Add i' d, env)
smallStep (Sub (Num n) (Num m)) env = (Num (n-m), env)
smallStep (Sub (Num n) d) env =  
    let (d', env') = smallStep d env
    in (Sub (Num n) d', env')
smallStep (Sub i d) env =
    let (i', env') = smallStep i env
    in (Sub i' d, env)
smallStep (Mul (Num n) (Num m)) env = (Num (n*m), env)
smallStep (Mul (Num n) d) env =  
    let (d', env') = smallStep d env
    in (Mul (Num n) d', env')
smallStep (Mul i d) env =
    let (i', env') = smallStep i env
    in (Mul i' d, env)
smallStep (Div (Num n) (Num m)) env = (Num (n `div` m), env)
smallStep (Div (Num n) d) env =  
    let (d', env') = smallStep d env
    in (Div (Num n) d', env')
smallStep (Div i d) env =
    let (i', env') = smallStep i env
    in (Div i' d, env)
smallStep (Sqrt (Num n)) env =
    let r = sqrt (fromIntegral n :: Double)
    in (Num (floor r), env)
smallStep (Sqrt e) env =
    let (e', env') = smallStep e env
    in (Sqrt e', env')


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
isValue _ = False