module Main where

import Lex (lexer, Token(..))
import Grammars (parse)
import Interp (interp, Env)
import Desugar (ASA(..), desugar)
import Control.Exception (catch, SomeException)

-- Combinador Z (orden aplicativa / ansiosa)
combinadorZ :: String
combinadorZ =
  "(lambda (f)                                   \
  \  ((lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))             \
  \   (lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))))"

-- Valor de Z ya desazucarado y evaluado en []
z :: ASA
z =
  let sasa = parse (lexer combinadorZ)
      asa  = desugar sasa
  in interp asa []

-- Definición de map
defMap :: ASA
defMap = interp (desugar (parse (lexer
  "(lambda (f lst) (if (null? lst) nil (cons (f (head lst)) (map f (tail lst)))))"
  ))) prelude

-- Definición de filter
defFilter :: ASA
defFilter = interp (desugar (parse (lexer
  "(lambda (pred lst) (if (null? lst) nil (if (pred (head lst)) (cons (head lst) (filter pred (tail lst))) (filter pred (tail lst)))))"
  ))) prelude


-- Pretty-printer sencillo
saca :: ASA -> String
saca (Num n)        = show n
saca (Boolean True) = "#t"
saca (Boolean False)= "#f"
saca (Closure _ _ _) = "#<procedure>"
saca other          = show other

-- Ambiente inicial
prelude :: Env
prelude =
  [ ("Z", z)
  , ("Y", z)
  , ("map", defMap)
  , ("filter", defFilter)
  ]

-- Punto de entrada
main :: IO ()
main = do
    putStrLn "Bienvenidx al sistema de pruebas de Lambdae (ae), ¿deseas ingresar al sistema?"
    putStrLn "1. Sí"
    putStrLn "2. No"
    opcion <- getLine
    case opcion of
        "1" -> menuPrincipal
        "2" -> putStrLn "Gracias por visitar Lambdae (ae). Hasta luego."
        _   -> putStrLn "Opción inválida. Intenta de nuevo." >> main

--Menu principal
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\nPerfecto, ¿qué quieres hacer?"
    putStrLn "0. Libre expresión (sin restricciones)"
    putStrLn "1. Operadores Aritméticos"
    putStrLn "2. Operadores Lógicos"
    putStrLn "3. Pares ordenados y proyecciones"
    putStrLn "4. Asignaciones locales"
    putStrLn "5. Condicional booleano"
    putStrLn "6. Listas y operaciones básicas"
    putStrLn "7. Condicional por clausulado"
    putStrLn "8. Funciones anónimas y lambdas"
    putStrLn "9. Funciones Extras"
    putStrLn "10. Regresar al menú anterior"
    putStrLn "Elige una opción:"
    opcion <- getLine
    case opcion of
        "0"  -> modoLibre
        "1"  -> submenu "Operadores Aritméticos"
        "2"  -> submenu "Operadores Lógicos"
        "3"  -> submenu "Pares ordenados y proyecciones"
        "4"  -> submenu "Asignaciones locales"
        "5"  -> submenu "Condicional booleano"
        "6"  -> submenu "Listas y operaciones básicas"
        "7"  -> submenu "Condicional por clausulado"
        "8"  -> submenu "Funciones anónimas y lambdas"
        "9"  -> menuFuncionesExtras
        "10" -> main
        _    -> putStrLn "Opción inválida. Intenta de nuevo." >> menuPrincipal

--Submenu
submenu :: String -> IO ()
submenu categoria = do
    putStrLn ("\nHas ingresado al apartado: " ++ categoria)
    putStrLn "Escribe una expresión MINILISP relacionada con esta categoría:"
    input <- getLine
    if categoriaValida categoria input
        then do
            ejecutar input
            volverAlMenu categoria
        else do
            putStrLn "\nLa expresión no parece pertenecer a esta categoría."
            putStrLn "Por favor, intenta con una expresión relacionada."
            volverAlMenu categoria

--Modo Libre
modoLibre :: IO ()
modoLibre = do
    putStrLn "\nHas ingresado al modo de libre expresión."
    putStrLn "Escribe cualquier expresión MINILISP:"
    input <- getLine
    ejecutar input
    volverLibre

--Regresar al Menu
volverAlMenu :: String -> IO ()
volverAlMenu categoria = do
    putStrLn "\n¿Deseas continuar en este apartado o regresar al menú principal?"
    putStrLn "1. Continuar en este apartado"
    putStrLn "2. Regresar al menú principal"
    opcion <- getLine
    case opcion of
        "1" -> submenu categoria
        "2" -> menuPrincipal
        _   -> putStrLn "Opción inválida. Regresando al menú principal..." >> menuPrincipal

--Volver del modo libre
volverLibre :: IO ()
volverLibre = do
    putStrLn "\n¿Deseas continuar en modo libre o regresar al menú principal?"
    putStrLn "1. Continuar en modo libre"
    putStrLn "2. Regresar al menú principal"
    opcion <- getLine
    case opcion of
        "1" -> modoLibre
        "2" -> menuPrincipal
        _   -> putStrLn "Opción inválida. Regresando al menú principal..." >> menuPrincipal

--Menu de funciones extras "9"
menuFuncionesExtras :: IO ()
menuFuncionesExtras = do
    putStrLn "\nHas ingresado al apartado de Funciones Extras:"
    putStrLn "1. Suma de los primeros n números naturales"
    putStrLn "2. Factorial"
    putStrLn "3. Fibonacci"
    putStrLn "4. Función map para listas"
    putStrLn "5. Función filter para listas"
    putStrLn "6. Regresar al menú principal"
    putStrLn "Elige una opción:"
    opcion <- getLine
    case opcion of
        "1" -> pedirNumero "sumN" "(letrec (sumN (lambda (n) (if (= n 0) 0 (+ n (sumN (- n 1)))))) (sumN "
        "2" -> pedirNumero "fact" "(letrec (fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact "
        "3" -> pedirNumero "fib" "(letrec (fib (lambda (n) (if (<= n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib "
        "4" -> menuMap
        "5" -> menuFilter
        "6" -> menuPrincipal
        _   -> putStrLn "Opción inválida. Intenta de nuevo." >> menuFuncionesExtras

--"Pedir un numero"
pedirNumero :: String -> String -> IO ()
pedirNumero nombre inicio = do
    putStrLn ("Ingresa un número para " ++ nombre ++ ":")
    nStr <- getLine
    case reads nStr :: [(Int, String)] of
        [(n, "")] -> do
            let expr = inicio ++ show n ++ "))"
            ejecutar expr
            menuFuncionesExtras
        _ -> do
            putStrLn "Entrada inválida. Por favor ingresa un número entero."
            menuFuncionesExtras

--ejecución
ejecutar :: String -> IO ()
ejecutar input = catch (do
    putStrLn ("\nEntrada original:\n" ++ input)

    let tokens = lexer input
    putStrLn "\nTokens generados por el lexer:"
    print tokens

    let ast = parse tokens
    putStrLn "\nAST generado por el parser (SASA):"
    print ast

    let coreAst = desugar ast
    putStrLn "\nAST desazucarizado (ASA):"
    print coreAst

    let result = interp coreAst prelude
    putStrLn "\nResultado de la evaluación:"
    putStrLn (saca result)

    ) handler

handler :: SomeException -> IO ()
handler e = putStrLn $ "\nError durante la ejecución: " ++ show e

-- Menú para map
menuMap :: IO ()
menuMap = do
  putStrLn "Has ingresado a la función map.\n"
  putStrLn "Ejemplos de funciones que puedes aplicar:"
  putStrLn "1. Doblar números: (lambda (x) (* x 2))"
  putStrLn "   Lista sugerida: (cons 1 (cons 2 (cons 3 nil)))"
  putStrLn "2. Negar booleanos: (lambda (x) (not x))"
  putStrLn "   Lista sugerida: (cons #t (cons #f (cons #t nil)))"
  putStrLn "3. Obtener primer elemento de pares: (lambda (x) (fst x))"
  putStrLn "   Lista sugerida: (cons (pair 1 2) (cons (pair 3 4) nil))"
  putStrLn "\nPuedes escribir cualquier función y lista MINILISP."
  putStrLn "Primero ingresa la función:"
  func <- getLine
  putStrLn "Ahora ingresa la lista:"
  lista <- getLine
  let expr = "(map " ++ func ++ " " ++ lista ++ ")"
  ejecutar expr
  menuFuncionesExtras

-- Menú para filter
menuFilter :: IO ()
menuFilter = do
  putStrLn "Has ingresado a la función filter.\n"
  putStrLn "Ejemplos de condiciones que puedes usar:"
  putStrLn "1. Mayor que 2: (lambda (x) (> x 2))"
  putStrLn "   Lista sugerida: (cons 1 (cons 2 (cons 3 nil)))"
  putStrLn "2. Es verdadero: (lambda (x) x)"
  putStrLn "   Lista sugerida: (cons #t (cons #f (cons #t nil)))"
  putStrLn "3. Primer elemento igual a 1: (lambda (x) (= (fst x) 1))"
  putStrLn "   Lista sugerida: (cons (pair 1 2) (cons (pair 3 4) nil))"
  putStrLn "\nPuedes escribir cualquier predicado y lista MINILISP."
  putStrLn "Primero ingresa el predicado:"
  pred <- getLine
  putStrLn "Ahora ingresa la lista:"
  lista <- getLine
  let expr = "(filter " ++ pred ++ " " ++ lista ++ ")"
  ejecutar expr
  menuFuncionesExtras

-- Validación de categoría
categoriaValida :: String -> String -> Bool
categoriaValida categoria input =
    let tokens = lexer input
        has t = t `elem` tokens
    in case categoria of
        "Operadores Aritméticos" ->
            any has [TokenSuma, TokenResta, TokenMult, TokenDiv, TokenAdd1, TokenSub1, TokenSqrt, TokenExpt]
        "Operadores Lógicos" ->
            any has [TokenEq, TokenDistinto, TokenMenor, TokenMayor, TokenMenorIgual, TokenMayorIgual, TokenNot]
        "Pares ordenados y proyecciones" ->
            any has [TokenFst, TokenSnd, TokenPair]
        "Asignaciones locales" ->
            any has [TokenLet, TokenLetStar]
        "Condicional booleano" ->
            has TokenIf
        "Listas y operaciones básicas" ->
            any has [TokenHead, TokenTail, TokenCA, TokenCC]
        "Condicional por clausulado" ->
            has TokenCond
        "Funciones anónimas y lambdas" ->
            has TokenLambda
        "Funciones Extras" ->
            any has [TokenVar "factorial", TokenVar "fibonacci", TokenVar "sum", TokenVar "map", TokenVar "filter"]
        _ -> False
