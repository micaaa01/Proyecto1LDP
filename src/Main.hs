module Main where

import Lexer (lexer, Token(..))
import Grammar (parser)
import Desugar (desugar)
import Eval (eval)
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
    putStrLn "Bienvenido al sistema de pruebas de Lambda-E (ae), ¿deseas ingresar al sistema?"
    putStrLn "1. Sí"
    putStrLn "2. No"
    opcion <- getLine
    case opcion of
        "1" -> menuPrincipal
        "2" -> putStrLn "Gracias por visitar Lambda-E. Hasta luego."
        _   -> putStrLn "Opción inválida. Intenta de nuevo." >> main

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
        "9"  -> submenu "Funciones Extrasssss"
        "10" -> main
        _    -> putStrLn "Opción inválida. Intenta de nuevo." >> menuPrincipal

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

modoLibre :: IO ()
modoLibre = do
    putStrLn "\nHas ingresado al modo de libre expresión."
    putStrLn "Escribe cualquier expresión MINILISP:"
    input <- getLine
    ejecutar input
    volverLibre

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

ejecutar :: String -> IO ()
ejecutar input = catch (do
    putStrLn ("\nEntrada original:\n" ++ input)

    let tokens = lexer input
    putStrLn "\nTokens generados por el lexer:"
    print tokens

    let ast = parser tokens
    putStrLn "\nAST generado por el parser (SASA):"
    print ast

    let coreAst = desugar ast
    putStrLn "\nAST desazucarizado (ASA):"
    print coreAst

    let result = eval coreAst
    putStrLn "\nResultado de la evaluación:"
    print result

    ) handler

handler :: SomeException -> IO ()
handler e = putStrLn $ "\nError durante la ejecución: " ++ show e

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
        _ -> True
