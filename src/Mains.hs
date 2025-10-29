-- Main.hs
module Main where

import Lexer (lexer, Token(..))
import Grammar (parser, SASA(..))
import Desugar (desugar, ASA(..))
import Control.Exception (catch, SomeException)

-- Ejemplo de expresión en tu lenguaje
-- input :: String
-- input = "(let ((x 5) (y 10)) (+ x y))"

main :: IO ()
main = do
    putStrLn "Ingresa una expresión MINILISP:"
    input <- getLine
    putStrLn ("\nEntrada original:\n" ++ input)

    catch (do
        -- Paso 1: Lexer
        let tokens = lexer input
        putStrLn "\nTokens generados por el lexer:"
        print tokens

        -- Paso 2: Parser
        let ast = parser tokens
        putStrLn "\nAST generado por el parser (SASA):"
        print ast

        -- Paso 3: Desugar
        let sasaAst = desugar ast
        putStrLn "\nAST desazucarizado (ASA):"
        print sasaAst
      )
      handler

handler :: SomeException -> IO ()
handler e = putStrLn $ "\n Error durante la ejecución: " ++ show e