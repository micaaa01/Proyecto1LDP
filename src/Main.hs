-- Main.hs
module Main where

import Lexer (lexer, Token(..))
import Grammar (parser, ASA(..))
import Desugar (desugar, SASA(..))

-- Ejemplo de expresión en tu lenguaje
input :: String
input = "(let ((x 5) (y 10)) (+ x y))"

main :: IO ()
main = do
    putStrLn "Entrada original:"
    putStrLn input

    -- Paso 1: Lexer
    let tokens = lexer input
    putStrLn "\nTokens generados por el lexer:"
    print tokens

    -- Paso 2: Parser
    let ast = parser tokens
    putStrLn "\nAST generado por el parser (ASA):"
    print ast

    -- Paso 3: Desugar
    let sasaAst = desugar ast
    putStrLn "\nAST desazucarizado (SASA):"
    print sasaAst