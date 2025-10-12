import Lexer
import Grammar


main :: IO ()
main = do
    putStrLn "Ingresa una expresió: "
    input <- getLine
    print (parser (lexer input))