module Main where

import Test.HUnit
import Lexer (lexer, Token(..))
import Grammar (parser, SASA(..))
import Desugar (desugar, ASA(..))
import Eval (eval)

-- Helper para mostrar todo el proceso
makeTest :: String -> ASA -> String -> Test
makeTest input expected label =
  TestCase $ do
    putStrLn $ "\n----------------------------------------------------------------------------\nPrueba de " ++ label
    putStrLn $ "Entrada original:\n" ++ input

    let tokens = lexer input
    putStrLn $ "\nTokens generados por el lexer:"
    print tokens

    let astSASA = parser tokens
    putStrLn $ "\nAST generado por el parser (SASA):"
    print astSASA

    let astASA = desugar astSASA
    putStrLn $ "\nAST desazucarizado (ASA):"
    print astASA

    let result = eval astASA
    putStrLn $ "\nResultado esperado:"
    print expected

    putStrLn $ "Resultado obtenido:"
    print result

    assertEqual "Resultado incorrecto" expected result

    putStrLn "--------------------------------------------------------------------------------------"

-- Pruebas
testSumaBinaria             = makeTest "(+ 2 3)"       (Num 5)  "suma binaria"
testSumaVariadica           = makeTest "(+ 1 2 3 4)"   (Num 10) "suma variádica"

testRestaBinaria            = makeTest "(- 10 4)"      (Num 6)  "resta binaria"
testRestaVariadica          = makeTest "(- 20 5 3)"    (Num 12) "resta variádica"

testMultBinaria             = makeTest "(* 3 4)"       (Num 12) "multiplicación binaria"
testMultVariadica           = makeTest "(* 2 3 4 5)"   (Num 120) "multiplicación variádica"

testDivBinaria              = makeTest "(/ 20 5)"      (Num 4)  "división binaria"
testDivVariadica            = makeTest "(/ 100 2 5)"   (Num 10) "división variádica"

testSqrt                    = makeTest "(sqrt 9)" (Num 3) "raíz cuadrada de 9"

testExpt                    = makeTest "(expt 2 3)" (Num 8) "2 elevado a la 3"
testExptVariadico           = makeTest "(expt 2 3 2)" (Num 512) "expt variádico como 2^(3^2)"

testNotTrue                 = makeTest "(not #t)" (Boolean False) "negación de verdadero"
testNotFalse                = makeTest "(not #f)" (Boolean True) "negación de falso"

testIgualBinario            = makeTest "(= 5 5)"        (Boolean True) "igualdad binaria"
testIgualVariadico          = makeTest "(= 2 2 2 2)"    (Boolean True) "igualdad variádica"

testMayorBinario            = makeTest "(> 7 3)"        (Boolean True) "mayor que binaria"
testMayorVariadico          = makeTest "(> 9 5 2)"      (Boolean True) "mayor que variádica"

testMenorBinario            = makeTest "(< 3 7)"        (Boolean True) "menor que binaria"
testMenorVariadico          = makeTest "(< 1 2 3 4)"    (Boolean True) "menor que variádica"

testMayorIgualBin           = makeTest "(>= 5 5)"       (Boolean True) "mayor o igual binaria"
testMayorIgualVar           = makeTest "(>= 9 9 8 7)"   (Boolean True) "mayor o igual variádica"

testMenorIgualBin           = makeTest "(<= 5 5)"       (Boolean True) "menor o igual binaria"
testMenorIgualVar           = makeTest "(<= 1 2 2 3)"   (Boolean True) "menor o igual variádica"

testDistintoBinario         = makeTest "(!= 5 3)"       (Boolean True) "diferente binaria"
testDistintoVariadico       = makeTest "(!= 1 2 3 4)"   (Boolean True) "diferente variádica"

testPairFst                 = makeTest "(fst (1, #t))" (Num 1) "proyección fst de par (1, #t)"
testPairSnd                 = makeTest "(snd (3, 5))"  (Num 5) "proyección snd de par (3, 5)"

testLetSimple               = makeTest "(let ((x 5)) (+ x 2))" (Num 7) "let con una asignación"
testLetVariadico            = makeTest "(let ((x 2) (y 3)) (+ x y))" (Num 5) "let variádico sin dependencia"

testLetStarSimple           = makeTest "(let* ((x 4)) (* x 2))" (Num 8) "let* con una asignación"
testLetStarVariadico        = makeTest "(let* ((x 3) (y (+ x 1))) (+ x y))" (Num 7) "let* variádico con dependencia"

testIfBoolean               = makeTest "(if #t 42 0)" (Num 42) "if con condición verdadera"
testIfFalse                 = makeTest "(if #f 42 0)" (Num 0) "if con condición falsa"

testListaVacia              = makeTest "nil" (eval (desugar (parser (lexer "nil")))) "lista vacía"
testListaNumeros            = makeTest "(cons 1 (cons 2 (cons 3 (cons 4 nil))))" (eval (desugar (parser (lexer "(cons 1 (cons 2 (cons 3 (cons 4 nil))))")))) "lista de números"
testHeadLista               = makeTest "(head (cons 1 (cons 2 (cons 3 nil))))" (Num 1) "head de lista construida con cons"
testTailLista               = makeTest "(tail (cons 3 (cons 5 (cons 6 nil))))" (eval (desugar (parser (lexer "(cons 5 (cons 6 nil) )")))) "tail de lista construida con cons"

testCondPrimeraVerdadera    = makeTest "(cond [(= 0 0) 42] [(= 1 1) 99] [else 0])" (Num 42) "cond con primera cláusula verdadera"
testCondSegundaVerdadera    = makeTest "(cond [(= 1 2) 10] [(= 3 3) 20] [else 30])" (Num 20) "cond con segunda cláusula verdadera"
testCondElse                = makeTest "(cond [(= 1 2) 10] [(> 2 5) 20] [else 99])" (Num 99) "cond sin coincidencias, ejecuta else"

testLambdaSimple            = makeTest "((lambda (x) (+ x 1)) 4)" (Num 5) "lambda con un parámetro"
testLambdaBinaria           = makeTest "((lambda (x y) (* x y)) 3 4)" (Num 12) "lambda con dos parámetros"
testLambdaVariadica         = makeTest "((lambda (x y z) (+ x y z)) 1 2 3)" (Num 6) "lambda con tres parámetros"


-- Agrupar
tests :: Test
tests = TestList
  [ testSumaBinaria, testSumaVariadica
  , testRestaBinaria, testRestaVariadica
  , testMultBinaria, testMultVariadica
  , testDivBinaria, testDivVariadica
  , testSqrt, testExpt, testExptVariadico
  , testNotTrue, testNotFalse
  , testIgualBinario, testIgualVariadico
  , testMayorBinario, testMayorVariadico
  , testMenorBinario, testMenorVariadico
  , testMayorIgualBin, testMayorIgualVar
  , testMenorIgualBin, testMenorIgualVar
  , testDistintoBinario, testDistintoVariadico
  , testPairFst, testPairSnd
  , testLetSimple, testLetVariadico
  , testLetStarSimple, testLetStarVariadico
  , testIfBoolean, testIfFalse
  , testListaVacia, testListaNumeros
  , testHeadLista, testTailLista
  , testCondPrimeraVerdadera, testCondSegundaVerdadera, testCondElse
  , testLambdaSimple, testLambdaBinaria, testLambdaVariadica

  ]


main :: IO ()
main = runTestTT tests >>= print
