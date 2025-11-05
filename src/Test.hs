module Main where

import Test.HUnit
import Lex (lexer)
import Grammars (parse)
import Interp (interp, Env)
import Desugar (desugar, ASA(..))


-- Combinador Z como string
combinadorZ :: String
combinadorZ =
  "(lambda (f)                                   \
  \  ((lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))             \
  \   (lambda (x)                                \
  \      (f (lambda (v) ((x x) v))))))"

-- Valor de Z evaluado
z :: ASA
z = interp (desugar (parse (lexer combinadorZ))) []

-- Entorno inicial
prelude :: Env
prelude = [("Z", z), ("Y", z)]

-- Helper para mostrar todo el proceso
makeTest :: String -> ASA -> String -> Test
makeTest input expected label =
  TestCase $ do
    putStrLn $ "\n----------------------------------------------------------------------------\nPrueba de " ++ label
    putStrLn $ "Entrada original:\n" ++ input

    let tokens = lexer input
    putStrLn $ "\nTokens generados por el lexer:"
    print tokens

    let astSASA = parse tokens
    putStrLn $ "\nAST generado por el parse (SASA):"
    print astSASA

    let astASA = desugar astSASA
    putStrLn $ "\nAST desazucarizado (ASA):"
    print astASA

    let result = interp astASA prelude
    putStrLn $ "\nResultado esperado:"
    print expected

    putStrLn $ "Resultado obtenido:"
    print result

    assertEqual "Resultado incorrecto" expected result

    putStrLn "--------------------------------------------------------------------------------------"


-- Pruebas
testSumaBinaria :: Test
testSumaBinaria             = makeTest "(+ 2 3)"       (Num 5)  "suma binaria"
testSumaVariadica :: Test
testSumaVariadica           = makeTest "(+ 1 2 3 4)"   (Num 10) "suma variádica"

testRestaBinaria :: Test
testRestaBinaria            = makeTest "(- 10 4)"      (Num 6)  "resta binaria"
testRestaVariadica :: Test
testRestaVariadica          = makeTest "(- 20 5 3)"    (Num 12) "resta variádica"

testMultBinaria :: Test
testMultBinaria             = makeTest "(* 3 4)"       (Num 12) "multiplicación binaria"
testMultVariadica :: Test
testMultVariadica           = makeTest "(* 2 3 4 5)"   (Num 120) "multiplicación variádica"

testDivBinaria :: Test
testDivBinaria              = makeTest "(/ 20 5)"      (Num 4)  "división binaria"
testDivVariadica :: Test
testDivVariadica            = makeTest "(/ 100 2 5)"   (Num 10) "división variádica"

testSqrt :: Test
testSqrt                    = makeTest "(sqrt 9)" (Num 3) "raíz cuadrada de 9"

testExpt :: Test
testExpt                    = makeTest "(expt 2 3)" (Num 8) "2 elevado a la 3"
testExptVariadico :: Test
testExptVariadico           = makeTest "(expt 2 3 2)" (Num 512) "expt variádico como 2^(3^2)"

testNotTrue :: Test
testNotTrue                 = makeTest "(not #t)" (Boolean False) "negación de verdadero"
testNotFalse :: Test
testNotFalse                = makeTest "(not #f)" (Boolean True) "negación de falso"

testIgualBinario :: Test
testIgualBinario            = makeTest "(= 5 5)"        (Boolean True) "igualdad binaria"
testIgualVariadico :: Test
testIgualVariadico          = makeTest "(= 2 2 2 2)"    (Boolean True) "igualdad variádica"

testMayorBinario :: Test
testMayorBinario = makeTest "(> 7 3)" (Boolean True) "mayor que binaria"

testMayorVariadico :: Test
testMayorVariadico = makeTest "(> 9 5 2)" (Boolean True) "mayor que variádica"

testMenorBinario :: Test
testMenorBinario = makeTest "(< 3 7)" (Boolean True) "menor que binaria"

testMenorVariadico :: Test
testMenorVariadico = makeTest "(< 1 2 3 4)" (Boolean True) "menor que variádica"

testMayorIgualBin :: Test
testMayorIgualBin = makeTest "(>= 5 5)" (Boolean True) "mayor o igual binaria"

testMayorIgualVar :: Test
testMayorIgualVar = makeTest "(>= 9 9 8 7)" (Boolean True) "mayor o igual variádica"

testMenorIgualBin :: Test
testMenorIgualBin = makeTest "(<= 5 5)" (Boolean True) "menor o igual binaria"

testMenorIgualVar :: Test
testMenorIgualVar = makeTest "(<= 1 2 2 3)" (Boolean True) "menor o igual variádica"

testDistintoBinario :: Test
testDistintoBinario = makeTest "(!= 5 3)" (Boolean True) "diferente binaria"

testDistintoVariadico :: Test
testDistintoVariadico = makeTest "(!= 1 2 3 4)" (Boolean True) "diferente variádica"

testPairFst :: Test
testPairFst = makeTest "(fst (1, #t))" (Num 1) "proyección fst de par (1, #t)"

testPairSnd :: Test
testPairSnd = makeTest "(snd (3, 5))" (Num 5) "proyección snd de par (3, 5)"

testLetSimple :: Test
testLetSimple = makeTest "(let ((x 5)) (+ x 2))" (Num 7) "let con una asignación"

testLetVariadico :: Test
testLetVariadico = makeTest "(let ((x 2) (y 3)) (+ x y))" (Num 5) "let variádico sin dependencia"

testLetStarSimple :: Test
testLetStarSimple = makeTest "(let* ((x 4)) (* x 2))" (Num 8) "let* con una asignación"

testLetStarVariadico :: Test
testLetStarVariadico = makeTest "(let* ((x 3) (y (+ x 1))) (+ x y))" (Num 7) "let* variádico con dependencia"

testIfBoolean :: Test
testIfBoolean = makeTest "(if #t 42 0)" (Num 42) "if con condición verdadera"

testIfFalse :: Test
testIfFalse = makeTest "(if #f 42 0)" (Num 0) "if con condición falsa"

testListaVacia :: Test
testListaVacia = makeTest "nil" (interp (desugar (parse (lexer "nil"))) prelude) "lista vacía"

testListaNumeros :: Test
testListaNumeros = makeTest "(cons 1 (cons 2 (cons 3 (cons 4 nil))))"
  (interp (desugar (parse (lexer "(cons 1 (cons 2 (cons 3 (cons 4 nil))))"))) prelude)
  "lista de números"

testHeadLista :: Test
testHeadLista = makeTest "(head (cons 1 (cons 2 (cons 3 nil))))" (Num 1) "head de lista construida con cons"

testTailLista :: Test
testTailLista = makeTest "(tail (cons 3 (cons 5 (cons 6 nil))))"
  (interp (desugar (parse (lexer "(cons 5 (cons 6 nil) )"))) prelude)
  "tail de lista construida con cons"

testCondPrimeraVerdadera :: Test
testCondPrimeraVerdadera = makeTest "(cond [(= 0 0) 42] [(= 1 1) 99] [else 0])" (Num 42) "cond con primera cláusula verdadera"

testCondSegundaVerdadera :: Test
testCondSegundaVerdadera = makeTest "(cond [(= 1 2) 10] [(= 3 3) 20] [else 30])" (Num 20) "cond con segunda cláusula verdadera"

testCondElse :: Test
testCondElse = makeTest "(cond [(= 1 2) 10] [(> 2 5) 20] [else 99])" (Num 99) "cond sin coincidencias, ejecuta else"

testLambdaSimple :: Test
testLambdaSimple = makeTest "((lambda (x) (+ x 1)) 4)" (Num 5) "lambda con un parámetro"

testLambdaBinaria :: Test
testLambdaBinaria = makeTest "((lambda (x y) (* x y)) 3 4)" (Num 12) "lambda con dos parámetros"

testLambdaVariadica :: Test
testLambdaVariadica = makeTest "((lambda (x y z) (+ x y z)) 1 2 3)" (Num 6) "lambda con tres parámetros"

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
