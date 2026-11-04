module TinyC where

import Data.List (intercalate)
import Grammars
import Interp
import Lex

compile :: String -> Program
compile = parse . lexer

runSource :: String -> Environment -> Store -> Execution
runSource source = interp (compile source)

runEmpty :: String -> Execution
runEmpty source = runSource source [] []

renderValue :: DataValue -> String
renderValue (NumV number) = show number
renderValue (RefV location) = "ref(" ++ show location ++ ")"

renderBinding :: (Identifier, DataValue) -> String
renderBinding (identifier, value) =
  identifier ++ " = " ++ renderValue value

renderValues :: [(Identifier, DataValue)] -> String
renderValues values =
  "[" ++ intercalate ", " (map renderBinding values) ++ "]"

renderOutput :: Output -> String
renderOutput output = "[" ++ intercalate ", " (map show output) ++ "]"

renderExecution :: Execution -> String
renderExecution execution =
  unlines
    [ "Ambiente: " ++ show environment
    , "Memoria: " ++ show store
    , "Valores actuales: " ++ renderValues (currentValues state)
    , "Salida: " ++ renderOutput (outputOf execution)
    ]
  where
    state@(environment, store) = stateOf execution

test :: String -> IO ()
test source = putStr (renderExecution (runEmpty source))

repl :: IO ()
repl = putStr "> " >> getLine >>= continue

continue :: String -> IO ()
continue "(exit)" = putStrLn "Bye."
continue source = test source >> repl

run :: IO ()
run =
  putStrLn "tinyC v4: funciones. Bienvenidx."
    >> repl

exampleFunctions :: String
exampleFunctions =
  concat
    [ "int x = 5; "
    , "void incrementar(int *p) { *p = (*p + 1); return; } "
    , "incrementar(&x); "
    , "int doble(int n) { int r = (n + n); return r; } "
    , "int y = doble(x); print(x); print(y);"
    ]

testFunctions :: IO ()
testFunctions = test exampleFunctions

exampleRecursion :: String
exampleRecursion =
  concat
    [ "int factorial(int n) { "
    , "int r = 0; "
    , "if (n == 0) { r = 1; } "
    , "else { r = (n * factorial((n - 1))); } "
    , "return r; } "
    , "int resultado = factorial(5); print(resultado);"
    ]

testRecursion :: IO ()
testRecursion = test exampleRecursion
