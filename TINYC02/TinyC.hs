module TinyC where

import Grammars
import Interp
import Lex

compile :: String -> Program
compile = parse . lexer

runSource :: String -> Environment -> Store -> State
runSource source = interp (compile source)

runEmpty :: String -> State
runEmpty source = runSource source [] []

renderState :: State -> String
renderState state@(environment, store) =
  unlines
    [ "Ambiente: " ++ show environment
    , "Memoria: " ++ show store
    , "Valores actuales: " ++ show (currentValues state)
    ]

test :: String -> IO ()
test source = putStr (renderState (runEmpty source))

repl :: IO ()
repl = do
  putStr "> "
  source <- getLine
  if source == "(exit)"
    then putStrLn "Bye."
    else do
      test source
      repl

run :: IO ()
run = do
  putStrLn "tinyC v2: estructuras de control. Bienvenidx."
  repl

testConductor :: IO ()
testConductor =
  test
    ( "int x = 0; int y = 0; "
        ++ "while (x <= 2) { "
        ++ "if (x == 1) { y = (y + 2); } "
        ++ "else { y = (y + 1); } "
        ++ "x = (x + 1); }"
    )

testNot :: IO ()
testNot =
  test
    ( "int x = 3; int y = 0; "
        ++ "while (! (x == 0)) { "
        ++ "y = (y + x); x = (x - 1); }"
    )
