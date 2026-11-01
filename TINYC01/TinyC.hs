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
  putStrLn "tinyC v1: programas secuenciales. Bienvenidx."
  repl

testConductor :: IO ()
testConductor =
  test
    "int x = 2; int y = (x - 2); y = (x + 3); x = (y * 2);"
