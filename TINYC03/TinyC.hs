module TinyC where

import Data.List (intercalate)
import Grammars
import Interp
import Lex

compile :: String -> Program
compile = parse . lexer

runSource :: String -> Environment -> Store -> State
runSource source = interp (compile source)

runEmpty :: String -> State
runEmpty source = runSource source [] []

renderValue :: Value -> String
renderValue (IntV number) = show number
renderValue (PtrV location) = "ref(" ++ show location ++ ")"

renderBinding :: (Identifier, Value) -> String
renderBinding (identifier, value) =
  identifier ++ " = " ++ renderValue value

renderValues :: [(Identifier, Value)] -> String
renderValues values = "[" ++ intercalate ", " (map renderBinding values) ++ "]"

renderState :: State -> String
renderState state@(environment, store) =
  unlines
    [ "Ambiente: " ++ show environment
    , "Memoria: " ++ show store
    , "Valores actuales: " ++ renderValues (currentValues state)
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
  putStrLn "tinyC v3: variables y referencias. Bienvenidx."
  repl

testPointer :: IO ()
testPointer =
  test "int x = 5; int *p = &x; *p = 8;"

testRedirect :: IO ()
testRedirect =
  test
    ( "int x = 1; int y = 2; int *p = &x; "
        ++ "p = &y; *p = 10;"
    )
