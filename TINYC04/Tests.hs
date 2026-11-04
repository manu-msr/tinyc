module Tests where

import Grammars
import Interp
import Lex
import TinyC

data Test = Test String Bool

assertEqual :: Eq a => String -> a -> a -> Test
assertEqual label expected actual = Test label (expected == actual)

assertTrue :: String -> Bool -> Test
assertTrue = Test

passed :: Test -> Bool
passed (Test _ result) = result

renderTest :: Test -> String
renderTest (Test label True) = "OK: " ++ label
renderTest (Test label False) = "ERROR: " ++ label

stateFor :: String -> State
stateFor = stateOf . runEmpty

outputFor :: String -> Output
outputFor = outputOf . runEmpty

functionProgram :: String
functionProgram =
  concat
    [ "int x = 5; "
    , "void incrementar(int *p) { *p = (*p + 1); return; } "
    , "incrementar(&x); "
    , "int doble(int n) { int resultado = (n + n); return resultado; } "
    , "int y = doble(x); print(x); print(y);"
    ]

functionASA :: Command
functionASA =
  FunDecl
    "doble"
    "n"
    (Decl "r" (Add (Id "n") (Id "n")))
    (Id "r")

voidASA :: Command
voidASA =
  VoidDecl
    "incrementar"
    "p"
    (AssignDeref (Id "p") (Add (Deref (Id "p")) (Num 1)))

recursionProgram :: String
recursionProgram =
  concat
    [ "int factorial(int n) { "
    , "int r = 0; "
    , "if (n == 0) { r = 1; } "
    , "else { r = (n * factorial((n - 1))); } "
    , "return r; } "
    , "int resultado = factorial(5);"
    ]

effectProgram :: String
effectProgram =
  concat
    [ "int total = 0; "
    , "int acumular(int n) { "
    , "total = (total + n); int r = total; return r; } "
    , "int resultado = (acumular(1) + acumular(2));"
    ]

pointerReturnProgram :: String
pointerReturnProgram =
  concat
    [ "int *identidad(int *p) { int copia = *p; return p; } "
    , "int x = 4; int *q = identidad(&x); *q = 9;"
    ]

declarationEnvironmentProgram :: String
declarationEnvironmentProgram =
  concat
    [ "int x = 10; "
    , "int sumarX(int n) { int r = (x + n); return r; } "
    , "int x = 20; int y = sumarX(1);"
    ]

printInsideProgram :: String
printInsideProgram =
  concat
    [ "int siguiente(int n) { "
    , "print(n); int r = (n + 1); return r; } "
    , "int x = siguiente(3); print(x);"
    ]

pointerProgram :: String
pointerProgram =
  "int x = 5; int *p = &x; *p = 8; int y = *p;"

isFunctionStored :: String -> State -> Bool
isFunctionStored identifier (environment, store) =
  isFunctionValue
    (lookupStore (lookupEnvironment identifier environment) store)

isFunctionValue :: Value -> Bool
isFunctionValue (FunD _ _ _ _) = True
isFunctionValue (VoidFunD _ _ _) = True
isFunctionValue (DataV _) = False

tests :: [Test]
tests =
  [ assertEqual
      "lexer reconoce las nuevas palabras reservadas"
      [TokenVoid, TokenReturn, TokenPrint]
      (lexer "void return print")
  , assertEqual
      "ASA de una funcion con resultado"
      functionASA
      (compile "int doble(int n) { int r = (n + n); return r; }")
  , assertEqual
      "ASA de una funcion void"
      voidASA
      (compile
        "void incrementar(int *p) { *p = (*p + 1); return; }")
  , assertEqual
      "una llamada con resultado es una expresion"
      (Decl "x" (Call "doble" (Num 3)))
      (compile "int x = doble(3);")
  , assertEqual
      "una llamada void es un comando"
      (CallVoid "incrementar" (Addr "x"))
      (compile "incrementar(&x);")
  , assertEqual
      "print usa la misma forma abstracta que una llamada void"
      (CallVoid "print" (Id "x"))
      (compile "print(x);")
  , assertEqual
      "el programa completo modifica x"
      (NumV 6)
      (valueOf "x" (stateFor functionProgram))
  , assertEqual
      "el programa completo calcula y"
      (NumV 12)
      (valueOf "y" (stateFor functionProgram))
  , assertEqual
      "print conserva el orden de la salida"
      [6, 12]
      (outputFor functionProgram)
  , assertEqual
      "declarar una funcion no ejecuta su cuerpo"
      (NumV 0)
      (valueOf "x"
        (stateFor
          "int x = 0; void cambiar(int n) { x = n; return; }"))
  , assertTrue
      "la definicion se guarda en una localidad"
      (isFunctionStored "doble"
        (stateFor "int doble(int n) { int r = n; return r; }"))
  , assertEqual
      "una funcion puede llamarse recursivamente"
      (NumV 120)
      (valueOf "resultado" (stateFor recursionProgram))
  , assertEqual
      "los operandos propagan la memoria de izquierda a derecha"
      (NumV 4, NumV 3)
      ( valueOf "resultado" (stateFor effectProgram)
      , valueOf "total" (stateFor effectProgram)
      )
  , assertEqual
      "una funcion puede retornar una referencia"
      (NumV 9)
      (valueOf "x" (stateFor pointerReturnProgram))
  , assertEqual
      "el cuerpo usa el ambiente de declaracion"
      (NumV 20, NumV 11)
      ( valueOf "x" (stateFor declarationEnvironmentProgram)
      , valueOf "y" (stateFor declarationEnvironmentProgram)
      )
  , assertEqual
      "la salida producida dentro de una llamada se conserva"
      [3, 4]
      (outputFor printInsideProgram)
  , assertEqual
      "conserva las operaciones con apuntadores de v3"
      (NumV 8, NumV 8)
      ( valueOf "x" (stateFor pointerProgram)
      , valueOf "y" (stateFor pointerProgram)
      )
  , assertEqual
      "las definiciones no aparecen entre los datos actuales"
      [("x", NumV 1)]
      (currentValues
        (stateFor
          ( "int x = 1; "
              ++ "int identidad(int n) { int r = n; return r; }"
          )))
  ]

report :: String
report = unlines (map renderTest tests)

allPassed :: Bool
allPassed = all passed tests
