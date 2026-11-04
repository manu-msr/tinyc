module Tests where

import Grammars
import Interp
import TinyC

data Test = Test String Bool

assertEqual :: Eq a => String -> a -> a -> Test
assertEqual label expected actual = Test label (expected == actual)

passed :: Test -> Bool
passed (Test _ result) = result

renderTest :: Test -> String
renderTest (Test label True) = "OK: " ++ label
renderTest (Test label False) = "ERROR: " ++ label

environmentOf :: State -> Environment
environmentOf (environment, _) = environment

storeOf :: State -> Store
storeOf (_, store) = store

controlProgram :: String
controlProgram =
  concat
    [ "int x = 0; int y = 0; "
    , "while (x <= 2) { "
    , "if (x == 1) { y = (y + 2); } "
    , "else { y = (y + 1); } "
    , "x = (x + 1); }"
    ]

pointerProgram :: String
pointerProgram = "int x = 5; int *p = &x; *p = 8;"

pointerASA :: Command
pointerASA =
  Seq
    (Decl "x" (Num 5))
    (Seq
      (PtrDecl "p" (Addr "x"))
      (AssignDeref (Id "p") (Num 8)))

copyProgram :: String
copyProgram = "int x = 5; int y = x; y = 8;"

redirectProgram :: String
redirectProgram =
  "int x = 1; int y = 2; int *p = &x; p = &y; *p = 10;"

pointerCopyProgram :: String
pointerCopyProgram =
  "int x = 3; int *p = &x; int *q = p; *q = 7;"

tests :: [Test]
tests =
  [ assertEqual "ASA del programa con apuntador" pointerASA (compile pointerProgram)
  , assertEqual
      "conserva el programa de la Nota 18"
      (IntV 3, IntV 4)
      (valueOf "x" (runEmpty controlProgram), valueOf "y" (runEmpty controlProgram))
  , assertEqual
      "una copia entera no crea alias"
      (IntV 5, IntV 8)
      (valueOf "x" (runEmpty copyProgram), valueOf "y" (runEmpty copyProgram))
  , assertEqual
      "direccion de una variable"
      (PtrV 0)
      (interpE (Addr "x") [("x", 0)] [(0, IntV 5)])
  , assertEqual
      "desreferencia de un apuntador"
      (IntV 5)
      (interpE (Deref (Id "p"))
        [("p", 1), ("x", 0)]
        [(1, PtrV 0), (0, IntV 5)])
  , assertEqual
      "asignacion indirecta modifica x"
      (IntV 8)
      (valueOf "x" (runEmpty pointerProgram))
  , assertEqual
      "p conserva la referencia a x"
      0
      (pointerTargetOf "p" (runEmpty pointerProgram))
  , assertEqual
      "cada variable conserva su propia localidad"
      [("p", 1), ("x", 0)]
      (environmentOf (runEmpty pointerProgram))
  , assertEqual
      "memoria despues de la asignacion indirecta"
      [(0, IntV 8), (1, PtrV 0), (0, IntV 5)]
      (storeOf (runEmpty pointerProgram))
  , assertEqual
      "reasignar p no modifica x"
      (IntV 1)
      (valueOf "x" (runEmpty redirectProgram))
  , assertEqual
      "la nueva referencia de p apunta a y"
      1
      (pointerTargetOf "p" (runEmpty redirectProgram))
  , assertEqual
      "la asignacion posterior modifica y"
      (IntV 10)
      (valueOf "y" (runEmpty redirectProgram))
  , assertEqual
      "copiar un apuntador comparte el destino"
      (IntV 7)
      (valueOf "x" (runEmpty pointerCopyProgram))
  , assertEqual
      "desreferencia una direccion sin variable intermedia"
      (IntV 6)
      (valueOf "y" (runEmpty "int x = 6; int y = *&x;"))
  , assertEqual
      "una desreferencia participa en aritmetica"
      (IntV 7)
      (valueOf "y"
        (runEmpty "int x = 5; int *p = &x; int y = (*p + 2);"))
  , assertEqual
      "while puede consultar mediante un apuntador"
      (IntV 3)
      (valueOf "x"
        (runEmpty
          "int x = 0; int *p = &x; while (*p <= 2) { *p = (*p + 1); }"))
  ]

report :: String
report = unlines (map renderTest tests)

allPassed :: Bool
allPassed = all passed tests
