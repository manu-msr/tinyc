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

controlASA :: Command
controlASA =
  Seq
    (Decl "x" (Num 0))
    (Seq
      (Decl "y" (Num 0))
      (While
        (Leq (Id "x") (Num 2))
        (Seq
          (If
            (Eq (Id "x") (Num 1))
            (Assign "y" (Add (Id "y") (Num 2)))
            (Assign "y" (Add (Id "y") (Num 1))))
          (Assign "x" (Add (Id "x") (Num 1))))))

sequentialProgram :: String
sequentialProgram =
  "int x = 2; int y = (x - 2); y = (x + 3); x = (y * 2);"

notProgram :: String
notProgram =
  concat
    [ "int x = 3; int y = 0; "
    , "while (! (x == 0)) { "
    , "y = (y + x); x = (x - 1); }"
    ]

tests :: [Test]
tests =
  [ assertEqual "ASA de la Nota 18" controlASA (compile controlProgram)
  , assertEqual
      "conserva los programas secuenciales"
      10
      (valueOf "x" (runEmpty sequentialProgram))
  , assertEqual
      "igualdad verdadera"
      1
      (interpA (Eq (Num 3) (Add (Num 1) (Num 2))) [] [])
  , assertEqual
      "igualdad falsa"
      0
      (interpA (Eq (Num 3) (Num 4)) [] [])
  , assertEqual
      "orden verdadero"
      1
      (interpA (Leq (Num 3) (Num 3)) [] [])
  , assertEqual
      "orden falso"
      0
      (interpA (Leq (Num 4) (Num 3)) [] [])
  , assertEqual "negacion de cero" 1 (interpA (Not (Num 0)) [] [])
  , assertEqual
      "negacion de un entero no cero"
      0
      (interpA (Not (Sub (Num 0) (Num 4))) [] [])
  , assertEqual
      "comparacion como inicializador"
      1
      (valueOf "x" (runEmpty "int x = (2 == 2);"))
  , assertEqual
      "if con condicion no cero"
      1
      (valueOf "x"
        (runEmpty "int x = 0; if 1 { x = 1; } else { x = 2; }"))
  , assertEqual
      "if con condicion cero"
      2
      (valueOf "x"
        (runEmpty "int x = 0; if 0 { x = 1; } else { x = 2; }"))
  , assertEqual
      "cualquier entero no cero es verdadero"
      7
      (valueOf "x"
        (runEmpty
          "int x = 0; if (0 - 3) { x = 7; } else { x = 9; }"))
  , assertEqual
      "la rama descartada no se ejecuta"
      1
      (valueOf "x"
        (runEmpty "int x = 0; if 1 { x = 1; } else { y = 2; }"))
  , assertEqual
      "while con cero iteraciones"
      4
      (valueOf "x"
        (runEmpty "int x = 4; while 0 { x = (x + 1); }"))
  , assertEqual
      "while reevalua su condicion"
      3
      (valueOf "x"
        (runEmpty "int x = 0; while (x <= 2) { x = (x + 1); }"))
  , assertEqual "valor final de x" 3 (valueOf "x" (runEmpty controlProgram))
  , assertEqual "valor final de y" 4 (valueOf "y" (runEmpty controlProgram))
  , assertEqual
      "ambiente final"
      [("y", 1), ("x", 0)]
      (environmentOf (runEmpty controlProgram))
  , assertEqual
      "memoria con asociaciones sombreadas"
      [ (0, 3), (1, 4), (0, 2), (1, 3)
      , (0, 1), (1, 1), (1, 0), (0, 0)
      ]
      (storeOf (runEmpty controlProgram))
  , assertEqual "ciclo con negacion" 0 (valueOf "x" (runEmpty notProgram))
  , assertEqual "acumulacion con negacion" 6 (valueOf "y" (runEmpty notProgram))
  ]

report :: String
report = unlines (map renderTest tests)

allPassed :: Bool
allPassed = all passed tests
