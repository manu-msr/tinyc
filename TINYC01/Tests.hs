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

conductor :: String
conductor =
  "int x = 2; int y = (x - 2); y = (x + 3); x = (y * 2);"

conductorASA :: Command
conductorASA =
  Seq
    (Decl "x" (Num 2))
    (Seq
      (Decl "y" (Sub (Id "x") (Num 2)))
      (Seq
        (Assign "y" (Add (Id "x") (Num 3)))
        (Assign "x" (Mul (Id "y") (Num 2)))))

tests :: [Test]
tests =
  [ assertEqual
      "ASA de la Nota 17"
      conductorASA
      (compile conductor)
  , assertEqual
      "asociacion a la derecha"
      (Seq
        (Decl "x" (Num 1))
        (Seq
          (Assign "x" (Add (Id "x") (Num 1)))
          (Assign "x" (Mul (Id "x") (Num 3)))))
      (compile "int x = 1; x = (x + 1); x = (x * 3);")
  , assertEqual
      "parentesis aritmeticos"
      (Assign "x" (Add (Num 2) (Mul (Num 3) (Num 4))))
      (compile "x = (2 + (3 * 4));")
  , assertEqual "valor final de x" 10 (valueOf "x" (runEmpty conductor))
  , assertEqual "valor final de y" 5 (valueOf "y" (runEmpty conductor))
  , assertEqual
      "localidades consecutivas"
      [("y", 1), ("x", 0)]
      (environmentOf (runEmpty conductor))
  , assertEqual
      "asociaciones sombreadas"
      [(0, 10), (1, 5), (1, 0), (0, 2)]
      (storeOf (runEmpty conductor))
  , assertEqual
      "valores visibles"
      [("x", 10), ("y", 5)]
      (currentValues (runEmpty conductor))
  , assertEqual
      "localidad fresca despues de una asignacion"
      [("y", 1), ("x", 0)]
      (environmentOf (runEmpty "int x = 1; x = 2; int y = (x + 1);"))
  , assertEqual
      "resta entera"
      (-3)
      (valueOf "x" (runEmpty "int x = (0 - 3);"))
  , assertEqual
      "inicializador con una variable anterior"
      7
      (valueOf "y" (runEmpty "int x = 4; int y = (x + 3);"))
  ]

report :: String
report = unlines (map renderTest tests)

allPassed :: Bool
allPassed = all passed tests
