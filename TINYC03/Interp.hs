module Interp where

import Grammars

type Identifier = String
type Location = Int
type Environment = [(Identifier, Location)]
type Store = [(Location, Value)]
type State = (Environment, Store)

data Value
  = IntV Integer
  | PtrV Location
  deriving (Eq, Show)

interpE :: Exp -> Environment -> Store -> Value
interpE (Num number) _ _ = IntV number
interpE (Id identifier) environment store =
  lookupStore (lookupEnvironment identifier environment) store
interpE (Addr identifier) environment store =
  addressOf location (lookupStore location store)
  where
    location = lookupEnvironment identifier environment
interpE (Deref expression) environment store =
  dereference (interpE expression environment store) store
interpE (Add left right) environment store =
  integerBinary (+)
    (interpE left environment store)
    (interpE right environment store)
interpE (Sub left right) environment store =
  integerBinary (-)
    (interpE left environment store)
    (interpE right environment store)
interpE (Mul left right) environment store =
  integerBinary (*)
    (interpE left environment store)
    (interpE right environment store)
interpE (Eq left right) environment store =
  integerComparison (==)
    (interpE left environment store)
    (interpE right environment store)
interpE (Leq left right) environment store =
  integerComparison (<=)
    (interpE left environment store)
    (interpE right environment store)
interpE (Not expression) environment store =
  canonical (integerOf (interpE expression environment store) == 0)

integerBinary :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
integerBinary operation (IntV left) (IntV right) =
  IntV (operation left right)
integerBinary _ _ _ =
  error "Una operacion aritmetica recibio un apuntador"

integerComparison :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
integerComparison relation (IntV left) (IntV right) =
  canonical (relation left right)
integerComparison _ _ _ =
  error "Una comparacion recibio un apuntador"

canonical :: Bool -> Value
canonical True = IntV 1
canonical False = IntV 0

integerOf :: Value -> Integer
integerOf (IntV number) = number
integerOf (PtrV _) = error "Se esperaba un entero y se obtuvo un apuntador"

pointerOf :: Value -> Location
pointerOf (PtrV location) = location
pointerOf (IntV _) = error "Se esperaba un apuntador y se obtuvo un entero"

requireInteger :: Value -> Value
requireInteger value@(IntV _) = value
requireInteger (PtrV _) =
  error "Una variable int debe inicializarse con un entero"

requirePointer :: Value -> Value
requirePointer value@(PtrV _) = value
requirePointer (IntV _) =
  error "Una variable int * debe inicializarse con un apuntador"

addressOf :: Location -> Value -> Value
addressOf location (IntV _) = PtrV location
addressOf _ (PtrV _) =
  error "tinyC v3 no incluye apuntadores a apuntadores"

dereference :: Value -> Store -> Value
dereference (PtrV location) store =
  dereferencedValue (lookupStore location store)
dereference (IntV _) _ =
  error "No se puede desreferenciar un entero"

dereferencedValue :: Value -> Value
dereferencedValue value@(IntV _) = value
dereferencedValue (PtrV _) =
  error "tinyC v3 no incluye apuntadores a apuntadores"

compatibleUpdate :: Value -> Value -> Value
compatibleUpdate (IntV _) newValue@(IntV _) = newValue
compatibleUpdate (PtrV _) newValue@(PtrV _) = newValue
compatibleUpdate _ _ =
  error "La asignacion cambia la clase de valor de la variable"

integerTarget :: Value -> Store -> Location
integerTarget (PtrV location) store =
  confirmIntegerTarget location (lookupStore location store)
integerTarget (IntV _) _ =
  error "El lado izquierdo de la asignacion indirecta no es un apuntador"

confirmIntegerTarget :: Location -> Value -> Location
confirmIntegerTarget location (IntV _) = location
confirmIntegerTarget _ (PtrV _) =
  error "tinyC v3 no incluye apuntadores a apuntadores"

interp :: Command -> Environment -> Store -> State
interp (Decl identifier expression) environment store =
  ( (identifier, location) : environment
  , (location, value) : store
  )
  where
    location = fresh store
    value = requireInteger (interpE expression environment store)
interp (PtrDecl identifier expression) environment store =
  ( (identifier, location) : environment
  , (location, value) : store
  )
  where
    location = fresh store
    value = requirePointer (interpE expression environment store)
interp (Assign identifier expression) environment store =
  (environment, (location, value) : store)
  where
    location = lookupEnvironment identifier environment
    oldValue = lookupStore location store
    value = compatibleUpdate oldValue (interpE expression environment store)
interp (AssignDeref pointerExpression valueExpression) environment store =
  (environment, (location, value) : store)
  where
    pointerValue = interpE pointerExpression environment store
    location = integerTarget pointerValue store
    value = requireInteger (interpE valueExpression environment store)
interp (Seq first second) environment store =
  interp second intermediateEnvironment intermediateStore
  where
    (intermediateEnvironment, intermediateStore) =
      interp first environment store
interp (If condition yes no) environment store
  | integerOf (interpE condition environment store) == 0 =
      interp no environment store
  | otherwise = interp yes environment store
interp loop@(While condition body) environment store
  | integerOf (interpE condition environment store) == 0 =
      (environment, store)
  | otherwise = interp loop intermediateEnvironment intermediateStore
  where
    (intermediateEnvironment, intermediateStore) =
      interp body environment store

lookupEnvironment :: Identifier -> Environment -> Location
lookupEnvironment identifier [] =
  error ("Identificador no declarado: " ++ identifier)
lookupEnvironment identifier ((boundIdentifier, location) : environment)
  | identifier == boundIdentifier = location
  | otherwise = lookupEnvironment identifier environment

lookupStore :: Location -> Store -> Value
lookupStore location [] =
  error ("Localidad sin contenido: " ++ show location)
lookupStore location ((boundLocation, value) : store)
  | location == boundLocation = value
  | otherwise = lookupStore location store

fresh :: Store -> Location
fresh store = choose 0
  where
    choose location
      | location `elem` map fst store = choose (location + 1)
      | otherwise = location

valueOf :: Identifier -> State -> Value
valueOf identifier (environment, store) =
  lookupStore (lookupEnvironment identifier environment) store

integerValueOf :: Identifier -> State -> Integer
integerValueOf identifier state = integerOf (valueOf identifier state)

pointerTargetOf :: Identifier -> State -> Location
pointerTargetOf identifier state = pointerOf (valueOf identifier state)

currentValues :: State -> [(Identifier, Value)]
currentValues (environment, store) =
  [ (identifier, lookupStore location store)
  | (identifier, location) <- reverse environment
  ]
