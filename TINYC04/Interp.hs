module Interp where

import Grammars

type Identifier = String
type Location = Int
type Environment = [(Identifier, Location)]
type Store = [(Location, Value)]
type State = (Environment, Store)
type Output = [Integer]
type Execution = (State, Output)
type ExprResult = (DataValue, Store, Output)

data DataValue
  = NumV Integer
  | RefV Location
  deriving (Eq, Show)

data Value
  = DataV DataValue
  | FunD Identifier Command Exp Environment
  | VoidFunD Identifier Command Environment
  deriving (Eq, Show)

-- Expresiones --------------------------------------------------------------

interpE :: Exp -> Environment -> Store -> ExprResult
interpE (Num number) _ store = (NumV number, store, [])
interpE (Id identifier) environment store =
  (storedData (lookupStore location store), store, [])
  where
    location = lookupEnvironment identifier environment
interpE (Addr identifier) environment store =
  (addressOf location (lookupStore location store), store, [])
  where
    location = lookupEnvironment identifier environment
interpE (Deref expression) environment store =
  (dereference value resultingStore, resultingStore, output)
  where
    (value, resultingStore, output) = interpE expression environment store
interpE (Call function argument) environment store =
  applyFunction
    (lookupStore functionLocation argumentStore)
    argumentValue
    argumentStore
    argumentOutput
  where
    functionLocation = lookupEnvironment function environment
    (argumentValue, argumentStore, argumentOutput) =
      interpE argument environment store
interpE (Add left right) environment store =
  integerBinary (+) left right environment store
interpE (Sub left right) environment store =
  integerBinary (-) left right environment store
interpE (Mul left right) environment store =
  integerBinary (*) left right environment store
interpE (Eq left right) environment store =
  integerComparison (==) left right environment store
interpE (Leq left right) environment store =
  integerComparison (<=) left right environment store
interpE (Not expression) environment store =
  (canonical (integerOf value == 0), resultingStore, output)
  where
    (value, resultingStore, output) = interpE expression environment store

integerBinary
  :: (Integer -> Integer -> Integer)
  -> Exp
  -> Exp
  -> Environment
  -> Store
  -> ExprResult
integerBinary operation left right environment store =
  ( NumV (operation (integerOf leftValue) (integerOf rightValue))
  , finalStore
  , leftOutput ++ rightOutput
  )
  where
    (leftValue, intermediateStore, leftOutput) =
      interpE left environment store
    (rightValue, finalStore, rightOutput) =
      interpE right environment intermediateStore

integerComparison
  :: (Integer -> Integer -> Bool)
  -> Exp
  -> Exp
  -> Environment
  -> Store
  -> ExprResult
integerComparison relation left right environment store =
  ( canonical (relation (integerOf leftValue) (integerOf rightValue))
  , finalStore
  , leftOutput ++ rightOutput
  )
  where
    (leftValue, intermediateStore, leftOutput) =
      interpE left environment store
    (rightValue, finalStore, rightOutput) =
      interpE right environment intermediateStore

applyFunction :: Value -> DataValue -> Store -> Output -> ExprResult
applyFunction
  (FunD parameter body result declarationEnvironment)
  argumentValue
  store
  argumentOutput =
    (resultValue, resultStore, argumentOutput ++ bodyOutput ++ resultOutput)
  where
    parameterLocation = fresh store
    callEnvironment =
      (parameter, parameterLocation) : declarationEnvironment
    callStore =
      (parameterLocation, DataV argumentValue) : store
    ((bodyEnvironment, bodyStore), bodyOutput) =
      interp body callEnvironment callStore
    (resultValue, resultStore, resultOutput) =
      interpE result bodyEnvironment bodyStore
applyFunction (DataV _) _ _ _ =
  error "Se intento llamar un dato"
applyFunction (VoidFunD _ _ _) _ _ _ =
  error "Una funcion void no produce un resultado"

-- Comandos -----------------------------------------------------------------

interp :: Command -> Environment -> Store -> Execution
interp (Decl identifier expression) environment store =
  ( ( (identifier, location) : environment
    , (location, DataV (requireInteger value)) : resultingStore
    )
  , output
  )
  where
    (value, resultingStore, output) = interpE expression environment store
    location = fresh resultingStore
interp (PtrDecl identifier expression) environment store =
  ( ( (identifier, location) : environment
    , (location, DataV (requireReference value)) : resultingStore
    )
  , output
  )
  where
    (value, resultingStore, output) = interpE expression environment store
    location = fresh resultingStore
interp (Assign identifier expression) environment store =
  ( (environment, (location, DataV updatedValue) : resultingStore)
  , output
  )
  where
    (newValue, resultingStore, output) =
      interpE expression environment store
    location = lookupEnvironment identifier environment
    oldValue = storedData (lookupStore location resultingStore)
    updatedValue = compatibleUpdate oldValue newValue
interp (AssignDeref pointerExpression valueExpression) environment store =
  ( (environment, (location, DataV (requireInteger value)) : finalStore)
  , pointerOutput ++ valueOutput
  )
  where
    (pointer, intermediateStore, pointerOutput) =
      interpE pointerExpression environment store
    location = integerTarget pointer intermediateStore
    (value, finalStore, valueOutput) =
      interpE valueExpression environment intermediateStore
interp (FunDecl function parameter body result) environment store =
  ( (declarationEnvironment, (location, definition) : store)
  , []
  )
  where
    location = fresh store
    declarationEnvironment = (function, location) : environment
    definition = FunD parameter body result declarationEnvironment
interp (VoidDecl function parameter body) environment store =
  ( (declarationEnvironment, (location, definition) : store)
  , []
  )
  where
    location = fresh store
    declarationEnvironment = (function, location) : environment
    definition = VoidFunD parameter body declarationEnvironment
interp (CallVoid "print" argument) environment store =
  ( (environment, resultingStore)
  , output ++ [integerOf value]
  )
  where
    (value, resultingStore, output) = interpE argument environment store
interp (CallVoid function argument) environment store =
  applyVoidFunction
    environment
    (lookupStore functionLocation argumentStore)
    argumentValue
    argumentStore
    argumentOutput
  where
    functionLocation = lookupEnvironment function environment
    (argumentValue, argumentStore, argumentOutput) =
      interpE argument environment store
interp (Seq first second) environment store =
  (finalState, firstOutput ++ secondOutput)
  where
    ((intermediateEnvironment, intermediateStore), firstOutput) =
      interp first environment store
    (finalState, secondOutput) =
      interp second intermediateEnvironment intermediateStore
interp (If condition yes no) environment store =
  chooseBranch
    (interpE condition environment store)
    yes
    no
    environment
interp loop@(While condition body) environment store =
  continueWhile
    (interpE condition environment store)
    loop
    body
    environment

applyVoidFunction
  :: Environment
  -> Value
  -> DataValue
  -> Store
  -> Output
  -> Execution
applyVoidFunction
  callerEnvironment
  (VoidFunD parameter body declarationEnvironment)
  argumentValue
  store
  argumentOutput =
    ((callerEnvironment, bodyStore), argumentOutput ++ bodyOutput)
  where
    parameterLocation = fresh store
    callEnvironment =
      (parameter, parameterLocation) : declarationEnvironment
    callStore =
      (parameterLocation, DataV argumentValue) : store
    ((_, bodyStore), bodyOutput) = interp body callEnvironment callStore
applyVoidFunction _ (DataV _) _ _ _ =
  error "Se intento llamar un dato"
applyVoidFunction _ (FunD _ _ _ _) _ _ _ =
  error "Una funcion con resultado no es una llamada void"

chooseBranch
  :: ExprResult
  -> Command
  -> Command
  -> Environment
  -> Execution
chooseBranch (value, resultingStore, conditionOutput) yes no environment
  | integerOf value == 0 =
      prependOutput conditionOutput (interp no environment resultingStore)
  | otherwise =
      prependOutput conditionOutput (interp yes environment resultingStore)

continueWhile
  :: ExprResult
  -> Command
  -> Command
  -> Environment
  -> Execution
continueWhile (value, conditionStore, conditionOutput) loop body environment
  | integerOf value == 0 =
      ((environment, conditionStore), conditionOutput)
  | otherwise =
      ( finalState
      , conditionOutput ++ bodyOutput ++ remainingOutput
      )
  where
    ((bodyEnvironment, bodyStore), bodyOutput) =
      interp body environment conditionStore
    (finalState, remainingOutput) =
      interp loop bodyEnvironment bodyStore

prependOutput :: Output -> Execution -> Execution
prependOutput prefix (state, output) = (state, prefix ++ output)

-- Datos y apuntadores ------------------------------------------------------

canonical :: Bool -> DataValue
canonical True = NumV 1
canonical False = NumV 0

integerOf :: DataValue -> Integer
integerOf (NumV number) = number
integerOf (RefV _) =
  error "Se esperaba un numero y se obtuvo una referencia"

referenceOf :: DataValue -> Location
referenceOf (RefV location) = location
referenceOf (NumV _) =
  error "Se esperaba una referencia y se obtuvo un numero"

requireInteger :: DataValue -> DataValue
requireInteger value@(NumV _) = value
requireInteger (RefV _) =
  error "Una variable int debe inicializarse con un numero"

requireReference :: DataValue -> DataValue
requireReference value@(RefV _) = value
requireReference (NumV _) =
  error "Una variable int * debe inicializarse con una referencia"

compatibleUpdate :: DataValue -> DataValue -> DataValue
compatibleUpdate (NumV _) newValue@(NumV _) = newValue
compatibleUpdate (RefV _) newValue@(RefV _) = newValue
compatibleUpdate _ _ =
  error "La asignacion cambia la clase de dato de la variable"

addressOf :: Location -> Value -> DataValue
addressOf location (DataV (NumV _)) = RefV location
addressOf _ (DataV (RefV _)) =
  error "tinyC v4 no incluye apuntadores a apuntadores"
addressOf _ (FunD _ _ _ _) =
  error "tinyC v4 no incluye apuntadores a funciones"
addressOf _ (VoidFunD _ _ _) =
  error "tinyC v4 no incluye apuntadores a funciones"

dereference :: DataValue -> Store -> DataValue
dereference (RefV location) store =
  dereferencedValue (lookupStore location store)
dereference (NumV _) _ =
  error "No se puede desreferenciar un numero"

dereferencedValue :: Value -> DataValue
dereferencedValue value@(DataV (NumV _)) = storedData value
dereferencedValue (DataV (RefV _)) =
  error "tinyC v4 no incluye apuntadores a apuntadores"
dereferencedValue (FunD _ _ _ _) =
  error "tinyC v4 no incluye apuntadores a funciones"
dereferencedValue (VoidFunD _ _ _) =
  error "tinyC v4 no incluye apuntadores a funciones"

integerTarget :: DataValue -> Store -> Location
integerTarget (RefV location) store =
  confirmIntegerTarget location (lookupStore location store)
integerTarget (NumV _) _ =
  error "El lado izquierdo de la asignacion indirecta no es una referencia"

confirmIntegerTarget :: Location -> Value -> Location
confirmIntegerTarget location (DataV (NumV _)) = location
confirmIntegerTarget _ (DataV (RefV _)) =
  error "tinyC v4 no incluye apuntadores a apuntadores"
confirmIntegerTarget _ (FunD _ _ _ _) =
  error "tinyC v4 no incluye apuntadores a funciones"
confirmIntegerTarget _ (VoidFunD _ _ _) =
  error "tinyC v4 no incluye apuntadores a funciones"

-- Ambiente y memoria -------------------------------------------------------

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

storedData :: Value -> DataValue
storedData (DataV value) = value
storedData (FunD _ _ _ _) =
  error "Una funcion no es un dato de primera clase"
storedData (VoidFunD _ _ _) =
  error "Una funcion void no es un dato de primera clase"

stateOf :: Execution -> State
stateOf (state, _) = state

outputOf :: Execution -> Output
outputOf (_, output) = output

valueOf :: Identifier -> State -> DataValue
valueOf identifier (environment, store) =
  storedData (lookupStore (lookupEnvironment identifier environment) store)

integerValueOf :: Identifier -> State -> Integer
integerValueOf identifier state = integerOf (valueOf identifier state)

referenceTargetOf :: Identifier -> State -> Location
referenceTargetOf identifier state = referenceOf (valueOf identifier state)

currentValues :: State -> [(Identifier, DataValue)]
currentValues (environment, store) =
  [ (identifier, storedData value)
  | (identifier, location) <- reverse environment
  , let value = lookupStore location store
  , isData value
  ]

isData :: Value -> Bool
isData (DataV _) = True
isData (FunD _ _ _ _) = False
isData (VoidFunD _ _ _) = False
