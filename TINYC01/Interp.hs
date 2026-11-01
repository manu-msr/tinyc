module Interp where

import Grammars

type Identifier = String
type Location = Int
type Environment = [(Identifier, Location)]
type Store = [(Location, Integer)]
type State = (Environment, Store)

interpA :: AExp -> Environment -> Store -> Integer
interpA (Num number) _ _ = number
interpA (Id identifier) environment store =
  lookupStore (lookupEnvironment identifier environment) store
interpA (Add left right) environment store =
  interpA left environment store + interpA right environment store
interpA (Sub left right) environment store =
  interpA left environment store - interpA right environment store
interpA (Mul left right) environment store =
  interpA left environment store * interpA right environment store

interp :: Command -> Environment -> Store -> State
interp (Decl identifier expression) environment store =
  ( (identifier, location) : environment
  , (location, value) : store
  )
  where
    location = fresh store
    value = interpA expression environment store
interp (Assign identifier expression) environment store =
  (environment, (location, value) : store)
  where
    location = lookupEnvironment identifier environment
    value = interpA expression environment store
interp (Seq first second) environment store =
  interp second intermediateEnvironment intermediateStore
  where
    (intermediateEnvironment, intermediateStore) =
      interp first environment store

lookupEnvironment :: Identifier -> Environment -> Location
lookupEnvironment identifier [] =
  error ("Identificador no declarado: " ++ identifier)
lookupEnvironment identifier ((boundIdentifier, location) : environment)
  | identifier == boundIdentifier = location
  | otherwise = lookupEnvironment identifier environment

lookupStore :: Location -> Store -> Integer
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

valueOf :: Identifier -> State -> Integer
valueOf identifier (environment, store) =
  lookupStore (lookupEnvironment identifier environment) store

currentValues :: State -> [(Identifier, Integer)]
currentValues (environment, store) =
  [ (identifier, lookupStore location store)
  | (identifier, location) <- reverse environment
  ]
