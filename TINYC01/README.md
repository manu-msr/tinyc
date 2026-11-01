# tinyC v1: programas secuenciales

Esta versión implementa el lenguaje de la Nota 17. Su sintaxis separa las
expresiones aritméticas de los comandos y exige paréntesis en cada operación
binaria. El ASA contiene `Num`, `Id`, `Add`, `Sub`, `Mul`, `Decl`, `Assign` y
`Seq`.

El intérprete es directo y corresponde a la semántica de paso grande de la
máquina CES. El ambiente asocia identificadores con localidades y la memoria
asocia localidades con enteros. Una actualización se agrega al inicio de la
memoria: las asociaciones anteriores permanecen en la lista, pero quedan
sombreadas por la primera coincidencia.

La implementación semántica usa ecuaciones y coincidencia de patrones. Los
programas se suponen bien formados, como en la nota; por ello el intérprete
devuelve directamente el estado final y las consultas fallidas quedan como
errores, igual que las variables libres en los intérpretes de MiniLisp. El
único uso de `IO` se encuentra en la interfaz interactiva.

## Componentes

- `Lex.x`: unidades léxicas de la sintaxis concreta.
- `Grammars.y`: gramática concreta y constructores del ASA.
- `Interp.hs`: ambiente, memoria y evaluadores directos `interpA` e `interp`.
- `TinyC.hs`: funciones para analizar y ejecutar programas, además del REPL.
- `Tests.hs`: análisis, secuenciación, localidades y sombreado de la memoria.

No hay un archivo `Desugar.hs`: en esta versión cada forma concreta se traduce
directamente a un constructor del ASA y no existe azúcar sintáctica.

## Compilación y pruebas

```text
alex Lex.x
happy --ghc Grammars.y
ghc -package array -Wall -fno-code Tests.hs
ghc -package array Tests.hs -e 'putStr report'
```

En GHCi:

```text
ghci -package array TinyC.hs
> testConductor
Ambiente: [("y",1),("x",0)]
Memoria: [(0,10),(1,5),(1,0),(0,2)]
Valores actuales: [("x",10),("y",5)]
> run
```
