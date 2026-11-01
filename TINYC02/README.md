# tinyC v2: estructuras de control

Esta versión implementa la extensión de la Nota 18. Conserva íntegramente el
fragmento secuencial y añade `Eq`, `Leq`, `Not`, `If` y `While`.

tinyC no introduce valores booleanos. Las comparaciones y la negación son
expresiones enteras que producen `0` o `1`; las condiciones de `if` y `while`
interpretan `0` como falso y cualquier entero distinto de cero como verdadero.
Las llaves delimitan comandos compuestos, pero no crean un nuevo alcance.

`interpA` implementa las reglas de expresiones y `interp` implementa las
reglas de comandos. El caso recursivo de `While` ejecuta el cuerpo y vuelve a
interpretar el mismo ASA con el ambiente y la memoria resultantes, tal como lo
establece la regla de paso grande de la nota.

La implementación semántica usa ecuaciones y coincidencia de patrones. Los
programas se suponen bien formados, como en la nota; el intérprete devuelve
directamente el estado final y no introduce un tipo adicional para errores.
El único uso de `IO` se encuentra en la interfaz interactiva.

## Componentes

- `Lex.x`: añade `if`, `else`, `while`, `==`, `<=`, `!` y llaves.
- `Grammars.y`: gramática concreta y ASA extendido.
- `Interp.hs`: semántica directa de expresiones, decisión y repetición.
- `TinyC.hs`: funciones para analizar y ejecutar programas, además del REPL.
- `Tests.hs`: conservación del fragmento secuencial, enteros como condiciones,
  ramas descartadas, ciclos y sombreado de la memoria.

No hay un archivo `Desugar.hs`: todas las formas concretas de esta versión
tienen una representación directa en el ASA.

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
Memoria: [(0,3),(1,4),(0,2),(1,3),(0,1),(1,1),(1,0),(0,0)]
Valores actuales: [("x",3),("y",4)]
> testNot
Ambiente: [("y",1),("x",0)]
Memoria: [(0,0),(1,6),(0,1),(1,5),(0,2),(1,3),(1,0),(0,3)]
Valores actuales: [("x",0),("y",6)]
> run
```
