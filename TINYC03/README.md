# tinyC v3: variables y referencias

Esta versión implementa la extensión de la Nota 19. Conserva el lenguaje de
la Nota 18 y añade apuntadores a enteros mediante cuatro formas:

```c
int *p = &x;
int y = *p;
*p = 8;
p = &y;
```

`int *p` declara una variable apuntador. Como cualquier variable, `p` tiene
su propia localidad; el contenido de esa localidad es una referencia a la
localidad de una variable entera. `&x` produce esa referencia y `*p` consulta
el entero al que apunta. En el lado izquierdo de una asignación, `*p`
actualiza el entero apuntado.

La versión se limita a apuntadores a enteros. No incluye apuntadores a
apuntadores, aritmética de apuntadores, direcciones nulas, asignación dinámica
ni liberación de memoria.

## Correspondencia semántica

`Value` distingue `IntV Integer` de `PtrV Location`. `interpE` recibe
explícitamente el ambiente y la memoria, mientras que `interp` recibe ambos y
devuelve el estado actualizado. Las expresiones nuevas sólo consultan el
estado; `AssignDeref` construye la memoria resultante sin usar mutación de
Haskell.

Los constructores nuevos son:

- `Addr`: obtiene una referencia con `&`.
- `Deref`: consulta mediante `*`.
- `PtrDecl`: representa `int *p = e;`.
- `AssignDeref`: representa `*p = e;`.

Las asignaciones directas preservan la clase de la variable: una variable
entera sigue almacenando enteros y una variable apuntador sigue almacenando
referencias.

## Componentes

- `Lex.x`: añade el token `&`; `*` se conserva y se usa según su contexto.
- `Grammars.y`: gramática concreta y ASA de tinyC v3.
- `Interp.hs`: valores, ambiente, memoria e intérpretes directos.
- `TinyC.hs`: funciones para analizar y ejecutar programas, además del REPL.
- `Tests.hs`: conservación de v2, copia, referencias y asignación indirecta.

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
> testPointer
Ambiente: [("p",1),("x",0)]
Memoria: [(0,IntV 8),(1,PtrV 0),(0,IntV 5)]
Valores actuales: [x = 8, p = ref(0)]
> testRedirect
> run
```
