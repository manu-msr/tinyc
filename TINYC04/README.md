# tinyC v4: funciones

Esta versión implementa la primera parte de la Nota 20. Conserva variables,
apuntadores y estructuras de control, y añade funciones con la sintaxis de
C. Un programa sigue siendo una secuencia de comandos: no existe una función
`main` y una función sólo puede llamarse después de su declaración.

```c
int x = 5;

void incrementar(int *p) {
  *p = (*p + 1);
  return;
}

incrementar(&x);

int doble(int n) {
  int resultado = (n + n);
  return resultado;
}

int y = doble(x);
print(x);
print(y);
```

El programa anterior deja `x = 6`, `y = 12` y produce la salida `[6, 12]`.

## Alcance de la versión

- Las funciones pueden regresar `int`, `int *` o no producir un resultado
  mediante `void`.
- Cada función recibe exactamente un parámetro.
- `return` aparece una sola vez y al final del cuerpo.
- Una llamada es una expresión cuando la función produce un dato y es un
  comando cuando la función es `void`.
- `print(e);` muestra únicamente números y está disponible sin declaración.
- Las funciones pueden ser recursivas.
- Las funciones no son datos de primera clase: no pueden asignarse, retornarse
  ni utilizarse mediante `&`.
- Se conservan las restricciones de v3: no hay apuntadores a apuntadores,
  apuntadores a funciones, aritmética de apuntadores ni memoria dinámica.

## Ambiente y memoria

El ambiente continúa asociando identificadores con localidades. La memoria
ahora puede contener datos o descriptores internos de función:

```text
DataValue ::= NumV Integer | RefV Location
Value     ::= DataV DataValue
            | FunD parámetro cuerpo retorno ambiente
            | VoidFunD parámetro cuerpo ambiente
```

Una declaración reserva una localidad para la función y guarda allí su
descriptor. El descriptor conserva el ambiente disponible en el momento de
la declaración e incluye el nombre de la propia función para permitir
recursión.

Las llamadas pueden modificar la memoria incluso cuando aparecen dentro de
una expresión. Por ello `interpE` devuelve el dato calculado, la memoria
resultante y la salida producida. Los operandos se recorren de izquierda a
derecha.

La salida de `print` se mantiene separada de la memoria:

```text
type Execution = (State, Output)
type Output = [Integer]
```

## Componentes

- `Lex.x`: añade `void`, `return` y la operación predefinida `print`.
- `Grammars.y`: extiende la sintaxis concreta y el ASA con declaraciones y
  llamadas.
- `Interp.hs`: implementa datos, descriptores, declaraciones, llamadas,
  recursión y salida.
- `TinyC.hs`: permite analizar, ejecutar y mostrar programas completos.
- `Tests.hs`: comprueba la extensión y la conservación de los apuntadores de
  v3.

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
> testFunctions
> testRecursion
> run
```
