Laboratorio 4 — Documentación y guía de uso

Resumen
-------
Este repositorio contiene la solución del Laboratorio 4. La implementación incluye:
- Un servidor HTTP que recibe comandos en formato ToCliCommand (texto plano), los ejecuta y devuelve una respuesta en texto plano.
- Un cliente implementado como Free‑monad DSL con dos intérpretes:
  - `interpretLocal`: intérprete local que actualiza un `Lib3.State` usando la mónada `State` (paquete `transformers`).
  - `interpretHTTP`: intérprete que envía los comandos al servidor vía HTTP (POST a `/cmd`).
- Parseadores hechos a mano (`src/Lib4.hs`) para la gramática del dominio (sin usar librerías externas de parseo).
- Persistencia del servidor (carga al inicio, guardados periódicos y guardado tras cambios) usando `Lib3`.

Estructura de archivos importantes
---------------------------------
- `app4/server/Main.hs`  : ejecutable del servidor (Warp/WAI POST endpoint).
- `app4/client/Main.hs`  : ejecutable del cliente — ejecuta un programa DSL de ejemplo y permite seleccionar intérprete con `--interpreter local|http`.
- `src/Lib1.hs`          : tipos del dominio (`Command`, `Action`, `Dribbleresult`, `Card`, `examples`).
- `src/Lib2.hs`          : serializador / formateador `ToCliCommand` y helpers para construir esas cadenas.
- `src/Lib3.hs`          : lógica del servidor y estado compartido (actualizaciones, estadísticas y persistencia).
- `src/Lib4.hs`          : parser manual; contiene `parseCommand` y parsers para cada acción/comando.
- `src/Lib4DSL.hs`       : Free‑monad DSL, smart constructors y los dos intérpretes (`interpretLocal` y `interpretHTTP`).
- `test/Spec.hs`         : tests automáticos (incluye QuickCheck property y pruebas de parseo).
- `fp2025_state.txt`     : fichero de persistencia usado por el servidor (guardado y cargado por `Lib3`).

Cómo funciona — alto nivel
-------------------------
1. El cliente (programa Haskell) es un pequeño runner que compone un programa DSL (construido mediante las smart constructors) y lo ejecuta con el intérprete elegido.
2. `interpretLocal` ejecuta las operaciones directamente en memoria, actualizando un `Lib3.State` local (útil para debugging o para ejecutar sin servidor).
3. `interpretHTTP` convierte cada operación DSL a su representación textual mediante `Lib2.toCliCommand` y hace un POST en texto plano a `http://127.0.0.1:4000/cmd`.
4. El servidor (Warp) lee el cuerpo de la petición, lo parsea con `Lib4.parseCommand`, ejecuta la acción contra su `TVar Lib3.State` y devuelve el resultado en el body como texto plano.
5. Persistencia: `main` del servidor levanta `Lib3.storageOpLoop` y realiza `Lib3.load` al inicio; varias operaciones llaman a `Lib3.save` y además hay un hilo que guarda periódicamente cada ~5s.

Cómo ejecutar
-------------
Abrir PowerShell en la raíz del repo (`c:\Users\Luken\Desktop\Erasmus\FunctionalPrograming\ownLanguage`) y usar `stack`.

1) Ejecutar tests:
```powershell
stack test
```
Salida esperada: `All 10 tests passed` (u otras cifras si modificas tests).

2) Iniciar el servidor (en una terminal):
```powershell
stack exec fp2025-server
```
Verás logs: `Starting server...`, `Listening on port 4000`, y por cada petición `Received HTTP body:` y `Sending response:`.

3) Ejecutar el cliente (intérprete local — no necesita servidor):
```powershell
stack exec fp2025-client -- --interpreter local
```
Salida: el cliente ejecuta el `sampleProgram` (hard‑coded en `app4/client/Main.hs`), y muestra `Result from local interpreter:` seguido de la salida.

4) Ejecutar el cliente (intérprete HTTP — el servidor debe estar corriendo):
```powershell
stack exec fp2025-client -- --interpreter http
```
Salida: el cliente enviará las operaciones al servidor y mostrará la respuesta que reciba.

5) Probar la API HTTP manualmente con PowerShell (recomendado):
PowerShell incluye `Invoke-RestMethod` que acepta encabezados en forma de hashtable:
```powershell
Invoke-RestMethod -Uri 'http://127.0.0.1:4000/cmd' -Method POST \
  -Headers @{ 'Content-Type' = 'text/plain' } \
  -Body 'enter "Neymar"'

Invoke-RestMethod -Uri 'http://127.0.0.1:4000/cmd' -Method POST \
  -Headers @{ 'Content-Type' = 'text/plain' } \
  -Body 'play "Neymar" [ pass "Neymar" to "Ronaldo" ]'

Invoke-RestMethod -Uri 'http://127.0.0.1:4000/cmd' -Method POST \
  -Headers @{ 'Content-Type' = 'text/plain' } \
  -Body 'showstats "Neymar"'
```
Si prefieres la utilidad `curl` real, usa `curl.exe` en PowerShell (o WSL):
```powershell
curl.exe -X POST -H "Content-Type: text/plain" --data 'enter "Neymar"' http://127.0.0.1:4000/cmd
```

Explicación del código (puntos clave)
------------------------------------
- `src/Lib1.hs`:
  - Define los tipos del dominio: `Action`, `Command`, `Dribbleresult`, `Card` y `examples` (lista de comandos de ejemplo usada por tests y Arbitrary).

- `src/Lib2.hs`:
  - Serializa comandos/acciones a la representación de texto (ToCliCommand). Este texto es exactamente el que el servidor espera en el cuerpo HTTP y el que el parser consume.

- `src/Lib3.hs`:
  - Define `State` (estructura que contiene la lista de comandos ejecutados y otros datos) y funciones puras de actualización `updateStatePure`, `updateStats`, etc.
  - Maneja la persistencia y un loop (`storageOpLoop`) para realizar `save`/`load` usando `Chan` y `TVar`.

- `src/Lib4.hs` (parser):
  - Implementa un parser hecho a mano usando `type Parser = ExceptT ErrorMsg (State Input)`.
  - Parsers por acción: `parsePass`, `parseDribble`, `parseFoul`, `parseHardFoul`, `parseShot`, `parseGoal`, `parseSteal`, `parseComposite`.
  - Parsers por comando: `parsePlay`, `parseEnter`, `parseSubstitution`, `parseShowStats`, `parseShowActivePlayers`, `parseDump`.
  - `parseCommand` combina los parsers en orden (la elección de orden importa).
  - Nota: si necesitas la documentación BNF por encima de cada parser, puedo añadirla (es una mejora rápida y recomendada para la defensa).

- `src/Lib4DSL.hs` (DSL + intérpretes):
  - Define `ClientF` functor con un constructor por cada `Lib1.Command` y una implementación mínima de Free monad `Client`.
  - Smart constructors: `enter`, `substitution`, `play`, `showStats`, `showActivePlayers`, `dumpExamples`.
  - `interpretLocal :: Client a -> State Lib3.State a` ejecuta las operaciones actualizando `Lib3.State` con `Lib3.updateStatePure`.
  - `interpretHTTP :: Client a -> IO a` crea un `manager` TLS y para cada operación construye la cadena con `Lib2.toCliCommand`, crea una petición `RequestBodyLBS` y hace `httpLbs` al servidor. Devuelve la respuesta (body) como `String`.

- `app4/server/Main.hs`:
  - Arranca `Lib3.storageOpLoop`, carga el estado `Lib3.load`, y crea un hilo que guarda periódicamente con `Lib3.save`.
  - Usa Warp y crea `makeApp` que lee `strictRequestBody`, hace `Lib4.runParser Lib4.parseCommand input` y, si parsea, ejecuta `processCommand` que actualiza `TVar` y devuelve respuestas en texto.
  - `processCommand` implementa la semántica para `Enter`, `Substitution`, `Play`, `ShowStats`, `ShowActivePlayers`, `Dump`.

Puntos a tener en cuenta / recomendaciones finales
------------------------------------------------
- El cliente actual tiene un `sampleProgram` hard‑coded: por eso siempre ves `showstats "Neymar"`. Para ejecutar otras cosas, modifica `app4/client/Main.hs` o pídeme que lo haga por ti (por ejemplo, añadir `--script file` o un REPL).
- Si quieres garantía absoluta de un `save` final en shutdown, puedo añadir manejo de señales a `app4/server/Main.hs`.
- Para la defensa, te recomiendo añadir comentarios BNF sobre cada parser en `src/Lib4.hs` (pequeña tarea, la hago si quieres).

¿Quieres que ahora:
- A) añada el README en otro formato (por ejemplo `README.md`) o haga commit? (Ya creé este archivo `LAB4_README.txt`.)
- B) añada los comentarios BNF en `src/Lib4.hs` ahora? (rápido)
- C) añada un `--script <file>` al cliente o un REPL? (más trabajo)

Archivo creado:
`fp-2025/LAB4_README.txt`

Si quieres que haga alguno de los cambios A–C ahora, dime cuál y lo implemento y pruebo con `stack test`/build.
