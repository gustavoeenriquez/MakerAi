# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 082 — **Computer Use: quién ejecuta la acción**.

Un `computer_call` puede resolverlo el propio proceso o cedérselo a otro. Este demo ejerce las tres salidas posibles contra el API real, **sin tocar la pantalla**, y verifica cada una con asserts y exit code.

| Caso | Situación | Qué debe pasar |
|------|-----------|----------------|
| **DELEGADA** | `OnCallToolFunction` llena `ToolCall.Response` | El driver no ejecuta nada en local; **no** emite `computer_call_output` y **no** recurre. El `computer_call` crudo queda en el historial como mensaje `assistant` con su `ToolCallId` |
| **SIN TOOL** | `cap_ComputerUse` activo pero falta `ChatTools.ComputerUseTool` | Configuración incompleta: el turno se corta con `LastError` en vez de mandar un output sin imagen (400 seguro) |
| **LOCAL** | Hay ejecutor (`OnExecuteAction` + `OnRequestScreenshot`) | Bucle agéntico normal: ejecuta, captura, emite `computer_call_output` y recurre |

Cada caso se corre **en síncrono y en streaming**, porque el driver de OpenAI tiene dos implementaciones separadas del `computer_call`: la síncrona en `ParseChat` y la de streaming en `OnInternalReceiveData`.

### Por qué existe, y en qué se diferencia del 066

El demo 066 ejecuta Computer Use **de verdad**: mueve el ratón, teclea y le manda tu escritorio completo al proveedor. Es el end-to-end real, pero no sirve para lo que aquí se verifica, que es justo el caso **headless** — un proceso sin pantalla, como un broker en un VPS, que recibe la llamada y se la reenvía a su cliente.

Aquí el ejecutor de los casos locales es **simulado**: `OnExecuteAction` devuelve éxito sin hacer nada y `OnRequestScreenshot` entrega un PNG de 1×1. Eso basta para validar el round-trip completo (output + recursión) sin mover el ratón ni exponer la pantalla.

**Este demo no captura ni envía tu pantalla en ningún caso.**

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `ComputerUsePassthruDemo.dproj`, build Win64.

```bash
msbuild 082-ComputerUsePassthru/ComputerUsePassthruDemo.dproj /p:Config=Release /p:Platform=Win64
```

```bash
ComputerUsePassthruDemo.exe                      # los 6 casos
ComputerUsePassthruDemo.exe --case=delegated     # solo la delegación
ComputerUsePassthruDemo.exe --case=missing       # solo el aviso de componente ausente
ComputerUsePassthruDemo.exe --case=local         # solo el camino normal
ComputerUsePassthruDemo.exe --mode=sync          # sin el camino de streaming
ComputerUsePassthruDemo.exe --model=gpt-6-astra
ComputerUsePassthruDemo.exe --prompt="Abre el menu de inicio"
ComputerUsePassthruDemo.exe --help
```

### Requisitos

| Variable | Obligatoria | Nota |
|----------|-------------|------|
| `OPENAI_API_KEY` | sí | Hace llamadas **reales**. El modelo por defecto, `gpt-6-astra`, es de gama alta ($10/$50 por 1M) |

### Exit codes

| Código | Significado |
|--------|-------------|
| 0 | Todo lo verificable pasó |
| 1 | Algún caso falló |
| 2 | Falta `OPENAI_API_KEY` |
| 3 | Excepción no controlada |

**SKIP no es fallo.** El modelo decide si usa la herramienta; si en un turno contesta con texto y no emite ningún `computer_call`, no hay nada que verificar y el caso se marca SKIP.

## Lo que el demo enseña del framework

**El gate de intercepción.** El contrato para cualquier tool call es: el driver dispara `OnCallToolFunction` y, si el handler dejó `Response` lleno, **no** la ejecuta en local. Vive en la clase base (`TAiChat.DoCallFunction`) y lo respetan Claude, OpenAI y Gemini. Es el punto por el que un broker se queda con la llamada.

**La delegación de un `computer_call` es atómica.** Un `computer_call` de `gpt-6-astra` trae un **array `actions`** (p.ej. `keypress[WIN,r]` + `type 'notepad'` + `keypress[ENTER]`) y admite un **único** `computer_call_output`. Repartir sus acciones entre un ejecutor local y uno remoto dejaría el screenshot final sin dueño, así que la primera acción reclamada entrega el lote entero. El caso LOCAL del demo lo aprovecha al revés: deja pasar dos rondas reales y delega a partir de la tercera solo para cortar el bucle agéntico.

**Computer Use es opt-in.** Sin `cap_ComputerUse` el tool `computer` ni siquiera se declara. El demo lo activa con `ModelCaps`/`SessionCaps` directos; la vía por registry está comentada en el código:

```pascal
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-6-astra',
  'ModelCaps', '[cap_Image, cap_Reasoning, cap_ComputerUse]');
```

**`Asynchronous` es quien enciende el streaming** en el driver de OpenAI (`FClient.Asynchronous` → `stream: true`), no una propiedad `Stream`.

## Trampas encontradas al escribirlo

- **`IfThen` con enteros** vive en `System.Math` y el de cadenas en `System.StrUtils`. Mezclarlos da `E2250`; el demo usa el de cadenas y resuelve el entero con un `if`.
- **En asíncrono, `OnReceiveDataEnd` no es el final de todo.** El último tramo del turno sigue corriendo en su hilo, así que el demo espera el evento y añade una gracia de 3 s antes de leer el historial. Sin eso, se lee a medias.
- **El `.dpr` va en UTF-8 con BOM y CRLF**, como el resto del repo.

## Archivos

| Archivo | Contenido |
|---------|-----------|
| `ComputerUsePassthruDemo.dpr` | Todo el demo: spy de eventos, matriz de casos y veredictos |

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for the demos index and [../../CLAUDE.md](../../CLAUDE.md) for the project overview. El subsistema completo está documentado en [../../Source/Tools/CLAUDE.md](../../Source/Tools/CLAUDE.md); el end-to-end real con ratón y teclado está en [../066-ComputerUseTest/](../066-ComputerUseTest/).
