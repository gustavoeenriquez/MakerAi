# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 079 — **Agente SUT conforme al TCK oficial de A2A**. Es el System Under Test con el que se certifica la implementación, y a la vez el ejemplo de cómo emitir **artifacts estructurados** desde un grafo.

Resultado actual contra [`a2aproject/a2a-tck`](https://github.com/a2aproject/a2a-tck): **MUST 67/67, SHOULD 4/4**.

## Build & Run

```bash
A2ASutAgentDemo.exe [--port 9999]
```

Y en otra consola, el TCK:

```bash
python run_tck.py --sut-host http://localhost:9999 --level must
python run_tck.py --sut-host http://localhost:9999 --level should
```

## El contrato del TCK

El TCK necesita que el agente implemente escenarios concretos para poder probar ciertas cosas. **Despacha por el prefijo del `messageId`, no por el texto** — eso es lo primero que hay que entender:

| `messageId` empieza por | El agente debe |
|---|---|
| `tck-artifact-text` | artifact con TextPart `"Generated text content"` |
| `tck-artifact-data` | artifact con DataPart `{"key":"value","count":42}` |
| `tck-artifact-file` | artifact con FilePart de bytes (`raw` en base64 + `filename`) |
| `tck-artifact-file-url` | artifact con FilePart por `url` |
| `tck-message-response` | responder con un **Message**, no con un Task |
| `tck-input-required` | dejar el task en `input-required` |
| `tck-complete-task` | completar con un mensaje de agente |

> Ojo al orden de comparación: `tck-artifact-file-url` empieza por `tck-artifact-file`, así que la variante más específica va primero. Es un error fácil de cometer y se manifiesta como un test de URL que devuelve bytes.

## Cómo se emiten artifacts estructurados

El canal es el **blackboard**. El servidor siembra antes de correr el grafo:

| Clave | Contenido |
|---|---|
| `A2A.MessageId` | `messageId` del mensaje entrante |
| `A2A.TaskId` | id del task |
| `A2A.ContextId` | id del contexto |

Y lee al terminar:

| Clave | Efecto |
|---|---|
| `A2A.Artifacts` | Array JSON de artifacts; se emite **tal cual** en vez del artifact de texto |
| `A2A.Message` | Objeto Message; `SendMessage` responde `{"message": ...}` en vez de `{"task": ...}` |

```pascal
B := Node.Graph.Blackboard;
if B.GetString(A2A_BB_MESSAGE_ID).StartsWith('tck-artifact-data') then
  B.SetString(A2A_BB_ARTIFACTS, NuevoArtifact(TaskId + '-a', 'output',
    PartDatos('{"key": "value", "count": 42}')));
```

Sin esto un agente MakerAI solo podía devolver texto plano. Las constantes están en `uMakerAi.A2A.Server`.

## La estructura de Part en v1.0

No existen `TextPart`/`DataPart`/`FilePart` como tipos distintos: hay **un único `Part`** y el contenido lo determina qué campo está presente.

| Campo | Para |
|---|---|
| `text` | texto |
| `data` | objeto JSON |
| `raw` | bytes, **en base64** (es cómo ProtoJSON serializa `bytes`) |
| `url` | referencia a fichero externo |

Más `mediaType` y `filename`, opcionales.

## El pool no es opcional aquí

El TCK lanza peticiones concurrentes. Con un único `TAIAgentManager` la segunda recibe `AgentBusy` (-32000) y revienta hasta la validación del envelope JSON-RPC, que aparentemente no tiene nada que ver. Por eso el demo asigna `OnAcquireManager` y `MaxConcurrentTasks := 8`.

Fue exactamente el último fallo que quedaba: un síntoma engañoso (`JSONRPC-FMT-001`) cuya causa era la concurrencia.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiA2AServer` (pool, artifacts, card cacheable) | `Source/Agents/uMakerAi.A2A.Server.pas` |
| Constantes `A2A_BB_*` | idem |
| `TAIAgentManager`, `TAIBlackboard` | `Source/Agents/uMakerAi.Agents.pas` |

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview.
> See [../../Tests/Interop/CLAUDE.md](../../Tests/Interop/CLAUDE.md) for the interop bench.
> See [../../Source/Agents/CLAUDE.md](../../Source/Agents/CLAUDE.md) for the A2A implementation.
