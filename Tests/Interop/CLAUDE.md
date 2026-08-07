# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**Banco de pruebas de interoperabilidad A2A** contra el SDK oficial [`a2a-sdk`](https://pypi.org/project/a2a-sdk/) de `a2aproject`.

La suite de regresión (`Tests/RegressionSuite`) valida el comportamiento del framework. Esto valida algo distinto: que **el formato de cable sea el de la spec**, no el que nosotros creemos que es.

> Esa distinción no es teórica. Durante meses los 9 casos A2A de la suite pasaron con el formato de respuesta **equivocado**, porque cliente y servidor de MakerAI compartían el mismo error. Un test que valida contra tu propia implementación demuestra que eres coherente contigo mismo, no que seas conforme.

## Cómo se ejecuta

```powershell
cd Tests\Interop
.\run_interop.ps1                # crea el venv si falta, instala a2a-sdk y corre todo
.\run_interop.ps1 -SkipInstall   # si el venv ya está listo
```

Exit code 0 = ambas direcciones conformes. Requiere Python 3.10+ y el demo 072 compilado en `Win64\Release`.

## Qué prueba

| Dirección | Qué valida |
|---|---|
| **1.** Nuestro cliente → servidor de referencia | Que sabemos **leer** lo que emite un agente v1.0 |
| **2.** Cliente conforme → nuestro servidor | Que lo que **emitimos** parsea como v1.0 |

La clave está en cómo se valida: `ref_server.py` construye las respuestas con los tipos **protobuf del SDK** y las serializa con `json_format.MessageToJson`, así que el JSON es el canónico. Y `ref_client.py` valida lo nuestro con `json_format.ParseDict` **sin** `ignore_unknown_fields`: cualquier campo de más o mal nombrado hace fallar el parseo.

## Archivos

| Archivo | Qué es |
|---|---|
| `ref_server.py <puerto>` | Servidor A2A conforme; serializa con los tipos del SDK |
| `ref_client.py <url>` | Cliente que valida card y respuestas contra los tipos protobuf |
| `run_interop.ps1` | Orquesta ambas direcciones y devuelve exit code |
| `.venv/` | Entorno virtual (no versionado) |

## El SDK 1.x es proto-first

Los tipos de `a2a.types` son **protobuf**, no Pydantic, y el formato de cable es **protojson**. Consecuencias prácticas:

- `TaskState` es un `EnumTypeWrapper`: se recorre con `.keys()`, no iterando.
- Los enums viajan como `TASK_STATE_COMPLETED` / `ROLE_AGENT` (SCREAMING_SNAKE_CASE), no en kebab-case como en la era 0.x.
- Los campos `snake_case` del proto salen en `camelCase` en el JSON.

Para inspeccionar la forma canónica de cualquier mensaje:

```python
import a2a.types as T
from google.protobuf import json_format
print([f.name for f in T.SendMessageResponse.DESCRIPTOR.fields])   # ['task', 'message']
```

## Hallazgos que produjo este banco (agosto 2026)

Ver `Source/Agents/CLAUDE.md` para el detalle. En resumen, dos fallos reales que ningún test interno podía ver:

1. `SendMessage` devolvía el Task **sin envolver**; v1.0 exige `{"result": {"task": {...}}}`. `GetTask`/`CancelTask` sí van directos.
2. La Agent Card publicaba `url`/`protocolVersion`/`preferredTransport` en la raíz, que en v1.0 **no existen**: van en `supportedInterfaces[]`.

Corregidos con la propiedad `TAiA2AServer.WireEra` y `TAiA2AClient.UnwrapSendMessageResult`.

## Navigation

> See [../RegressionSuite/CLAUDE.md](../RegressionSuite/CLAUDE.md) for the framework regression suite.
> See [../../Source/Agents/CLAUDE.md](../../Source/Agents/CLAUDE.md) for the A2A implementation.
