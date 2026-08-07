# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**Suite de regresión de MakerAI** — la red de seguridad del framework. Valida los subsistemas críticos **in-process**, sin servicios externos, sin API keys y sin depender de demos compilados: levanta sus propios servidores MCP y A2A en puertos altos (18790-18793) y los apaga al terminar.

Está construida sobre `TAiEvalRunner` (`Source/Core/uMakerAi.Evals.pas`), así que es a la vez la suite de tests y el ejemplo canónico de uso del componente de evals.

## Build & Run

```bash
msbuild MakerAiRegressionSuite.dproj /p:Config=Release /p:Platform=Win64
```

```bash
# Ejecutar (exit code 0 = todo verde, 1 = fallos, 2 = error no controlado)
Win64\Release\MakerAiRegressionSuite.exe

# Reporte JSON para CI
Win64\Release\MakerAiRegressionSuite.exe --json report.json

# Con trazas OpenTelemetry (collector OTLP en localhost:4318)
Win64\Release\MakerAiRegressionSuite.exe --otel
```

Duración típica: < 1 segundo.

## Cobertura actual (32 casos)

| Área | Casos |
|------|-------|
| MCP dual-era | negociación moderna (2026-07-28), fallback a handshake legacy, `tools/list`, `tools/call` |
| MCP MRTR | reintento con `accept`, mensaje de elicitation recibido, sin handler → error explícito |
| Agentes | grafo secuencial con status final y salida encadenada |
| A2A 1.0 | Agent Card, `SendMessage` → `TASK_STATE_COMPLETED`, federación (grafo local → agente remoto) |
| A2A orquestación | pool con 3 tasks simultáneos, human-in-the-loop con resume por `taskId`, human-in-the-loop federado (suspensión del nodo local), `blocking=false` + `GetTask`, tolerancia de literales de estado, cancelar task terminal → `-32002` |
| Guardrails | blocklist con comodín, allowlist estricta, patrón prohibido en argumentos, veto programático, integración real (el tool bloqueado NO se ejecuta) |
| A2A streaming | reanudar un task en `input-required` con `SendStreamingMessage` (SSE crudo) |
| A2A Agent Card | skills declaradas (con tags) y skill `run-graph` por defecto cuando no hay ninguna |
| RAG | búsqueda con `Options` en nil sobre el driver `.mkai` (regresión del AV por `IfThen`) |
| Evals | autoprueba del runner (conteo PASS/FAIL) |

## Estructura

| Archivo | Contenido |
|---------|-----------|
| `MakerAiRegressionSuite.dpr` | Programa principal: CLI (`--json`, `--otel`), ejecución y exit code |
| `uRegression.Suites.pas` | Definición de los casos (`DefineCases`) y el *dispatcher* que ejecuta cada escenario contra los componentes reales |
| `uRegression.Fixtures.pas` | Tools MCP de prueba (`echo_upper`, `confirm_op` con MRTR), servidor MCP "solo legacy" (responde `-32601` a `server/discover`) y handlers `of object` (incluye `NodeSuspendOnce` para human-in-the-loop y `AcquireManager` como fábrica del pool A2A) |

Los escenarios A2A de orquestación viven en `RunA2AFlowScenario`, aparte del bloque `a2a:` básico, porque cada uno arma su propia topología (pool, suspensión, no bloqueante).

## Cómo agregar un caso

1. En `uRegression.Suites.pas` → `DefineCases`, declarar el caso con su escenario y expectativas:
   ```pascal
   FRunner.AddCase('area.subarea.detalle')
     .Input('area:escenario')
     .ExpectContains('...');
   ```
2. En el `Run<Area>Scenario` correspondiente, implementar el escenario devolviendo un string con el resultado observable.
3. Si el escenario necesita un tool o servidor nuevo, agregarlo a `uRegression.Fixtures.pas`.

Convenciones: nombres de caso en `area.subarea.detalle`; los escenarios devuelven strings compactos (`'estado|salida'`) para poder afirmar con `ExpectEquals`.

## Notas

- Los eventos del framework son `of object` (sin lambdas): los handlers viven en `TFixtureHandlers`.
- Cada caso emite un span `eval.case <nombre>` cuando se corre con `--otel`, útil para ver la suite completa en Jaeger/Langfuse.
- La suite no requiere claves de API. Si en el futuro se agregan casos que llamen a proveedores reales, deben omitirse (skip) cuando falte la variable de entorno correspondiente.

## Navigation

> See [../../CLAUDE.md](../../CLAUDE.md) for project overview.
