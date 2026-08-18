# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 073 — **Guardrails de tool calls y framework de evals**. Consola, determinista, **sin LLM ni API keys**.

Cubre los dos componentes que entraron en la fase 3 de v3.6 y que no tenían demo:

| Bloque | Qué muestra |
|--------|-------------|
| **A. Guardrails aislado** | `TAiGuardrails` con blocklist por comodín (`shell_*`), allowlist estricta, patrón prohibido en los argumentos (`DROP TABLE`) y veto programático vía `OnCheckToolCall` |
| **B. Integración real** | Un tool registrado en `TAiFunctions` que el guardrail bloquea: se comprueba que el handler **nunca se ejecuta** y que el LLM recibe el motivo en `ToolCall.Response` |
| **C. Evals** | `TAiEvalRunner` sobre un clasificador determinista, con `ExpectContains`/`ExpectEquals`/`ExpectNotContains`/`ExpectRegex`/`ExpectMaxLength`, reporte a consola y exit code |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `GuardrailsEvalsDemo.dproj`, build Win64.

```bash
GuardrailsEvalsDemo.exe                  # los tres bloques
GuardrailsEvalsDemo.exe --json rep.json  # ademas vuelca el reporte de evals
GuardrailsEvalsDemo.exe --otel           # trazas OTLP a localhost:4318
```

Exit code: 0 si todos los casos de eval pasan, 1 si alguno falla, 2 ante excepción.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiGuardrails` | `Source/Tools/uMakerAi.Guardrails.pas` |
| `TAiEvalRunner` / `TAiEvalReport` | `Source/Core/uMakerAi.Evals.pas` |
| `TAiFunctions` (choke point) | `Source/Tools/uMakerAi.Tools.Functions.pas` |

## Notas

- Los guardrails son **opt-in**: sin `TAiFunctions.Guardrails := <instancia>` no se aplica ninguna política.
- La política se consulta en `TAiFunctions.DoCallFunction`, que es el único choke point: cubre por igual tools locales, MCP y AutoMCP.
- Si `AllowedTools` no está vacía, manda ella y `BlockedTools` pasa a ser irrelevante.
- **Trampa que el demo hace explícita:** `AArguments` es el **JSON crudo** del modelo, no un valor parseado. Una ruta Windows llega escapada (`c:\\windows`), así que un filtro que compare contra `c:\windows` no dispara nunca y el guardrail queda silenciosamente inútil. El handler `GuardCheck` desescapa antes de comparar.
- Para LLM-as-judge basta asignar `Runner.Judge := <un TAiChat>` y usar `.ExpectJudge('criterio')`. Se omite aquí para no exigir API key.
- Los eventos del framework son `of object`: no aceptan lambdas, por eso los handlers viven en `TDemoHandlers`.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
