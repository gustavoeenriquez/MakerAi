# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**Pruebas de integración**: consolas con asserts que verifican un fix o un subsistema de punta a punta, contra el API real de un proveedor o contra un servidor real levantado en el mismo proceso. Hasta nov 6/2026 vivían en `Demos/` (con el mismo número), pero no enseñan a usar el framework: comprueban que algo sigue funcionando.

No confundir con las otras dos carpetas de `Tests/`:

| Carpeta | Qué valida | Claves |
|---|---|---|
| `Tests/RegressionSuite` | El comportamiento del framework, in-process, ~5 s. **Correr antes de cada release** | Ninguna |
| `Tests/Interop` | Que el formato de cable A2A sea el de la spec (contra el SDK oficial en Python) | Ninguna |
| `Tests/Integration` (esta) | Un issue o subsistema contra el API real | Las del proveedor (salvo 069) |

## Proyectos

| Proyecto | Verifica | Requiere |
|---|---|---|
| `014-ChatTest/ClaudeChatTest` | Banco de pruebas de drivers. **Ojo: su `begin..end` principal NO prueba Claude**: corre `code_execution` con PNG contra el driver de MakerAI (`mk-gpt-oss-20b`, `mk-claude-sonnet`), que va al servidor de producción; `--groq` corre `code_interpreter` en Groq (`openai/gpt-oss-20b`). Las secciones de Claude (PDF, JSON, JSON Schema, web search, documentos) existen como procedimientos, pero no se invocan | Clave de MakerAI o `GROQ_API_KEY` |
| `068-AsyncHistoryTest` | Issue #105: en asíncrono Claude archiva sus respuestas en el historial; conversación de 3 turnos donde el último debe recordar los anteriores. `claude-haiku-5-5`; PASS nov 6/2026 | `CLAUDE_API_KEY` |
| `069-McpSessionGateTest` | Issue #110: vetting de cliente MCP (`OnClientConnect`) y gate de sesión HTTP por `Mcp-Session-Id`. Levanta un `TAiMCPHttpServer` real y lo prueba como cliente en 4 fases. 18/18 PASS nov 6/2026 | Ninguna (in-process) |

Siguen en `Demos/` dos consolas del mismo tipo porque el libro las cita por su ruta: `066-ComputerUseTest` (mueve el ratón de verdad) y `067-PromptCacheTest` (prompt caching de Claude).

## Compilar y correr

```bash
msbuild Tests/Integration/IntegrationTests.groupproj /t:Build /p:Config=Release /p:Platform=Win64
Tests/Integration/069-McpSessionGateTest/Win64/Release/McpSessionGateTest.exe   # exit 0 = OK
Tests/Integration/068-AsyncHistoryTest/Win64/Release/AsyncHistoryTest.exe
```

Los `.dproj` buscan el código en `..\..\..\Source\...` (un nivel más que en `Demos/`). Si se agrega un proyecto aquí, ajustar esa ruta y sumarlo a `IntegrationTests.groupproj`.

Gastan dinero real (salvo 069): usar modelos baratos y no meterlas en un bucle.

## Navigation

> See [../RegressionSuite/CLAUDE.md](../RegressionSuite/CLAUDE.md) for the regression suite and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
