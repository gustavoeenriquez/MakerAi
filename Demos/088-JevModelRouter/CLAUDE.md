# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 088 — **Enrutador de modelos con Jev**: cada petición al modelo más barato que alcanza. Consola. **Requiere `TYPESAFE_API_KEY`, `GROQ_API_KEY`, `DEEPSEEK_API_KEY` y `CLAUDE_API_KEY`.**

| Nivel | Tier | Modelo |
|-------|------|--------|
| 0 trivial | rapido | Groq `openai/gpt-oss-20b` |
| 1 estándar | estandar | DeepSeek `deepseek-v4-flash` |
| 2 exigente / sensible | exigente | Claude `claude-sonnet-5` |
| 3 experto | experto | Claude `claude-opus-5` |

| Bloque | Qué muestra |
|--------|-------------|
| **A. Peticiones sueltas** | 8 peticiones de nivel 0 a 3: cada una la responde el modelo elegido y `TAiJevEvalScorer` mide si la respuesta es correcta y útil (la prueba de calidad del enrutamiento) |
| **B. Conversación que escala** | Turno trivial (Groq) y turno experto (Claude) en la misma `TAiChatConnection`: el historial se migra entre proveedores y el modelo nuevo debe recordar el primer turno |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `JevModelRouterDemo.dproj`, build Win64.

```bash
JevModelRouterDemo.exe
```

Tarda ~2–3 minutos (Sonnet y Opus). Exit code: 0 ok, 2 si falta la API key de Jev o hay excepción; los errores de un proveedor se imprimen como `ERROR:` en la petición afectada.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiJevModelRouter`, `TAiJevModelTiers`, `TAiModelRoute` | `Source/Tools/uMakerAi.Jev.ModelRouter.pas` |
| `TAiJevEvalScorer` (medir la calidad) | `Source/Tools/uMakerAi.Jev.Evals.pas` |

## Notas

- **Resultado verificado (sep 28/2026):** 7 de 8 respuestas útiles (calidad ≥ 0.70). Nivel 0 en Groq 0.4–0.6 s (0.86 y 0.99), nivel 1 en DeepSeek 3.7–5.6 s (0.94–0.96, incluido el access violation que la calibración dejó un nivel abajo), Sonnet ~37 s, Opus 41 s (0.84). La de 0.57 (PostgreSQL vs SQL Server en Sonnet) muy probablemente quedó incompleta por el tope global `Max_Tokens=1200`. Bloque B: Groq → Claude Sonnet con 4 mensajes migrados; la respuesta partió de "tu ferretería mayorista de tornillos" (Jev 0.92).
- **Groq retiró los `llama-3.x`** (`model_not_found`) aunque siguen registrados en `uMakerAi.Chat.Initializations`; sus modelos de chat activos son `openai/gpt-oss-20b/120b` y `qwen/qwen3.6|3.8-27b`.
- **Claude necesita `Params` propios en el tier** (`ThinkingLevel=tlLow`, `SessionCaps=[cap_Image, cap_Pdf]`): con los defaults (razonamiento adaptativo + búsqueda web) Opus superó el timeout HTTP (~300 s, WinHTTP 12002) en la pregunta de arquitectura; así respondió en 41–91 s.
- `ConnectionParams` (global) y `Tier.Params` se aplican **después** de asignar `Model`: asignarlo recarga los defaults del modelo (un `Max_Tokens` aplicado antes terminaba en 65536).

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
