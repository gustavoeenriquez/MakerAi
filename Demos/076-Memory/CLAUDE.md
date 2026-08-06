# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 076 — **`TAiMemory`: memoria semántica persistente** sobre SQLite (FTS5). Consola.

Un chat sin memoria arranca de cero en cada sesión. `TAiMemory` guarda hechos, preferencias y decisiones, y los recupera después por relevancia.

| Paso | Qué muestra |
|------|-------------|
| **1. Store** | Memorias con tipo (`mt_Fact`, `mt_Preference`, `mt_Decision`, `mt_ErrorFix`, `mt_Pattern`, `mt_Summary`), importancia 1-10, tags CSV y TTL opcional |
| **2. Search** | Búsqueda con score y `MatchType` (`fts` / `semantic` / `hybrid`) |
| **3. Recall** | Lo importante sin consulta previa — para arrancar una sesión nueva |
| **4. Context** | Bloque de texto listo para inyectar en el system prompt, con presupuesto de tokens |
| **5. Stats** | Estado del namespace |
| **6. Persistencia** | Se cierra la base, se reabre y se comprueba que todo sigue |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `MemoryDemo.dproj`, build Win64.

```bash
MemoryDemo.exe          # base temporal, se borra al terminar
MemoryDemo.exe --keep   # conserva el .db para inspeccionarlo
MemoryDemo.exe --otel   # trazas OTLP a localhost:4318
```

**Sin API key** funciona en modo `ms_FTS` (búsqueda léxica sobre SQLite). **Con `OPENAI_API_KEY`** definida engancha un `TAiOpenAiEmbeddings` y pasa a `ms_Hybrid`: FTS + semántica fusionadas con RRF.

La diferencia se ve en la tercera consulta del demo: *"problema con un cobro"* no comparte ninguna palabra con *"Un cobro duplicado se reclama por el formulario de facturación"*… salvo "cobro". En modo híbrido la encuentra por significado y la puntúa primera.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiMemory` | `Source/Memory/uMakerAi.Memory.pas` |
| `TMemoryType`, `TMemorySearchMode`, `TMemoryEntry`, `TMemoryStats` | `Source/Memory/uMakerAi.Memory.Types.pas` |
| Almacenamiento SQLite | `Source/Memory/uMakerAi.Memory.Storage.pas` |
| Embeddings | `Source/Embeddings/uMakerAi.Embeddings.OpenAi.pas` |

## Notas

- **App de consola: hay que enlazar FireDAC explícitamente.** Sin `FireDAC.Stan.Def`, `FireDAC.Phys.SQLite`, `FireDAC.Phys.SQLiteDef`, `FireDAC.Stan.Async`, `FireDAC.Stan.ExprFuncs` y `FireDAC.DApt` en el `uses`, `TAiMemory` falla al abrir la base con *"Object factory for class {...} is missing"*. En una app VCL/FMX los enlaza el diseñador.
- `Store` **deduplica por hash de contenido** dentro del namespace: repetir el mismo texto actualiza en vez de duplicar.
- `Namespace` aísla conjuntos de memorias en la misma base (por usuario, por proyecto…).
- `Embedder` es opcional. Sin él las entradas se guardan sin vector y `ms_Semantic`/`ms_Hybrid` degradan a FTS. Si se añade un embedder más tarde, `BackfillEmbeddings` rellena las que faltan.
- El TTL (`ATtlDays`) es para contexto de una sola sesión; `CleanupExpired` y `Prune` hacen la limpieza.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
