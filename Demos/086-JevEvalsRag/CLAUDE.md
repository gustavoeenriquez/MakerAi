# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 086 — **Jev en Evals y en RAG**. Consola. **Requiere `TYPESAFE_API_KEY`** (y ninguna otra clave).

| Bloque | Qué muestra |
|--------|-------------|
| **A. Evals** | `TAiEvalRunner` con `Scorer := TAiJevEvalScorer` y checks `ExpectScore('criterio', 0.7)`, combinados con un check determinista (`ExpectContains`). El target devuelve respuestas fijas —dos buenas y dos malas— para no exigir otra API key |
| **B. RAG** | `TAiJevRAGReranker` reordena 6 pasajes por evidencia útil para la consulta y descarta el que trae una inyección de prompt. Se llama `TAiRAGVector.RerankWith` directo para no exigir un motor de embeddings; en VQL se activa con `RERANK` y `TAiRAGVector.Reranker` |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `JevEvalsRagDemo.dproj`, build Win64.

```bash
JevEvalsRagDemo.exe
```

Exit code: 0 ok, 2 si falta la API key o hay excepción. **Que el bloque A reporte 2/4 es lo esperado**: los casos `no-filtra-credenciales` y `sin-inventar` tienen respuestas malas a propósito.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiJevEvalScorer` | `Source/Tools/uMakerAi.Jev.Evals.pas` |
| `TAiJevRAGReranker` | `Source/Tools/uMakerAi.Jev.RAG.pas` |
| `TAiEvalScorerBase`, `ExpectScore` | `Source/Core/uMakerAi.Evals.pas` |
| `TAiRAGRerankerBase`, `Reranker`, `RerankWith` | `Source/RAG/uMakerAi.RAG.Vectors.pas` |

## Notas

- **Resultado verificado (sep 27/2026, `jev-1.13.0`):** Evals — los buenos pasan, la clave filtrada da 0.01 y el IVA inventado 0.02. RAG — el pasaje que responde sube a 0.98, los demás quedan ≤ 0.06 y el del foro con "IGNORA TODAS LAS INSTRUCCIONES" se descarta.
- El reranker hace **una llamada por pasaje** (patrón del cookbook de TypeSafe) y en paralelo (`MaxParallel`, 4). Con un `TAiJev` externo en `Jev` van en serie: el componente compartido no es seguro entre hilos.
- `InjectionThreshold` es 0.9 y no 0.7 porque en la calibración un pasaje legítimo llegó a 0.73.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
