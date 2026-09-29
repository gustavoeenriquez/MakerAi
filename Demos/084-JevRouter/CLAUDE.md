# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 084 — **Enrutar consultas a agentes especializados con Jev** (TypeSafe AI). Consola. **Requiere `TYPESAFE_API_KEY`.**

Jev no es un LLM de chat: no genera texto. Responde preguntas tipadas (Choice / Score / Noul) con probabilidades calibradas. El demo lo usa como paso previo a los agentes: decide qué especialista atiende cada consulta y si ese agente necesita su versión con RAG, **antes** de gastar un LLM.

| Bloque | Qué muestra |
|--------|-------------|
| **A. Router de agentes** | Una sola llamada por consulta con `dominio` (Choice entre contable/tributario/legal/laboral/general), un Noul `toca_<dominio>` por área (detecta consultas que cruzan dos áreas) y `necesita_fuentes` (Noul: ¿hay que citar norma/tarifa/plazo?). Las reglas y el catálogo de agentes viven en código (`EnrutarConsulta`) |
| **B. Atajos** | `TAiJev.Choose` y `TAiJev.Noul` para una sola pregunta |
| **C. Dentro de un grafo** | Nodo `Recepcion` con `TAiJevRouterTool` → link `lmConditional` → un nodo por agente; `NextNo` → `humano`. `OnRoute` convierte `contable` en `contable_rapido` o `contable_normativo` según el flag `fuentes` |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `JevRouterDemo.dproj`, build Win64.

```bash
JevRouterDemo.exe                                  # consultas de ejemplo
JevRouterDemo.exe "consulta 1" "consulta 2"         # tus propias consultas
```

Exit code: 0 ok, 2 si falta la API key o hay excepción. Costo: ~700 tokens de entrada por consulta (US$0.042 por millón); las 8 consultas de ejemplo cuestan ~US$0.00025.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiJev`, `TAiJevQuestions`, `TAiJevResult` | `Source/Tools/uMakerAi.Jev.pas` |
| `TAiJevRouterTool` | `Source/Agents/uMakerAi.Agents.Tools.JevRouter.pas` |

## Notas

- **Resultado verificado (sep 27/2026, `jev-1.13.0`):** las 8 consultas se enrutan como se espera. "Registrar la nómina" va al contable **rápido** (fuentes 0.35) y "¿qué dice la NIC 16?" al contable **normativo** (fuentes 0.89); la venta de un activo con ganancia ocasional sale como contable + "consultar también: tributario".
- **Por qué no se le pasa a Jev la lista de agentes con sus capacidades para que "elija el mejor":** Jev juzga el *contenido* de la consulta, no conoce los agentes. Las descripciones de `DOMINIOS` son lo que más pesa en el acierto: escribirlas como se le explicarían a una recepcionista.
- **Choice + un Noul por opción no son redundantes** (lo recomienda la documentación de Jev): el Choice es relativo (cuál de todos), los Nouls son absolutos y son los que revelan que una consulta necesita a dos especialistas.
- El demo marca `(pide fuentes y no tiene RAG)` cuando la consulta necesita citar normas y el agente de su dominio no tiene RAG (el laboral, en el catálogo de ejemplo). Contar esos casos en producción dice a qué agente vale la pena agregarle RAG.
- **Bloque C coincide con el A** cuando la pregunta de ruta tiene la misma redacción. Con la genérica "¿Qué ruta o especialista debe atender…?" la nómina bajó a confianza 0.44 y la venta del activo a 0.43, y ambas cayeron a `humano`. La redacción mueve la confianza más que el umbral: fijarla primero y calibrar después.
- **Umbrales:** la confianza de una misma entrada varía unos puntos entre llamadas. `Model` queda fijo en `jev-1.13.0` (no `jev-latest`) y los umbrales tienen margen.
- Jev no hace aritmética, no cuenta y no compara fechas de forma fiable: eso va en código.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
