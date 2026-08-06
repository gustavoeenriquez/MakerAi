# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 074 — **Flujos de orquestación sobre A2A** (spec 1.0). Consola, determinista, **sin LLM ni API keys**.

El [demo 072](../072-A2AFederation/CLAUDE.md) muestra el camino feliz punto a punto. Este muestra lo que hace falta para que A2A sirva en una orquestación real:

| Escenario | Qué demuestra |
|-----------|---------------|
| **1. Pool** | Tres tasks **simultáneos** contra el mismo agente. Con un solo `TAIAgentManager` dos recibirían `AgentBusy` (-32000), porque un grafo no ejecuta dos corridas a la vez. Con `OnAcquireManager` el servidor mantiene un pool y los tres completan (~450 ms en vez de ~1200) |
| **2. Human-in-the-loop** | El grafo se suspende → task en `input-required` con la pregunta en `status.message` → un `SendMessage` con el **mismo `taskId`** lo reanuda hasta `completed` |
| **3. HITL federado** | Quien pide aprobación es el agente **remoto**: el nodo local se suspende en vez de fallar, y al reanudarlo la respuesta viaja al mismo task remoto |
| **4. No bloqueante** | `configuration.blocking=false` devuelve el task en `working` de inmediato; `GetTask` refresca el estado desde el grafo vivo |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `A2AOrchestrationDemo.dproj`, build Win64.

```bash
A2AOrchestrationDemo.exe          # los cuatro escenarios
A2AOrchestrationDemo.exe --otel   # trazas OTLP a localhost:4318
```

Usa el puerto 8281 (el demo 072 usa el 8280, así que pueden convivir).

## Key Source

| Componente | Unit |
|------------|------|
| `TAiA2AServer` (pool, task lifecycle, resume) | `Source/Agents/uMakerAi.A2A.Server.pas` |
| `TAiA2AClient` + `TAiA2ARemoteAgentTool` | `Source/Agents/uMakerAi.A2A.Client.pas` |
| `TAIAgentManager` (grafos, `Suspend`/`ResumeThread`) | `Source/Agents/uMakerAi.Agents.pas` |

## Notas

- **La fábrica es obligatoria para el pool.** Sin `OnAcquireManager` el límite efectivo sigue siendo 1, aunque `MaxConcurrentTasks` sea mayor: no hay forma de fabricar un segundo grafo equivalente. La fábrica debe devolver un grafo con el mismo shape; el servidor se hace cargo de liberarlo.
- **`ResumeThread` reejecuta el nodo suspendido**, no continúa después de él. Por eso `NodoAprobacion` marca en el blackboard si ya pasó, para distinguir la primera pasada de la reanudación. Es el error más fácil de cometer al escribir un nodo human-in-the-loop.
- `TAiA2ARemoteAgentTool.SuspendOnInputRequired` (default `True`) es lo que convierte un `input-required` remoto en una suspensión local. Con `False` el grafo local muere con excepción.
- El task remoto pendiente se recuerda en el blackboard bajo `A2A.<NombreNodo>.PendingTask`.
- Comparar estados con `LastTaskState` (enum), nunca con strings: el literal puede llegar como `TASK_STATE_COMPLETED` o `completed` según el agente.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
> See [../../Source/Agents/CLAUDE.md](../../Source/Agents/CLAUDE.md) for the A2A orchestration section.
