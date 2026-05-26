# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Console demo que demuestra la **ejecución durable de agentes** (human-in-the-loop) usando `TAiDatabaseCheckpointer` — la implementación FireDAC del contrato `IAiCheckpointer`. El estado del agente persiste en SQLite entre ejecuciones, permitiendo suspender y reanudar flujos que requieren aprobación humana.

## Build Commands

```bash
# Debug Win64
msbuild AgentCheckpointDBDemo.dproj /p:Config=Debug /p:Platform=Win64

# Release Win64
msbuild AgentCheckpointDBDemo.dproj /p:Config=Release /p:Platform=Win64
```

Output: `.\Win64\Debug\AgentCheckpointDBDemo.exe`  
BD SQLite generada junto al exe: `AgentCheckpointDBDemo.db`

## Architecture

### Flujo del Agente

```
Inicio -> Procesar -> Aprobar (SUSPEND) -> Finalizar
```

| Nodo | Rol |
|------|-----|
| `Inicio` | Recibe el input inicial |
| `Procesar` | Simula trabajo (Sleep 400 ms) |
| `Aprobar` | Llama `Node.Suspend()` — requiere aprobación humana |
| `Finalizar` | Se ejecuta tras la reanudación |

### Patrón Human-in-the-Loop

**Fase 1 — Primera ejecución:**
1. `FAgents.Run('Solicitud-2026-001')` arranca el grafo en `TThreadPool`
2. El nodo `Aprobar` ejecuta `Node.Suspend(Reason, Context)` — el framework:
   - Serializa el estado completo en `TAiCheckpointSnapshot`
   - Llama a `TAiDatabaseCheckpointer.SaveCheckpoint(ThreadID, snapshot)` → SQLite
   - Dispara el evento `OnSuspend` con el `ThreadID`
3. El agente pasa a estado `esSuspended`; `Busy` regresa a `False`
4. El demo muestra el `ThreadID` y espera `[ENTER]`

**Fase 2 — Reanudación:**
1. `FAgents.ResumeThread(ThreadID, 'Finalizar', 'Aprobado')` carga el snapshot desde SQLite y reanuda desde el nodo `Finalizar`
2. Al completar, `TAiDatabaseCheckpointer.DeleteCheckpoint(ThreadID)` elimina la fila

**Detección de threads previos:**  
Al iniciar, el demo llama `FCheckpointer.GetActiveThreadIDs` — si hay checkpoints en la BD (de ejecuciones previas interrumpidas), ofrece reanudarlos directamente.

### Clases Clave

**`TAiDatabaseCheckpointer`** (`Source/Agents/uMakerAi.Agents.Checkpoint.DB.pas`):
- Implementa `IAiCheckpointer` sobre `TFDConnection` (cualquier driver FireDAC)
- UPSERT genérico: `DELETE WHERE thread_id = :tid` + `INSERT` (compatible con SQLite, PostgreSQL, Firebird…)
- Thread-safe: `TCriticalSection` protege las operaciones DB; la serialización JSON ocurre fuera del lock
- `CreateSchema` crea la tabla si no existe (llamar una vez al inicio)

```pascal
// Tabla generada por CreateSchema:
// agent_checkpoints (
//   thread_id     VARCHAR(255) PRIMARY KEY,
//   snapshot      TEXT NOT NULL,       -- JSON de TAiCheckpointSnapshot
//   checkpoint_id INTEGER NOT NULL,
//   created_at    TIMESTAMP NOT NULL,
//   updated_at    TIMESTAMP NOT NULL
// )
```

**`IAiCheckpointer`** (`Source/Agents/uMakerAi.Agents.Checkpoint.pas`):
```pascal
IAiCheckpointer = interface
  procedure SaveCheckpoint(const AThreadID: string; ASnapshot: TAiCheckpointSnapshot);
  function  LoadCheckpoint(const AThreadID: string): TAiCheckpointSnapshot;
  function  GetActiveThreadIDs: TArray<string>;
  procedure DeleteCheckpoint(const AThreadID: string);
end;
```

### Inicialización del Checkpointer

```pascal
var
  LCp: TAiDatabaseCheckpointer;
begin
  LCp := TAiDatabaseCheckpointer.Create(FDConnection1);
  LCp.CreateSchema;                    // una vez al inicio
  FCheckpointer := LCp;               // IAiCheckpointer toma ownership via refcount
  FAgents.Checkpointer := FCheckpointer;
end;
```

> **Lifetime:** `TAiDatabaseCheckpointer` es `TInterfacedObject`. Guardar siempre
> en una variable `IAiCheckpointer` — nunca liberar manualmente. El caller gestiona
> el ciclo de vida de `TFDConnection`.

### Eventos del Agente

| Evento | Tipo | Firma |
|--------|------|-------|
| `OnPrint` | `TAIAgentsOnPrint` | `(Sender: TObject; Value: string)` |
| `OnEnd` | `TAIAgentsOnEnd` | `(Node: TAIAgentsNode; Value: string)` |
| `OnError` | `TAIAgentsOnError` | `(Sender, Node, Link, E; var Abort)` |
| `OnSuspend` | `TAIAgentsOnSuspend` | `(Sender; const AThreadID, ANodeName, AReason, AContext)` |

> **Nota:** `TAIAgents` es un alias deprecated — usar `TAIAgentManager` directamente.

### Threading

- Nodos ejecutan en `TThreadPool`; los handlers de evento llaman `TThread.Synchronize` para escribir en consola
- `WaitForCompletion` hace polling de `FAgents.Busy` con `CheckSynchronize` + `Sleep(50)` para procesar las llamadas sincronizadas

## Dependencies

| Paquete | Razón |
|---------|-------|
| `MakerAI.bpl` | `uMakerAi.Agents`, `uMakerAi.Agents.Checkpoint` |
| `FireDAC.bpl` | `TFDConnection`, `TFDQuery` |
| `FireDACCommonDriver.bpl` | Driver base FireDAC |
| `FireDACSqliteDriver.bpl` | Driver SQLite |
| `FireDAC.ConsoleUI.Wait` | Wait dialog para consola (evita dialog VCL) |

Library paths requeridas (en IDE o MSBuild):
- `../../Source/Agents`
- `../../Source/Chat`
- `../../Source/Core`

## Extending to Other Databases

Para usar PostgreSQL en lugar de SQLite, cambiar solo la configuración de `TFDConnection`:

```pascal
FConnection.DriverName := 'PG';
FConnection.Params.Values['Server']   := 'localhost';
FConnection.Params.Values['Database'] := 'mydb';
FConnection.Params.Values['User_Name']:= 'postgres';
FConnection.Params.Values['Password'] := 'secret';
```

El `TAiDatabaseCheckpointer` funciona igual — el DDL usa `CREATE TABLE IF NOT EXISTS` y parámetros estándar SQL-92.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
