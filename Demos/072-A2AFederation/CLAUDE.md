# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 072 — **Federación de agentes con el protocolo A2A** (Agent-to-Agent, Linux Foundation, spec 1.0). Demuestra el stack A2A completo de MakerAI **sin LLM ni API keys** (los nodos son transformaciones de texto deterministas):

1. **Servidor**: un grafo `TAIAgentManager` ("Agente Editor": Limpiar → Firmar) expuesto como agente A2A vía `TAiA2AServer` — Agent Card en `/.well-known/agent-card.json` y JSON-RPC `SendMessage`/`GetTask`/`CancelTask`.
2. **Cliente**: `TAiA2AClient` descubre la Agent Card y envía un mensaje (`SendText` → task con artifacts).
3. **Federación**: un grafo local ("Agente Redactor": Preparar → Editar → Publicar) delega el nodo `Editar` en el agente remoto usando `TAiA2ARemoteAgentTool` asignado como `Tool` del nodo.

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `A2AFederationDemo.dproj`, build Win64.

```bash
# Demo completo en un solo proceso (servidor + cliente + federacion)
A2AFederationDemo.exe

# Solo el servidor A2A (para consumirlo desde otro proceso/maquina)
A2AFederationDemo.exe --serve --port 8280

# Solo el cliente contra un agente remoto
A2AFederationDemo.exe --client --url http://localhost:8280 "texto a editar"

# Con trazas OpenTelemetry (collector OTLP en localhost:4318)
A2AFederationDemo.exe --otel
```

Verificar la Agent Card con curl:

```bash
curl http://localhost:8280/.well-known/agent-card.json
```

## Key Source

| Component | Unit |
|-----------|------|
| `TAiA2AServer` | `Source/Agents/uMakerAi.A2A.Server.pas` |
| `TAiA2AClient` + `TAiA2ARemoteAgentTool` | `Source/Agents/uMakerAi.A2A.Client.pas` |
| `TAIAgentManager` (grafos) | `Source/Agents/uMakerAi.Agents.pas` |
| `TAiTelemetry` (--otel) | `Source/Core/uMakerAi.Telemetry.pas` |

## Notas

- Los estados del task siguen la spec 1.0 (`TASK_STATE_COMPLETED`, `TASK_STATE_INPUT_REQUIRED`...). Una suspensión del grafo (`Node.Suspend`, human-in-the-loop) se publica como `INPUT_REQUIRED`.
- El servidor acepta también los aliases de método de la era 0.x (`message/send`, `tasks/get`, `tasks/cancel`).
- Streaming/push notifications no soportados en el MVP (declarados `false` en la card; `SendStreamingMessage` responde `UnsupportedOperationError`).

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
