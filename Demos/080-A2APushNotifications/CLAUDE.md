# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 080 — **Push notifications de A2A**: el agente hace un POST a tu webhook cuando termina una tarea. Todo en un solo proceso y sin claves.

El streaming SSE sirve cuando el cliente puede quedarse conectado. Las push son para lo contrario: tareas largas en las que el cliente se desconecta y quiere que le avisen.

Levanta un agente A2A (8282) y un receptor de webhook propio (8283) para ver exactamente qué llega.

## ⚠️ Estado: el demo FALLA a propósito

**Este demo no pasa hoy**, y su exit code es 1. No es un error del demo: destapa una limitación real del framework.

| Qué | Estado |
|---|---|
| CRUD de configuraciones (`Create`/`Get`/`List`/`Delete`, idempotencia) | ✅ funciona |
| Registrar el webhook dentro de `SendMessage` (`configuration.taskPushNotificationConfig`) | ✅ se registra bien |
| **Entrega al webhook cuando nadie consulta el task** (`blocking:false`) | ❌ **no llega** |
| Entrega cuando el flujo es bloqueante o hay un resume | ✅ funciona (es lo que valida el TCK) |

**La causa:** la entrega se dispara al detectar un cambio de estado, y ese cambio solo se detecta cuando algo refresca el task. En los flujos bloqueantes lo hace `WaitTask`; con `blocking:false` no lo hace nadie. Se intentaron dos soluciones —un hilo que sondeaba los tasks vivos, y engancharse a `TAIAgentManager.OnFinish`— y **ninguna de las dos entregó** en este escenario. Queda pendiente de diagnóstico.

Ojo con la consecuencia práctica: **el TCK pasa `PUSH-DELIVER-*` porque su flujo es bloqueante**. La conformidad certificada no cubre este caso, y por eso hace falta este demo.

## Build & Run

```bash
A2APushDemo.exe [--port 8282] [--hook-port 8283]
```

Exit code 0 si llegaron los dos avisos, 1 si no.

## Lo que sí demuestra el demo

- Las dos formas de registrar un webhook, con su `authentication`.
- Que `Delete` es idempotente: borrar algo que ya no está no es error.
- El receptor imprime la cabecera `Authorization` y el cuerpo, así que sirve de banco para comprobar el formato: el payload es un **`StreamResponse`** (con el task dentro de `task`), no el Task desnudo.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiA2AServer` (CRUD de configs, `DeliverPushNotifications`) | `Source/Agents/uMakerAi.A2A.Server.pas` |

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview.
> See [../../Source/Agents/CLAUDE.md](../../Source/Agents/CLAUDE.md) for the A2A implementation.
