# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 080 — **Push notifications de A2A**: el agente hace un POST a tu webhook cuando termina una tarea. Todo en un solo proceso y sin claves.

El streaming SSE sirve cuando el cliente puede quedarse conectado. Las push son para lo contrario: tareas largas en las que el cliente se desconecta y quiere que le avisen.

Levanta un agente A2A (8282) y un receptor de webhook propio (8283) para ver exactamente qué llega.

## Por qué existe este demo

Cubre justo lo que la conformidad certificada **no** cubre: **el TCK pasa `PUSH-DELIVER-*` con un flujo bloqueante**, así que nunca comprueba el caso interesante — que el webhook reciba el aviso con `blocking:false`, sin que nadie consulte el task. Este demo lo verifica de punta a punta y devuelve exit code 1 si no llega.

Escribirlo destapó dos fallos reales, uno en cada lado:

1. **En el framework.** `TAIAgentManager` dispara `OnFinish` y solo *después* pone `FBusy` a 0. El servidor A2A se engancha a ese evento para refrescar el task, pero dentro del handler el manager todavía se declaraba ocupado: `RefreshTask` se iba por la rama "sigue trabajando" y nunca veía el paso a `completed`. La entrega no se disparaba jamás. Arreglado con el parámetro `AIgnoreBusy`.
2. **En el receptor de este demo.** `TIdHTTPServer` solo entiende autenticación `Basic`. Al llegar un `Authorization: Bearer …` respondía **401 él solo**, sin llegar a `OnCommandGet`. El POST salía bien del framework y moría en la puerta. Se arregla manejando `OnParseAuthentication` y poniendo `VHandled := True` — trampa clásica de Indy, la misma que ya apareció en el demo 037.

Vale la pena quedarse con el detalle: durante un buen rato el síntoma ("el webhook no recibe nada") apuntaba entero al framework, y la mitad del problema estaba en el receptor. El diagnóstico solo se cerró al imprimir el **código de estado** del POST: sin excepción de transporte, un 401 se parece mucho a un envío correcto.

## Build & Run

```bash
A2APushDemo.exe [--port 8282] [--hook-port 8283]
```

Exit code 0 si llegaron los dos avisos, 1 si no. Hoy pasa (exit 0).

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
