# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Demo 081 - AgentChatHITL

Human-in-the-loop de agentes **integrado en el chat**, sin ventanas modales ni threads bloqueados. Responde a la pregunta recurrente "¿cómo hago que el agente pida algo al usuario sin una ventana síncrona?" — el patrón funciona igual en Windows, Android y Linux porque nada queda en espera.

**No requiere API keys**: los nodos hacen trabajo determinístico para que el flujo de suspensión/reanudación sea visible. En una app real, `Preparar`/`Ejecutar` usarían `TAiChatConnection` u otras tools.

## Flujo

```text
Usuario escribe en TAIChatInput
  └─ Run(prompt)                      (async, la UI nunca se bloquea)
       Preparar  ── genera el plan y lo deja en Blackboard['plan']
       Aprobar   ── TAiWaitApprovalTool: Node.Suspend → grafo queda esSuspended
       │             └─ OnSuspend → la pregunta aparece como mensaje del chat
       │                (el worker del pool se libera: NADA queda esperando)
       ▼  el usuario responde en el chat
       ResumeThread(ThreadID, 'Aprobar', respuesta)
       Ejecutar  ── recibe la respuesta humana como Input y decide
  └─ OnFinish → resultado como mensaje del chat
```

## Piezas del framework que muestra

| Pieza | Uso en el demo |
|-------|----------------|
| `TAiWaitApprovalTool` (`uMakerAi.Agents.Tools.Approval`) | Tool de fábrica que suspende el grafo y, al reanudar, deja pasar la respuesta humana como `Output`. `SuspendReason` = pregunta; `ContextKey` = clave del blackboard que viaja como contexto |
| `TAIAgentManager.OnSuspend` | Puente agente→chat: llega en un thread del pool, se pasa por `TThread.Queue` y se agrega la pregunta con `ChatView1.AddMessage(crAssistant, ...)` |
| `TAIAgentManager.ResumeThread(ThreadID, NodeName, Input)` | Puente chat→agente: la siguiente respuesta del usuario reanuda el grafo donde quedó |
| `TAiFileCheckpointer` (`uMakerAi.Agents.Checkpoint`) | La suspensión sobrevive al cierre de la app (carpeta `Checkpoints` junto al exe). Al arrancar, `GetActiveThreads` detecta hilos pendientes y la próxima respuesta del chat los reanuda |
| `TAIChatView` / `TAIChatInput` (`AIChat.Control.FMX` / `AIChat.Input`) | La UI de chat **nueva** (Skia). Primer demo que la usa — `TChatList`/`TChatInput` están deprecados |
| `Run(prompt, ASeed)` | La sobrecarga con seed fuerza `ResetExecutionState`: cada petición nueva arranca con estado limpio |

## Detalles que cuesta ver

- **`FPendingThreadID`** es el conmutador del chat: si está vacío, el mensaje del usuario inicia una corrida nueva (`Run`); si no, reanuda la suspendida (`ResumeThread`). Es todo el "protocolo".
- El nombre del nodo a reanudar se conoce por el evento `OnSuspend`; tras un **reinicio de la app** no hay evento, así que el demo usa la constante `NODO_APROBACION` (único nodo que suspende). Con varios nodos de aprobación habría que persistir el nombre (p. ej. en el propio checkpoint via blackboard).
- `OnSuspend`/`OnFinish` llegan en threads del pool → siempre `TThread.Queue` para tocar la UI. La excepción de `OnFinish` **no** se captura en el closure (se libera al volver): se copia `E.Message` antes.
- `OnFinish` también se dispara con `esSuspended`; ese caso se ignora porque `OnSuspend` ya actualizó la UI.
- Para adjuntos/audio como respuesta humana: `ResumeThread` recibe un string — pasar la ruta o una clave de blackboard (el `aMediaFiles` del `OnSendEvent` está disponible para eso).

## Extensión natural (no incluida)

El mismo grafo servido con `TAiA2AServer` publica la suspensión como task `input-required` y un `SendMessage` con el mismo `taskId` la reanuda — ver demos 074/079/080. Eso permite que el chat sea un cliente remoto (Android/web) del agente que corre en un servidor.

## Build & Run

```bash
msbuild AgentChatHITL.dproj /p:Config=Release /p:Platform=Win64
Win64\Release\AgentChatHITL.exe
```

Prueba sugerida: pedir una tarea → aparece el plan y la pregunta → **cerrar la app sin responder** → volver a abrirla → el demo anuncia la tarea recuperada del checkpoint → responder "si" → el plan se ejecuta.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
