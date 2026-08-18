# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

The Agents module implements a graph-based autonomous agent orchestration framework. It enables building workflows where multiple AI-powered nodes execute in sequence or parallel, with conditional routing and shared state management.

## Unit Structure

| Unit | Purpose |
|------|---------|
| `uMakerAi.Agents.pas` | Core framework: `TAIAgentManager`, `TAIAgentsNode`, `TAIAgentsLink`, `TAIBlackboard` |
| `uMakerAi.Agents.Attributes.pas` | RTTI attributes `TToolAttribute`, `TToolParameterAttribute` and `TSecretAttribute` (`[TSecret]`: la propiedad nunca se serializa a disco y los valores entrantes desde JSON se ignoran — credenciales solo en runtime) |
| `uMakerAi.Agents.EngineRegistry.pas` | Singleton registries for tool discovery (`TEngineRegistry`, `TAgentHandlerRegistry`) |
| `uMakerAi.Agents.GraphBuilder.pas` | `TGraphBuilder` parses JSON graph specs into runtime structures. `StrictValidation` (default True) raises `EAiGraphError` on structural defects (edge to missing node, undeclared port, >4 fanout outputs); set False for the legacy warn-and-drop behavior (fix M-02) |
| `uMakerAi.Agents.DmGenerator.pas` | `TDataModuleGenerator` generates Delphi DataModule code from JSON graphs |
| `uMakerAi.A2A.Server.pas` | `TAiA2AServer` (spec A2A 1.0): expone `TAIAgentManager`(s) como agente A2A — Agent Card en `/.well-known/agent-card.json`, JSON-RPC `SendMessage`/`GetTask`/`CancelTask` (+ aliases 0.x). Pool de managers, ciclo de vida real del task, resume de `input-required`, auth bearer y `traceparent`; sin streaming (rechaza con UnsupportedOperationError) |
| `uMakerAi.A2A.Client.pas` | `TAiA2AClient`: consume agentes A2A remotos — `FetchAgentCard`, `SendText`/`SendTextEx`, `GetTask`/`CancelTask`; estado normalizado en `LastTaskState` y pregunta del agente en `LastStatusMessage`. Incluye `TAiA2ARemoteAgentTool` (federacion): asignado como `Tool` de un nodo, delega el input del nodo en un agente A2A remoto; registrado en `TEngineRegistry` |

## Core Classes

**TAIAgentManager** - Orchestrates workflow execution via TThreadPool. Key properties: `StartNode`, `EndNode`, `MaxConcurrentTasks` (default 4). Use `Run(APrompt)` for sync execution, `Compile()` to validate before running. `Compile` is purely structural (validation + InEdges); it does NOT clear the Blackboard — a pre-seeded `Blackboard.AskMsg` survives the first `Run` (fix M-03). For an explicit clean slate between runs call `ResetExecutionState` (clears blackboard, node/link transient state; keeps graph structure).

**TAIAgentsNode** - Workflow vertex. Executes via `OnExecute` callback or attached `Tool: TAiToolBase`. Join modes: `jmAny` (first input triggers), `jmAll` (waits for all inputs).

**TAIAgentsLink** - Directed edge connecting nodes. Four routing modes:
- `lmFanout` - Broadcasts to all NextA/B/C/D slots
- `lmConditional` - Routes based on `Blackboard[ConditionalKey]` value
- `lmManual` - Targets specified programmatically via `ManualTargetsKey`
- `lmExpression` - Evaluates `ExpressionA/B/C/D` bindings against blackboard

**TAIBlackboard** - Thread-safe shared state (TDictionary with TCriticalSection). Standard accessors: `SetString/GetString`, `SetInteger/GetInteger`, `SetBoolean/GetBoolean`. Chat messages via `AskMsg`, `ResMsg` properties.

**TAiToolBase** - Abstract base for node tools. Inherit and implement `Execute(ANode, AInput, var AOutput)`.

**TAiToolParams** - Public RTTI mapper for tool parameters (fix M-04): `ToJSON`/`FromJSON`/`SchemaOf`. Excludes `AI_TOOL_RESERVED_PROPS` (`Name`, `Tag`, `ID`, `Description`) both ways, honors `[TSecret]` (never written; incoming values ignored; `SchemaOf` marks them `writeOnly` + `x-credential-type`). `FromJSON` accepts typed JSON values or their string representation (single mapper — `GraphBuilder.SetToolParameters` delegates here). Serialization notes: `TAIAgentManager.SaveToStream` raises `EAiGraphError` for nodes with `OnExecute` and no `Tool` unless `AllowPartialSerialization := True` (fix M-06).

## Creating Custom Tools

```pascal
uses uMakerAi.Agents, uMakerAi.Agents.Attributes, uMakerAi.Agents.EngineRegistry;

type
  [TToolAttribute('MyTool', 'Does something useful', 'Custom')]
  TMyTool = class(TAiToolBase)
  private
    [TToolParameterAttribute('API Key', 'Your API key', '')]
    FApiKey: string;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string; var AOutput: string); override;
  published
    property ApiKey: string read FApiKey write FApiKey;
  end;

initialization
  TEngineRegistry.Instance.RegisterTool(TMyTool, 'uMyToolUnit');
```

## JSON Graph Format

GraphBuilder expects this structure:
```json
{
  "nodes": [
    {
      "id": "node_guid",
      "label": "NodeName",
      "toolClassName": "TMyTool",
      "parameters": { "ApiKey": "xxx" },
      "properties": {
        "engine": { "joinMode": "jmAny", "linkMode": "lmFanout" }
      }
    }
  ],
  "edges": [
    {
      "sourceNodeId": "node1_guid",
      "targetNodeId": "node2_guid",
      "sourceTerminal": "out_a"
    }
  ]
}
```

Port terminals: `out_a`, `out_b`, `out_c`, `out_d`, `out_failure` (maps to NextNo in ALL link modes, including `lmConditional` — fix M-07).

## Reusing a manager across runs

`Compile` validates structure only; it does **not** clear execution state (fix M-03 — clearing there freed the `AskMsg`/`ResMsg` that `Run` seeds beforehand). Since `Compile` also early-exits when `FCompiled` is already `True`, **consecutive `Run(APrompt)` calls on the same instance inherit the previous run's state**: blackboard keys, node `Input`/`Output`/`FError`/`FSuspended`, and link `NoCycles`.

That is the historic behavior and it is preserved. To get a clean run, use the seeding overload:

```pascal
Manager.Run('prompt',
  procedure(B: TAIBlackboard)
  begin
    B.SetString('cliente_id', '42');   // seeded AFTER the reset, so it survives
  end);
```

Guaranteed order: `Compile` -> `ResetExecutionState` -> `ASeed` -> seed `AskMsg`/`ResMsg` -> execute. Seeding before `Run` instead of inside `ASeed` would be wiped by the reset.

`ASeed` may assign `AskMsg` (it is not overwritten if already set). It need not assign `ResMsg` — `Run` always creates a fresh one for the run.

`ResetExecutionState` is also public if you prefer to call it explicitly.

> `TAIBlackboard.SetAskMsg`/`SetResMsg` free the previously stored message before replacing it. Without that, every repeated `Run` leaked the prior `ResMsg`, because `SetValue` only does `AddOrSetValue` and `Clear` was no longer being reached.

## Execution Status

`TAgentExecutionStatus`: `esUnknown`, `esRunning`, `esCompleted`, `esError`, `esTimeout`, `esAborted`, `esSuspended`

Access via `Blackboard.SetStatus()`/`GetStatus()`.

`esSuspended` is set when one or more nodes called `Node.Suspend(...)` during execution. Use `TAIAgentManager.ResumeThread(ThreadID, NodeName, Input)` to resume.

## Durable Execution (Checkpoint / Suspend-Resume)

**Key types** (in `uMakerAi.Agents.Checkpoint`):
- `IAiCheckpointer` — persistence contract; assign to `TAIAgentManager.Checkpointer`
- `TAiNullCheckpointer` — no-op (default behavior, no disk writes)
- `TAiFileCheckpointer` — JSON on disk (`<dir>/<GUID>.checkpoint.json`)
- `TAiCheckpointSnapshot` — serialized state (blackboard + node/link states + pending steps)

**Suspend a node from `OnExecute`:**
```pascal
procedure MyNodeExecute(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
  Input: string; var Output: string);
begin
  Output := Input; // pass-through
  Node.Suspend('Requiere aprobación', 'Contexto adicional');
end;
```

Or use the built-in `TAiWaitApprovalTool` (from `uMakerAi.Agents.Tools.Approval`).

**Resume after suspension:**
```pascal
// Detect suspended threads on app startup
var Threads := AgentManager.GetActiveThreads;
// Resume when user approves
AgentManager.ResumeThread(ThreadID, 'NombreDelNodo', 'Aprobado');
```

**Event for human-in-the-loop UI:**
```pascal
AgentManager.OnSuspend := procedure(Sender: TObject; const ThreadID, NodeName,
  Reason, Context: string) begin
  ShowMessage('Aprobación requerida: ' + Reason);
end;
```

## A2A: flujos de orquestación

**Concurrencia.** Un `TAIAgentManager` no ejecuta dos grafos a la vez, así que un servidor A2A con un solo manager serializa los tasks (el segundo recibe `AgentBusy`, `-32000`). Para fan-out real asignar `TAiA2AServer.OnAcquireManager`: la fábrica devuelve un grafo nuevo y el servidor mantiene un pool de hasta `MaxConcurrentTasks`. La reserva del slot es atómica (no depende de `Busy`), de modo que dos peticiones simultáneas no se quedan con el mismo manager.

```pascal
procedure TForm1.AcquireManager(Sender: TObject; var AManager: TAIAgentManager);
begin
  AManager := TAIAgentManager.Create(nil);   // el servidor lo libera
  AManager.AddNode('Uno', NodeExec).AddNode('Dos', NodeExec);
  AManager.AddEdge('Uno', 'Dos');
  AManager.SetEntryPoint('Uno').SetFinishPoint('Dos');
end;
```

**Ciclo de vida del task.** `SendMessage` con `configuration.blocking = false` (o al vencer `WaitTimeoutMs`) responde con el task en `working`; el cliente sigue con `GetTask`, que refresca el estado desde el grafo vivo. El slot se libera solo al llegar a un estado terminal. Un vencimiento de espera **no** marca `failed`.

> El servidor fuerza `Asynchronous := True` en los managers que conduce: en modo síncrono `Run` haría `Wait(INFINITE)` (ignorando el timeout) y lanzaría excepción en vez de dejar el estado en el blackboard.

**Human-in-the-loop a través de A2A.** Una suspensión (`Node.Suspend`) se publica como `input-required` con la pregunta en `status.message`, y el task conserva su manager. Un `SendMessage` posterior con el mismo `taskId` reanuda el grafo (`ResumeThread`) usando el texto recibido como respuesta humana.

En el lado federado, `TAiA2ARemoteAgentTool` suspende el **nodo local** cuando el agente remoto pide input (`SuspendOnInputRequired`, default `True`) en vez de fallar; al reanudar el nodo local, la respuesta viaja al mismo task remoto. El task remoto pendiente se recuerda en el blackboard bajo `A2A.<NodoName>.PendingTask`.

**Literales de estado.** `StateNaming` elige la forma emitida: `anProto` (`TASK_STATE_COMPLETED`, default) o `anLower` (`completed`). La lectura tolera ambas siempre — usar `TAiA2AClient.LastTaskState` (enum) y no comparar strings.

> v1.0 pasó de kebab-case a SCREAMING_SNAKE_CASE por conformidad con ProtoJSON, así que **`anProto` es el formato correcto de la spec** y `anLower` el de compatibilidad con la era 0.x. Es al revés de lo que sugiere la intuición.

## Fuentes normativas del protocolo

**No razonar de memoria ni desde la web: el proto manda.**

| Fuente | Estatus |
|---|---|
| [`specification/a2a.proto`](https://github.com/a2aproject/A2A/blob/main/specification/a2a.proto) | **Única definición normativa** de todos los objetos y mensajes |
| `spec/a2a.json` | JSON Schema generado en build, **no versionado y no normativo** |
| [a2a-protocol.org/latest/specification](https://a2a-protocol.org/latest/specification/) | Prosa derivada del proto; útil para entender, no para dirimir |
| [`a2aproject/a2a-tck`](https://github.com/a2aproject/a2a-tck) | **TCK oficial de conformidad**; valida JSON-RPC con niveles RFC 2119 (MUST / SHOULD / MAY) |

Atajo práctico para consultar la forma canónica de cualquier mensaje sin leer el proto: instalar `a2a-sdk` (es proto-first) e inspeccionar el descriptor.

```python
import a2a.types as T
print([f.name for f in T.SendMessageResponse.DESCRIPTOR.fields])   # ['task', 'message']
```

## Formato de cable: v1.0 vs 0.x

Verificado contra el SDK oficial `a2a-sdk` 1.1.2 (agosto 2026). El SDK 1.x es **proto-first**: los tipos son protobuf y el cable es **protojson**.

`WireEra` (default `weV1`) controla dos diferencias que rompen la interop:

**1. `SendMessage` devuelve el Task envuelto.** `SendMessageResponse` es un *oneof* `{task | message}`:

```jsonc
{"result": {"task": {...}}}    // v1.0
{"result": {...campos...}}     // 0.x
```

**`GetTask` y `CancelTask` NO llevan wrapper** en ninguna era: en el proto son `returns (Task)` y no tienen mensaje de respuesta propio.

El cliente normaliza ambas formas con `UnwrapSendMessageResult`, que además sintetiza un Task cuando el agente responde con `{"message": ...}` (respuesta inmediata sin crear tarea).

**2. La Agent Card cambió de forma.** En v1.0 la raíz **no tiene** `url`, `protocolVersion` ni `preferredTransport`: van dentro de `supportedInterfaces[] = [url, protocolBinding, tenant, protocolVersion]`. Sin eso, un cliente v1.0 no sabe a qué URL hablar. Además `security` se llama `securityRequirements` (con un campo `schemes`) y `capabilities.stateTransitionHistory` ya no existe.

El cliente lee la URL de `supportedInterfaces` con fallback al `url` plano de 0.x.

### Autorización

`ApiKey` exige `Authorization: Bearer <clave>` (o `X-API-Key`). Sin ella → 401 con `WWW-Authenticate`. `OnAuthorize` permite decidir a mano y siempre se llama.

Dos cosas que costaron encontrar y conviene no volver a romper:

> **`TIdHTTPServer` solo entiende autenticación `Basic`.** Ante un `Authorization: Bearer …` responde **401 él solo** y `HttpCommand` no llega a ejecutarse nunca. Por eso el servidor engancha `OnParseAuthentication` con `VHandled := True`. Sin ese handler la auth por bearer **no funciona en absoluto** — solo pasaba el `X-API-Key`, y el 401 parecía nuestro. Los servidores MCP ya lo hacían; el A2A no. Misma trampa del demo 037 y del 080.

> **La Agent Card base queda fuera de la comprobación de `ApiKey`.** Es un documento de descubrimiento, y es justo donde el cliente lee *qué* esquema de seguridad usar: protegerla es un círculo vicioso. Para datos solo a clientes autenticados está la extended card. `OnAuthorize` sí se sigue llamando sobre ella, por si alguien quiere cerrarla a propósito.

El secreto se compara **exacto**; solo el nombre del esquema va sin distinguir mayúsculas. Con `SameText` sobre la cabecera entera, `clavesecreta` valía por `ClaveSecreta`.

### Skills de la Agent Card

Una skill es lo que un cliente lee para **decidir si este agente le sirve**. No es un punto de entrada: la spec no lleva selector de skill en `SendMessage`, así que las skills describen, no enrutan.

Por eso **no se derivan de los nodos del grafo**. Un grafo normal tiene nodos `Nodo1`/`Nodo2` sin descripción, y publicarlos sería ruido que además insinúa una granularidad de invocación que no existe. Se declaran:

```pascal
Server.Skills.AddSkill('traducir', 'Traductor', 'Traduce texto', 'idiomas, texto');
```

`Tags` va separado por comas y se emite como array JSON. Si la colección queda vacía se publica una única skill `run-graph` — la card **nunca** sale sin skills.

`PublishNodesAsSkills := True` añade además un skill por nodo, pero **solo para los nodos cuyo `Tool` trae `Description`**; los demás se omiten a propósito. Para control total sigue estando `OnCustomizeCard`.

> **Los tests de interop deben hablar HTTP crudo.** Los casos `a2a.wire.v1`, `a2a.wire.v03` y `a2a.listtasks` de la suite no usan `TAiA2AClient` a propósito: durante meses los 9 casos A2A pasaron con el formato equivocado porque cliente y servidor compartían el error. Un test que valida contra tu propia implementación no prueba conformidad.
>
> Para conformidad real está `Tests/Interop/run_interop.ps1`, que prueba ambas direcciones contra el SDK oficial `a2a-sdk`.

## Métodos JSON-RPC soportados

| Método | Estado |
|---|---|
| `SendMessage` | ✅ (+ alias 0.x `message/send`) |
| `GetTask` | ✅ (+ `tasks/get`) |
| `CancelTask` | ✅ (+ `tasks/cancel`) |
| `ListTasks` | ✅ filtros `contextId` y `status`, paginado por `pageSize`; devuelve `{tasks, totalSize, pageSize}` |
| `GetExtendedAgentCard` | ✅ si `PublishExtendedCard := True`; si no, `UnsupportedOperationError`. Para diferenciarla de la pública hay que enriquecerla en `OnCustomizeCard` |
| `SendStreamingMessage` | ✅ SSE (+ alias `message/stream`) |
| `SubscribeToTask` | ✅ SSE (+ `tasks/resubscribe`); error si el task ya es terminal |
| `CreateTaskPushNotificationConfig` etc. | ✅ CRUD completo + entrega al webhook |

## Threading Model

- Nodes execute in parallel via TThreadPool
- `MaxConcurrentTasks` limits concurrent executions
- TAIBlackboard operations are thread-safe
- Node callbacks (`OnExecute`) run on worker threads - use `TThread.Synchronize` for UI updates

## Dependencies

Internal: `uMakerAi.Chat`, `uMakerAi.Core`, `uMakerAi.Chat.Messages`

Framework: `System.Threading`, `System.Bindings.*`, `System.Rtti`, `System.JSON`, `System.SyncObjs`

## Streaming SSE

`EnableStreaming` (default `True`) habilita `SendStreamingMessage` y
`SubscribeToTask`, y lo anuncia en `capabilities.streaming`.

**El framing depende del binding.** Cada evento va en una línea `data:`, pero:

```jsonc
// binding JSON-RPC (el nuestro) — CON envoltorio, spec §9.4.2
data: {"jsonrpc":"2.0","id":1,"result":{ StreamResponse }}

// binding REST — el StreamResponse DESNUDO
data: {"statusUpdate": { ... }}
```

`StreamResponse` es un oneof: `task | message | statusUpdate | artifactUpdate`.

**Orden de emisión:** snapshot inicial `task` → `artifactUpdate` (uno por
artifact, el último con `lastChunk: true`) → `statusUpdate` terminal. Los
artifacts van **antes** del status final; es el orden del ejemplo de la spec y
lo que valida el test de ordenación del TCK.

**`SendStreamingMessage` con `taskId` reanuda**, igual que `SendMessage`: mismas
validaciones (task inexistente → `-32001`, par `taskId`/`contextId` cruzado →
`-32602`, task terminal → `-32004`). Sin esto un human-in-the-loop por streaming
abría un task nuevo en cada turno y perdía el grafo suspendido.

> **El estado puede ser final antes del primer snapshot** — un grafo rápido, o un
> resume que completa enseguida. El seguimiento se hace comparando contra el
> estado del snapshot, así que en ese caso no queda ningún cambio que detectar y
> el stream se quedaba abierto hasta agotar `WaitTimeoutMs`. Ahora se cierra en
> el momento.
>
> Con un matiz que cuesta ver: **`input-required` cierra solo si NO es una
> suscripción**. En `SendStreamingMessage` significa "te toca a ti"; quien hace
> `SubscribeToTask` es un observador y tiene que seguir viendo las transiciones
> posteriores. Cerrarle el stream rompe `SUBSCRIBE-TERMINAL` del TCK.

Un stream también se cierra en `input-required`: la pelota pasa al cliente.

> **Los snapshots de un mismo task deben ser idénticos.** Dos clientes
> suscritos reciben los mismos eventos, así que nada volátil puede colarse en
> ellos. Dos fallos reales por esto: el `messageId` del status se generaba con
> un GUID nuevo en cada llamada, y `RefreshTask` sellaba `UpdatedAt := Now` en
> cada consulta en vez de solo al cambiar de estado. Lo segundo además impedía
> que venciera el TTL de purga, que se mide contra ese campo.

## Llamar a un agente A2A desde un chat

`TAiA2AAgentTool` expone un agente remoto como **herramienta de chat**: al
asignarle un `TAiFunctions`, el LLM puede invocarlo igual que a cualquier otra
función.

```pascal
AgentTool := TAiA2AAgentTool.Create(Self);
AgentTool.AgentUrl := 'http://host:8280';
AgentTool.ToolName := 'preguntar_experto';
AgentTool.Description := 'Consulta a un agente especializado en normativa';
AgentTool.Functions := AiFunctions1;   // al asignarlo se registra la función
```

Es el hermano de `TAiA2ARemoteAgentTool`: aquel federa un **nodo de grafo**,
este una **conversación**.

`DiscoverFromCard` rellena la descripción desde la Agent Card del agente, de
modo que el LLM vea la que el propio agente publica de sí mismo.

Los fallos se le cuentan al modelo como texto en vez de romper el turno: si el
agente remoto cae o pide más datos, el LLM puede reintentar, preguntar al
usuario o seguir sin esa información. Una excepción abortaría la conversación
entera por un problema de un colaborador.

## Push notifications

`EnablePushNotifications` (default `True`) habilita el CRUD de configs y la
entrega, y lo anuncia en la card.

- La config llega por `CreateTaskPushNotificationConfig` **o** dentro de
  `SendMessage`, en `configuration.taskPushNotificationConfig`.
- Los parámetros se aceptan en camelCase **y** snake_case: el binding usa
  camelCase pero varios clientes mandan los nombres del proto.
- El payload del webhook es un **`StreamResponse`** (brazo `task`), no el Task
  desnudo. Se envía con `Authorization` si la config trae `authentication`.
- `Delete` es idempotente y re-registrar el mismo `id` reemplaza, para que el
  webhook no reciba la misma notificación dos veces.

**La entrega es activa**: no hace falta que nadie consulte el task. El servidor
engancha `TAIAgentManager.OnFinish` (encadenando el handler previo) y desde ahi
refresca el task, lo que dispara la entrega. Funciona con `blocking:false`.

> **Cuidado con el orden del motor de grafos.** `TAIAgentManager` dispara
> `OnFinish` y **solo despues** pone `FBusy` a 0. Un refresco lanzado desde ese
> evento ve el manager como ocupado, se va por la rama "sigue trabajando" y se
> pierde la transicion final — el webhook no se entera jamas. Por eso
> `RefreshTask` tiene el parametro `AIgnoreBusy`, que solo debe usarse desde
> `OnGraphFinished`, donde ya se sabe que el grafo termino.

El TCK pasa `PUSH-DELIVER-*` con un flujo bloqueante, asi que **no cubre este
escenario**; lo cubre el demo 080.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
