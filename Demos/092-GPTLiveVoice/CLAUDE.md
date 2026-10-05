# CLAUDE.md — Demo 092-GPTLiveVoice

Conversación de voz full-duplex con **OpenAI GPT-Live** (`TAiOpenAiLiveChat`, `gpt-live-1`): el modelo escucha mientras habla (se le puede interrumpir) y delega el razonamiento. FMX, solo Windows (captura y reproducción WASAPI). Requiere `OPENAI_API_KEY` (y la key del proveedor elegido para `DelegateChat`).

**Probado en vivo (oct 5 2026)** con headset: conversación, interrupción, funciones locales, delegación a Responses y a `DelegateChat`, y un cuento de ~2 minutos.

## Piezas

| Pieza | Para qué |
|---|---|
| `TAiAudioCapture` (`asMicrophone`, 24 kHz mono) | Micrófono → `RealtimeSTT := FLive` (el audio va directo al driver). Se activa en `OnSessionReady`: antes de `session.started` el servidor no acepta audio |
| `TAiOpenAiLiveChat` | Sesión GPT-Live. Voz, instrucciones y delegación se fijan al conectar (no cambian con la sesión abierta) |
| `TAiAudioPlayer` | Reproduce `OnAudioChunk` (PCM16 24 kHz). El servidor manda audio **continuo** (incluye silencio) a ritmo real, así que la cola del reproductor se mantiene corta y cortar la voz del modelo corta lo que suena |
| `TAiFunctions` | `hora_actual(zona)` y `luz(habitacion, encendida)` (cambia el círculo de la pantalla). Las funciones corren en un hilo del driver: la UI va por `TThread.Queue` |
| `TAiChatConnection` (opcional) | "Mi chat (DelegateChat)": Driver/Modelo de la pantalla, con las mismas funciones |
| Cliente MCP en `TAiFunctions.MCPClients` (opcional) | "RAG del demo 037 por MCP": la herramienta `rag_vector` del servidor 037 llega al modelo como `cafe_99_rag_vector`, junto con las funciones locales, en los dos modos de delegación |
| `TAiGuardrails` (`FFunctions.Guardrails`) | `OnCheckToolCall`: el RAG solo admite `search`, `list_docs` y `stats`; `clear`, `delete_doc` e `index_*` quedan bloqueados (el modelo de voz no puede borrar la base por una frase mal entendida). El log muestra cada consulta y cada bloqueo |
| Panel "Guía rápida" | Preguntas de ejemplo por tema (conversación, interrupción, funciones, RAG, guardrail, web, delegación) |

## RAG por MCP (demo 037)

1. Configurar el 037 con PostgreSQL (`Driver=postgres`, ver su CLAUDE.md) y arrancarlo: `MCPServerRAG.exe --config <ini> --protocol http --port 8093`.
2. Indexar `conocimiento_cafe_la_ceiba.txt` (negocio **ficticio**: así se nota que la respuesta sale del RAG y no del modelo) con la operación `index_file` (`filePath`, `docName`).
3. En el demo marcar "RAG del demo 037 por MCP" y conectar. Antes de conectar se llama `Initialize` del cliente MCP; si el servidor no responde se avisa en el log.

Las instrucciones se completan solas: al modelo de voz se le dice que consulte la base para preguntas del café; al modelo delegado (`DelegationInstructions`, o el `SystemPrompt` de `DelegateChat`), que use `rag_vector` con `operation=search` y `topK=3`.

**Probado en vivo (oct 5 2026):** "¿Cuál es la contraseña del wifi del Café La Ceiba? ¿Y a qué hora abren los sábados?" → "La contraseña del wifi es RaizDeCeiba2026. Y los sábados abren de ocho de la mañana a dos de la tarde." (~0.9 s al primer texto, ~10 s con la búsqueda).

## Interrupciones y eco

GPT-Live se calla cuando oye algo mientras habla: una tos, un "mm" o **su propia voz** si el micrófono la capta (parlantes, o audífonos con volumen alto). Un cuento largo "se cortaba" por eso; con audio limpio el modelo lo termina.

- **"Modo altavoz"**: pausa el micrófono (`TAiAudioCapture.Muted`, manda silencio sin cortar el flujo) mientras la salida tiene voz — detectada por la energía del audio que llega, más 800 ms de margen. No se puede interrumpir al modelo, pero no se escucha a sí mismo.
- **"Silenciar micrófono"**: `session.input_audio.mute` del lado del servidor (útil para escuchar algo largo sin interrupciones).
- **Diagnóstico en el log**: si el modelo transcribe algo del usuario mientras habla, se muestra `El modelo te escuchó mientras hablaba: "..."`. Si no lo dijo el usuario, es eco.

## Notas

- La latencia del log es aproximada (desde el último texto del usuario, que llega con retraso). Medida en pruebas: ~0.5 s hasta el primer texto del asistente; delegación con función ~2 s (Responses) y ~4.9 s (`DelegateChat` con `gpt-6-luna`).
- El costo de la barra de estado es solo la voz (US$0.05/min); el modelo delegado se cobra aparte.
- `ReportMemoryLeaksOnShutdown := True` en el `.dpr` para detectar fugas al cerrar. Destapó tres fugas del framework (oct 2026), ya corregidas: la respuesta de `tools/list` en `Initialize` del cliente MCP, el diccionario de `TAiRealtimeFactory` y el esquema clonado en cada `tools/list` del servidor MCP. El driver GPT-Live midió 0 bloques perdidos con y sin `Disconnect`.
- Si el demo se lanza desde un proceso abierto antes de cambiar `OPENAI_API_KEY` con `setx`, hereda la key vieja (401 en el handshake).
