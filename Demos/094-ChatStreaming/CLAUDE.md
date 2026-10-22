# CLAUDE.md

Demo 094: streaming, prompt de sistema e historial, en consola. Es la base del episodio 3 de la serie de Café La Ladera.

## Qué muestra

1. **Prompt de sistema.** `SystemPrompt` define a Lucía, la barista virtual. Le da la carta de la tienda y sus reglas: responde breve, solo habla de café y no inventa precios. La charla guiada pide ayuda con una tarea de matemáticas para mostrar que la regla se cumple.
2. **Streaming.** Con `Params.Values['Asynchronous'] := 'True'`, `AddMessageAndRun` vuelve enseguida. Los pedazos llegan por `OnReceiveData` y el final por `OnReceiveDataEnd`. La consola mide cuánto tarda el primer pedazo. Con `--sin-streaming` la misma charla bloquea hasta la respuesta completa.
3. **Historial.** "¿Y cuánto cuestan dos bolsas de ese?" solo se entiende porque `Messages` guarda los turnos anteriores. Después el demo guarda con `Messages.SaveToFile`, olvida con `NewChat` y recupera con `Messages.LoadFromFile`.

## Uso

```bash
ChatStreaming.exe                          # OpenAI (@OPENAI_API_KEY)
ChatStreaming.exe --driver Claude          # cualquier driver registrado
ChatStreaming.exe --driver Ollama --model gemma4
ChatStreaming.exe --sin-streaming
ChatStreaming.exe --interactivo            # /nuevo /guardar /cargar /historial /salir
```

## Probado (oct 22/2026)

Con OpenAI `gpt-6-sol`, con y sin streaming:
- Recomendó Sierra Nevada Orgánico y rechazó la tarea de matemáticas.
- Calculó 72.000 COP por dos bolsas.
- Tras `NewChat` no recordaba nada, y tras `LoadFromFile` recordó la pregunta.

Con streaming, el primer pedazo llegó entre 0,8 y 2,6 s.

## Notas

- Los eventos son `of object`, por eso viven en la clase `TOyente`.
- El driver de OpenAI llama `OnReceiveData` desde el hilo HTTP, por eso la consola va con candado. Otros drivers encolan con `TThread.Queue`, y el hilo principal los atiende con `CheckSynchronize` mientras espera.
- `NewChat` borra los mensajes pero no el `SystemPrompt`: la barista sigue siendo Lucía.
- El historial guardado incluye el mensaje `system`.
- Empresa, productos y precios ficticios.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
