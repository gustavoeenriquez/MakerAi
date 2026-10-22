# CLAUDE.md

Demo 093 — un chat que consulta a un agente A2A como herramienta (`TAiA2AAgentTool`).

## Qué muestra

1. **Agente:** un grafo `TAIAgentManager` de dos nodos (`Buscar` → `Firmar`) con el catálogo de Café La Ladera (precios, notas de cata, existencias). Es determinista, sin LLM. Se publica como agente A2A 1.0 con `TAiA2AServer`, con una skill en la Agent Card.
2. **Herramienta:** `TAiA2AAgentTool` con `AgentUrl`, `ToolName := 'catalogo_cafe'` y `Functions := Funcs` (al asignarlo registra la función con un único parámetro `query`). `DiscoverFromCard` suma a la descripción la que el agente publica de sí mismo.
3. **Chat:** `TAiChatConnection` con `AiFunctions := Funcs` y `Tool_Active := True` (canal tipado). El modelo decide cuándo consultar al agente y cuántas veces.

Es el hermano de `TAiA2ARemoteAgentTool` (demos 072 y 074): aquel federa un **nodo de grafo**; este, una **conversación**. Si el agente cae o pide datos (`input-required`), la herramienta se lo cuenta al modelo como texto en vez de romper el turno.

## Uso

```bash
A2AChatAgentTool.exe                               # OpenAI (@OPENAI_API_KEY)
A2AChatAgentTool.exe --driver Claude               # cualquier driver registrado (@CLAUDE_API_KEY)
A2AChatAgentTool.exe --driver Ollama --model gemma4
A2AChatAgentTool.exe --sin-llm                     # llama la herramienta directo
A2AChatAgentTool.exe "¿Qué café descafeinado tienen?"
A2AChatAgentTool.exe --port 8293                   # puerto del agente A2A
```

Sin la variable de la clave del driver pasa solo a `--sin-llm`.

## Probado (oct 22/2026)

- `--sin-llm`: el agente responde por A2A y la herramienta devuelve el texto.
- OpenAI (`gpt-6-sol`, default del driver): en la primera pregunta el modelo consultó **dos veces** al agente; el café con notas a chocolate estaba agotado y en la segunda consulta encontró el Descafeinado Suave (notas de cacao) como alternativa disponible. En la segunda pregunta calculó el total de dos bolsas (84.000 COP) con el precio del catálogo.

## Notas

- `PREGUNTAS_EJEMPLO` y no `PREGUNTAS`: Delphi no distingue mayúsculas y chocaba con la variable `Preguntas`.
- El nodo `Buscar` corre en un hilo del servidor A2A: la consola va con candado (`Log`).
- La consola se pone en UTF-8 (`SetConsoleOutputCP`/`SetTextCodePage`) para las tildes.
- Empresa, productos y precios ficticios.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
