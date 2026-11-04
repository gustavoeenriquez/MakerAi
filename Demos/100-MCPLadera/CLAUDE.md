# CLAUDE.md

Demo 100: las funciones de la bodega de Café La Ladera publicadas con MCP. Es la base del episodio 10 de la serie. El mismo `.exe` es servidor y cliente.

## Qué muestra

| Pieza | Qué hace |
|---|---|
| `TAiMCPStdioServer` | `--servidor`: publica un `TAiFunctions` (`inventario`, `precio_mayorista`) por stdio. Termina cuando el cliente cierra stdin (`WaitForInputClosed`). |
| `TAiFunctions.MCPClients` | El chat lanza este mismo `.exe` con `--servidor` como proceso hijo (`tpStdIo`, `Command`/`Arguments`). Las herramientas del servidor llegan al modelo como `ladera_99_inventario` y `ladera_99_precio_mayorista`, junto con las locales. |
| `OnLog` del cliente MCP | El demo filtra el tráfico JSON-RPC (`CLIENT -> SERVER`/`SERVER -> CLIENT`) y muestra cada `tools/call` y su resultado. Con `--trafico`, cada mensaje completo. |
| `TAiMCPHttpServer` | `--http`: las mismas funciones por HTTP en `http://127.0.0.1:8390/mcp` (endpoint `/mcp` por defecto) y el chat se conecta con `tpHttp` y `Params.Values['URL']`. En el demo el servidor corre en el mismo proceso; en la red solo cambia la URL. |

## Uso

```bash
MCPLadera.exe              # chat cliente: 3 preguntas con gpt-6-sol
MCPLadera.exe --servidor   # servidor MCP por stdio (para Claude Desktop o Claude Code)
MCPLadera.exe --trafico    # el chat mostrando cada mensaje JSON-RPC en una línea (sin _meta, recortado a 160)
MCPLadera.exe --http [--trafico] [--port 8390]   # lo mismo por HTTP
MCPLadera.exe --servidor-http [--port 8390]      # solo el servidor HTTP; Enter para salir
```

El cliente requiere `OPENAI_API_KEY`; el servidor no llama a ningún modelo.

Claude Desktop (`%APPDATA%\Claude\claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "ladera": {
      "command": "E:\ruta\100-MCPLadera\Win64\Release\MCPLadera.exe",
      "args": ["--servidor"]
    }
  }
}
```

Claude Code: `claude mcp add ladera -- E:\ruta\MCPLadera.exe --servidor`.

## Probado (oct 22/2026)

Con `gpt-6-sol`:
- **Nariño Altura.** Llamó `inventario` y respondió 6 kg.
- **Hotel, 12 kg de Huila Reserva.** Encadenó `inventario` (18 kg) y `precio_mayorista`: 1.058.400 COP con el 10 %.
- **5 kg de Descafeinado Suave.** Agotado (próximo tueste el martes); dio el precio y avisó que no hay para entregar.

## Probado (nov 4/2026): `--trafico` y `--http`

- **El saludo es `server/discover`, no `initialize`.** Con la versión 2026-07-28 el cliente abre con `server/discover`; el servidor responde `supportedVersions` (`2026-07-28`, `2025-03-26`) y `capabilities` (`tools`, `resources`). Cada mensaje lleva la versión y la identidad del cliente en `params._meta` (sin sesión): `--trafico` lo omite para que se vea el método.
- Después `tools/list` (dos veces: una de `Initialize` y otra de `ListTools` del demo) y un `tools/call` por herramienta, con su `result.content[0].text`.
- Por HTTP el modelo pide herramientas en paralelo y las llamadas llegan en hilos distintos: `TTrafico.McpLog` va con `TMonitor` para no mezclar líneas.
- `TAiFunctions` conecta su `OnLog` al cliente MCP en el primer `tools/call`; el demo lo asigna a `Servidor.MCPClient.OnLog` antes de `Initialize` para ver también el saludo.
- Mismas respuestas por stdio y por HTTP: 6 kg; 18 kg y 1.058.400 COP; agotado y 440.000 COP.

## Notas

- En modo servidor stdout es el canal JSON-RPC: nada de `Writeln`; los mensajes van a `ErrOutput`.
- `Normalizar` quita tildes: el modelo puede escribir "Narino".
- Empresa, productos, existencias y precios ficticios.
