# CLAUDE.md

Demo 100: las funciones de la bodega de Café La Ladera publicadas con MCP. Es la base del episodio 10 de la serie. El mismo `.exe` es servidor y cliente.

## Qué muestra

| Pieza | Qué hace |
|---|---|
| `TAiMCPStdioServer` | `--servidor`: publica un `TAiFunctions` (`inventario`, `precio_mayorista`) por stdio. Termina cuando el cliente cierra stdin (`WaitForInputClosed`). |
| `TAiFunctions.MCPClients` | El chat lanza este mismo `.exe` con `--servidor` como proceso hijo (`tpStdIo`, `Command`/`Arguments`). Las herramientas del servidor llegan al modelo como `ladera_99_inventario` y `ladera_99_precio_mayorista`, junto con las locales. |
| `OnLog` del cliente MCP | El demo filtra el tráfico JSON-RPC (`CLIENT -> SERVER`/`SERVER -> CLIENT`) y muestra cada `tools/call` y su resultado. |

## Uso

```bash
MCPLadera.exe              # chat cliente: 3 preguntas con gpt-6-sol
MCPLadera.exe --servidor   # servidor MCP por stdio (para Claude Desktop o Claude Code)
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

## Notas

- En modo servidor stdout es el canal JSON-RPC: nada de `Writeln`; los mensajes van a `ErrOutput`.
- `Normalizar` quita tildes: el modelo puede escribir "Narino".
- Empresa, productos, existencias y precios ficticios.
