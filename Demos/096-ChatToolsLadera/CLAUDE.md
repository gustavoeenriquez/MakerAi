# CLAUDE.md

Demo 096: el modelo actúa con tres herramientas del sistema, cada una con su límite: búsqueda web, editor de archivos y consola. Es la base del episodio 7 de la serie de Café La Ladera.

## Qué muestra

Las capacidades van en `ModelCaps` (canal tipado). El driver declara la tool nativa del proveedor y, cuando el modelo la llama, la ejecuta el componente de `ChatTools`.

| Capacidad | En OpenAI | Quién la ejecuta | Límite en el demo |
|---|---|---|---|
| `cap_WebSearch` | `web_search` | El proveedor (Delphi no ejecuta nada) | — |
| `cap_TextEditor` | `apply_patch` | `ChatTools.TextEditorTool` (`TAiTextEditorTool`) | `OnBeforeCommand` lo confina a la carpeta de trabajo y no deja borrar |
| `cap_Shell` | `shell` | `ChatTools.ShellTool` (`TAiShell`) | `SecurityMode := ssmAllowList` con comandos de lectura (`dir`, `type`, `findstr`, `cd`...) |

Las cuatro tareas:
1. **Buscar en la web** el precio interno de referencia de la Federación.
2. **Crear** `inventario.md` con el editor.
3. **Leerlo con la consola** y sacar cuentas.
4. **Intentar borrar todo.** La consola bloquea `del` y el editor bloquea `delete`.

## Uso

```bash
ChatToolsLadera.exe                    # OpenAI (@OPENAI_API_KEY)
ChatToolsLadera.exe --driver Claude    # bash y editor nativos de Claude (@CLAUDE_API_KEY)
```

La carpeta de trabajo es `trabajo_ladera`, junto al exe.

## Probado (oct 22/2026)

Con OpenAI `gpt-6-sol`, las cuatro tareas salieron bien:
- **Precio de la Federación:** 2.235.000 COP por carga del 21 de oct, con fuente.
- **Inventario:** el modelo creó el archivo, lo leyó y calculó 49 bolsas, con Sierra Nevada agotado.
- **Borrado:** el modelo intentó `cd /d ... && del /f /q *`, y el comando se bloqueó.

La prueba encontró dos bugs del framework, ya corregidos:
- **`TAiShell` revisaba solo el primer token de todo el comando.** Ese mismo `cd ... && del *` pasaba la lista permitida porque `cd` está en ella. También pasaban `dir & del` y `echo $(rm x)`, y en lista bloqueada `d^el` y `/bin/rm`. Ahora se revisa cada tramo, y en lista permitida se rechazan redirecciones y `$( )`. Caso de la suite: `tools.shell.security-chaining`.
- **`apply_patch` de OpenAI no podía crear archivos.** `create_file` se mandaba a `apply_diff` sobre un archivo inexistente. Ahora va a `create` con el texto que sale de `V4ACreateFileText`. Caso de la suite: `tools.applypatch.create-file`.

## Notas

- `TAiTextEditorTool` no implementa `delete`, y el `delete_file` de `apply_patch` termina en error. El demo lo rechaza explícitamente en `OnBeforeCommand` para que el límite quede a la vista.
- Antes del arreglo, el modelo probó borrar por el editor cuando la consola le negó `del`. Cada herramienta necesita su propio límite.
- Empresa, productos y precios ficticios.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
