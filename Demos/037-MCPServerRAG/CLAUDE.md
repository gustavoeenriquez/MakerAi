# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Servidor MCP (Model Context Protocol) en consola que expone un sistema RAG **real** respaldado por **SQL Server 2025** usando el motor vectorial de MakerAI. Protocolo por defecto: **SSE**; también HTTP y StdIO. Acceso protegido con login/password quemados y configuración por archivo `.ini`.

Pila: `TAiRAGVector` + `TAiRAGVectorMSSQLDriver` (búsqueda híbrida en T-SQL: `VECTOR_DISTANCE` coseno + `FREETEXTTABLE` BM25) + `TAiOpenAiEmbeddings` (`text-embedding-3-small`).

## Build & Run

```bash
msbuild MCPServerRAG.dproj /p:Config=Release /p:Platform=Win64
```

```bash
# SSE (default según ini), puerto del ini
MCPServerRAG.exe

# Overrides de línea de comandos (prioridad sobre el ini)
MCPServerRAG.exe --config ruta\otro.ini --protocol sse|http|stdio --port 8080
```

## Configuración (.ini)

`MCPServerRAG.ini` junto al ejecutable; **se crea con defaults en la primera ejecución**. Secciones:

| Sección | Claves | Notas |
|---------|--------|-------|
| `[Server]` | `Protocol`, `Port` | sse/http/stdio |
| `[Database]` | `Server`, `Database`, `UserName`, `Password`, `OSAuthent`, `TableName`, `Entidad` | `OSAuthent=Yes` usa autenticación Windows |
| `[Embeddings]` | `ApiKey`, `Model`, `Dimensions` | `ApiKey` admite convención `@VAR_ENTORNO` |
| `[Search]` | `UseBM25`, `EmbeddingWeight`, `BM25Weight`, `Language` | pesos con punto decimal (parseo invariante) |

Requisitos: SQL Server 2025 (17.x) / Azure SQL (tipo `VECTOR` nativo), ODBC Driver instalado, `OPENAI_API_KEY` en el entorno.

## Autenticación

Login/password **quemados** en `uTool.RAG.pas` (`RAG_LOGIN = 'admin'`, `RAG_PASSWORD = 'MakerAi2026*'`), validados vía `OnValidateRequest` (Layer 2 del Core). El cliente debe enviar uno de:

```text
Authorization: Basic base64(admin:MakerAi2026*)
Authorization: Bearer admin:MakerAi2026*     <- compatible con MCPClient MakerAI (ApiBearerToken)
X-API-Key: admin:MakerAi2026*
```

Sin credenciales o con credenciales inválidas → 401 en ambos endpoints (SSE `GET /sse` y `POST /messages`; HTTP `POST /mcp`).

**Nota framework:** esta demo motivó un fix en `UMakerAi.MCPServer.Http.pas` y `UMakerAi.MCPServer.SSE.pas`: se asigna `OnParseAuthentication` al `TIdHTTPServer` para que Indy no rechace con 401 los esquemas `Authorization` distintos de `Basic` (p.ej. `Bearer`) antes de llegar a `ValidateRequest`.

## Architecture

| File | Purpose |
|------|---------|
| `MCPServerRAG.dpr` | Entry point: `--config/--protocol/--port`, factory del transporte, `InitRagEngine` |
| `uTool.RAG.pas` | Tool MCP `rag_vector`, auth (`TRagAuth`), config (`TRagServerConfig`/`LoadServerConfig`), motor singleton |

### Motor RAG (singleton de unidad)

- `TFDConnection` (MSSQL) + `TAiRAGVectorMSSQLDriver` + `TAiRAGVector` + `TAiOpenAiEmbeddings`
- **Conexión perezosa** (`EnsureDb`): el servidor arranca aunque SQL Server esté caído; `CreateSchema` (idempotente) se ejecuta en la primera operación; cada operación reporta el error real de DB
- Con `Driver` asignado, `Search` delega 100% al T-SQL del driver; `AddItem` genera el embedding client-side y hace upsert
- `list_docs`/`delete_doc`/`clear`/`stats` van con SQL directo sobre la tabla usando `JSON_VALUE(properties, '$.doc')` (la memoria es solo cache de sesión y queda vacía tras reiniciar)
- Concurrencia: `TCriticalSection` global serializa todas las operaciones (`TFDConnection` no es thread-safe entre hilos del servidor)

### Operaciones del tool `rag_vector`

| Operation | Params | Notes |
|-----------|--------|-------|
| `index_text` | `textContent`, `docName`, `chunkSize` (800), `overlapPct` (15) | Reindexar borra la versión previa del doc (SQL por metadato) |
| `index_file` | `filePath`, `docName` opcional | UTF-8 estricto con fallback ANSI/BOM |
| `search` | `query`, `topK` (5), `minScore` ("0", string) | Score híbrido 0..1 en `Idx`; vector resultado del driver es propietario |
| `list_docs` | — | `GROUP BY JSON_VALUE(properties,'$.doc')` |
| `delete_doc` | `docName` | `DELETE` por metadato + limpieza de cache en memoria |
| `clear` | — | `DELETE` por entidad |
| `stats` | — | Totales, modelo, DB/tabla/entidad, `fulltext_available` |

## Testing

```bash
# HTTP directo con auth
MCPServerRAG.exe --protocol http --port 8093
curl -X POST http://localhost:8093/mcp -H "Content-Type: application/json" \
  -H "Authorization: Bearer admin:MakerAi2026*" \
  -d '{"jsonrpc":"2.0","method":"tools/list","id":1}'

# SSE con auth
curl -N -H "Authorization: Bearer admin:MakerAi2026*" http://localhost:8094/sse
```

**Estado de pruebas:** compilación, generación del ini, arranque sin DB, los 3 formatos de credenciales (200) y rechazos (401) en HTTP y SSE están verificados en runtime. **Limitación conocida:** la ruta de datos (driver MSSQL contra SQL Server 2025 real) no tiene validación runtime — se decidió no ejecutar esa prueba; si aparecen reportes, revisar primero el CAST nvarchar→VECTOR vía parámetro FireDAC y FREETEXTTABLE parametrizado.

## Key Gotchas

- El vector resultado de `Search` con driver ES propietario de sus nodos (`Create(nil, True)`) — `ResVec.Free` libera todo. (En modo memoria pura sería no-propietario.)
- `AddItem` copia los metadatos con `Assign`: el caller libera su `TAiEmbeddingMetaData`.
- `ReadFloat` de `TIniFile` depende del locale; los pesos se leen como string y se parsean con `TFormatSettings.Invariant`.
- Consolas FireDAC requieren `FireDAC.ConsoleUI.Wait` en el uses.
- El transporte SSE del framework es experimental; para producción preferir HTTP o StdIO.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
