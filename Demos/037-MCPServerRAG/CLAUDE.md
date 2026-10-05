# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Servidor MCP (Model Context Protocol) en consola que expone un sistema RAG **real** respaldado por una base de datos, elegida con `Driver` en el `.ini`: **SQL Server 2025** (`mssql`, default) o **PostgreSQL + pgvector** (`postgres`, oct 2026). Protocolo por defecto: **SSE**; también HTTP y StdIO. Acceso protegido con login/password quemados y configuración por archivo `.ini`.

Pila: `TAiRAGVector` + `TAiRAGVectorMSSQLDriver` (búsqueda híbrida en T-SQL: `VECTOR_DISTANCE` coseno + `FREETEXTTABLE` BM25) **o** `TAiRAGVectorPostgresDriver` (HNSW coseno + `tsvector`) + `TAiOpenAiEmbeddings` (`text-embedding-3-small`).

**Probado en runtime con PostgreSQL (oct 5 2026):** indexado, búsqueda híbrida, `list_docs`/`stats`, y consumido por voz desde el demo 092 (GPT-Live) vía MCP HTTP.

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
| `[Database]` | `Driver`, `Server`, `Port`, `VendorLib`, `Database`, `UserName`, `Password`, `OSAuthent`, `TableName`, `Entidad` | `Driver=mssql` (default) o `postgres`. `OSAuthent=Yes` (solo mssql) usa autenticación Windows. Con postgres, `VendorLib` vacío autodetecta `libpq.dll` en `C:\Program Files\PostgreSQL\<18..12>\bin`. **Valores que empiezan con `@` se leen del entorno** (`Password=@PGPASSWORD`, `Database=@PGDATABASE`): la clave no queda en el ini |
| `[Embeddings]` | `ApiKey`, `Model`, `Dimensions` | `ApiKey` admite convención `@VAR_ENTORNO` |
| `[Search]` | `UseBM25`, `EmbeddingWeight`, `BM25Weight`, `Language` | pesos con punto decimal (parseo invariante) |

Requisitos: SQL Server 2025 (17.x) / Azure SQL (tipo `VECTOR` nativo) con ODBC Driver, **o** PostgreSQL 12+ con la extensión `vector` (`CreateSchema` la crea si el usuario tiene permiso); `OPENAI_API_KEY` en el entorno.

Ejemplo PostgreSQL (el usado con el demo 092):

```ini
[Server]
Protocol=http
Port=8093

[Database]
Driver=postgres
Server=@PGHOST
Port=5432
Database=@PGDATABASE
UserName=@PGUSER
Password=@PGPASSWORD
TableName=rag_live_demo
Entidad=demo092
```

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

- `TFDConnection` (MSSQL o PG) + `TAiRAGVectorMSSQLDriver` o `TAiRAGVectorPostgresDriver` + `TAiRAGVector` + `TAiOpenAiEmbeddings`
- **Conexión perezosa** (`EnsureDb`): el servidor arranca aunque la base esté caída; `CreateSchema` (idempotente) se ejecuta en la primera operación; cada operación reporta el error real de DB
- Con `Driver` asignado, `Search` delega 100% al T-SQL del driver; `AddItem` genera el embedding client-side y hace upsert
- `list_docs`/`delete_doc`/`clear`/`stats` van con SQL directo sobre la tabla usando el metadato `doc`: `JSON_VALUE(properties, '$.doc')` en SQL Server, `properties->>'doc'` en PostgreSQL (`DocExpr`). La memoria es solo cache de sesión y queda vacía tras reiniciar. El driver de PostgreSQL pasa el nombre de tabla a minúsculas (`RagTable`)
- Concurrencia: `TCriticalSection` global serializa todas las operaciones (`TFDConnection` no es thread-safe entre hilos del servidor)

### Operaciones del tool `rag_vector`

| Operation | Params | Notes |
|-----------|--------|-------|
| `index_text` | `textContent`, `docName`, `chunkSize` (800), `overlapPct` (15) | Reindexar borra la versión previa del doc (SQL por metadato). Para documentos cortos dejar 800: con 500 un dato quedó partido entre dos fragmentos |
| `index_file` | `filePath`, `docName` opcional | UTF-8 estricto con fallback ANSI/BOM |
| `search` | `query`, `topK` (5), `minScore` ("0", string) | Score híbrido 0..1 en `Idx`; vector resultado del driver es propietario |
| `list_docs` | — | `GROUP BY` del metadato `doc` (`DocExpr`) |
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

**Estado de pruebas:** compilación, generación del ini, arranque sin DB, los 3 formatos de credenciales (200) y rechazos (401) en HTTP y SSE están verificados en runtime. **Con `Driver=postgres` la ruta de datos completa está probada** (oct 2026: 4/4 preguntas de control con el fragmento correcto entre los dos primeros). **Limitación conocida:** con `Driver=mssql` la ruta de datos contra SQL Server 2025 real no tiene validación runtime; si aparecen reportes, revisar primero el CAST nvarchar→VECTOR vía parámetro FireDAC y FREETEXTTABLE parametrizado.

La descripción del tool usaba nombres de parámetros en snake_case (`text_content`, `top_k`...) distintos de los del schema (`textContent`, `topK`...); se corrigió en oct 2026 porque confundía al modelo.

## Key Gotchas

- El vector resultado de `Search` con driver ES propietario de sus nodos (`Create(nil, True)`) — `ResVec.Free` libera todo. (En modo memoria pura sería no-propietario.)
- `AddItem` copia los metadatos con `Assign`: el caller libera su `TAiEmbeddingMetaData`.
- `ReadFloat` de `TIniFile` depende del locale; los pesos se leen como string y se parsean con `TFormatSettings.Invariant`.
- Consolas FireDAC requieren `FireDAC.ConsoleUI.Wait` en el uses.
- El transporte SSE del framework es experimental; para producción preferir HTTP o StdIO.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
