# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 077 — **RAG vectorial sobre PostgreSQL + pgvector, desde consola y con embeddings locales**.

Los demos de RAG con Postgres (021, 023, 025) son todos gráficos, así que no había forma de ejercitar el stack sin abrir el IDE. Este lo hace headless y con Ollama, de modo que **no necesita ninguna API key**.

| Paso | Qué hace |
|------|----------|
| 1 | Conecta a PostgreSQL y verifica la extensión `pgvector` |
| 2 | Comprueba que Ollama responde y que el modelo de embeddings está descargado |
| 3 | `CreateSchema` (idempotente: tabla + índice HNSW coseno + GIN sobre `properties` + GIN de FTS) |
| 4 | Siembra películas de ejemplo en `pelicula` (omite las ya existentes por título) |
| 5 | Indexa solo las que aún no tienen vector, vía `TAiRAGVectorPostgresDriver` |
| 6 | Búsquedas semánticas |
| 7 | Búsqueda con filtro de metadatos (`TAiFilterCriteria`) |
| 8 | Solo-embeddings vs híbrido (BM25 + RRF) |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `RagPostgresConsoleDemo.dproj`, build Win64.

```bash
RagPostgresConsoleDemo.exe
RagPostgresConsoleDemo.exe --model nomic-embed-text --dim 768
RagPostgresConsoleDemo.exe --no-seed     # no inserta peliculas nuevas
RagPostgresConsoleDemo.exe --reindex     # borra los vectores y reindexa
RagPostgresConsoleDemo.exe --otel        # trazas OTLP a localhost:4318
```

### Configuración (variables de entorno)

| Variable | Default |
|----------|---------|
| `PGHOST` | `localhost` |
| `PGPORT` | `5432` |
| `PGDATABASE` | `peliculasdb` |
| `PGUSER` | `postgres` |
| `PGPASSWORD` | *(obligatoria)* |
| `PGLIBPQ` | autodetección en `C:\Program Files\PostgreSQL\<v>\bin\libpq.dll` |
| `OLLAMA_URL` | `http://localhost:11434/` |

Requiere `ollama pull mxbai-embed-large` (o `nomic-embed-text`).

## Dimensiones: por qué usa su propia tabla

La tabla `pelicula_vector` de esta base es `vector(1536)` porque se indexó con `text-embedding-3-small` de OpenAI. Los modelos de Ollama producen otra dimensión — `mxbai-embed-large` 1024, `nomic-embed-text` 768 — y, más importante, **viven en otro espacio vectorial**: aunque las dimensiones coincidieran, mezclar embeddings de modelos distintos en la misma tabla haría que la similitud coseno devolviera resultados sin sentido.

Por eso el demo crea `pelicula_vector_ollama_<dim>` y no toca la tabla de OpenAI. Cambiar de modelo implica reindexar, siempre.

## Hallazgos observados (25 películas, corpus en español)

Son resultados reales de correr este demo, no teoría:

**1. Qué texto se vectoriza pesa tanto como el modelo.** La primera versión indexaba `"Título (año). Director: X. Género: Y. Sinopsis"`. Como todos los documentos empezaban con la misma plantilla, el embedding quedaba dominado por el boilerplate y los scores se apiñaban entre 0.57 y 0.66. Poniendo el contenido con carga semántica primero (título + sinopsis) y la ficha al final, el top-1 de la consulta de ciencia ficción pasó de **0.62 a 0.795**, con separación clara respecto al segundo.

**2. `nomic-embed-text` se comporta mejor que `mxbai-embed-large` en español.** En *"algo que da miedo en una casa vieja"*, nomic devuelve las tres películas de Terror; mxbai coloca un Drama en primer lugar. Vale la pena probar ambos antes de decidir.

**3. Ambos fallan con modismos.** Ninguno relaciona *"familia política"* con *Mi Suegra Astronauta*, ni *"reencuentro entre familiares"* con *Cicatrices de Sal* (dos hermanas que se reencuentran tras veinte años). Son modelos entrenados sobre todo en inglés: el sentido literal lo capturan, la expresión idiomática no. Para un corpus en español conviene medirlo antes de dar por buena la recuperación.

**4. El híbrido agudiza el ranking aunque no cambie el orden.** En *"origamis que defienden una biblioteca"* los embeddings ya aciertan (0.808), pero con BM25+RRF el acierto sube a 1.000 y la distancia con el segundo se triplica. El aporte no es solo encontrar: es la confianza en lo encontrado.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiRAGVector` | `Source/RAG/uMakerAi.RAG.Vectors.pas` |
| `TAiRAGVectorPostgresDriver` | `Source/RAG/uMakerAi.RAG.Vector.Driver.Postgres.pas` |
| `TAiFilterCriteria`, `TAiEmbeddingMetaData` | `Source/RAG/uMakerAi.RAG.MetaData.pas` |
| `TAiEmbeddingNode` | `Source/RAG/uMakerAi.RAG.Vectors.Index.pas` |
| `TAiOllamaEmbeddings` | `Source/Embeddings/uMakerAi.Embeddings.Ollama.pas` |

## Notas

- **App de consola: hay que enlazar FireDAC a mano.** `FireDAC.Stan.Def`, `.Stan.Async`, `.Stan.ExprFuncs`, `.Phys.PG`, `.Phys.PGDef`, `.DApt`, `.Comp.Client`. Sin eso falla con *"Object factory ... is missing"*.
- `ExecSQLScalar` devuelve `Variant`: `Format` con `%d` lanza `EConvertError`, hay que convertir con `VarToStr` o castear.
- **El filtro de metadatos se resuelve en SQL** sobre la columna `properties` (JSONB con índice GIN), no en memoria: no recalcula vectores ni trae filas de más.
- La búsqueda léxica usa `ts_rank_cd` + `websearch_to_tsquery` con configuración `spanish`, sobre la columna generada `search_vector`. La elige `Driver.Language` (`alSpanish` por defecto).
- El indexado es incremental: consulta qué `pelicula_id` ya tienen vector y solo procesa los que faltan. `--reindex` fuerza el borrado.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
> See [../../Source/RAG/CLAUDE.md](../../Source/RAG/CLAUDE.md) for the RAG module internals.
