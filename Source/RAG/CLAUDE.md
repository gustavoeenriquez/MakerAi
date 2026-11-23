# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Module Overview

The RAG (Retrieval-Augmented Generation) module provides two complementary retrieval systems:
- **Vector RAG** (`TAiRAGVector`): Embedding-based semantic search with hybrid BM25 support
- **Graph RAG** (`TAiRagGraph`): Knowledge graph with semantic search over nodes and edges

Both systems share common infrastructure for embeddings, metadata filtering, and database drivers.

## Architecture

### Core Components

```text
uMakerAi.RAG.Vectors.Index.pas   # Base classes: TAiEmbeddingNode, TAIEmbeddingIndex, THNSWIndex, TAIBm25Index
uMakerAi.RAG.MetaData.pas        # TAiEmbeddingMetaData, TAiFilterCriteria (filter tree)
uMakerAi.RAG.Vectors.pas         # TAiRAGVector - main vector store component
uMakerAi.RAG.Vectors.VQL.pas     # VGQL query language (Lexer, Parser, Compiler)
uMakerAi.RAG.Graph.Core.pas      # TAiRagGraph, TAiRagGraphNode, TAiRagGraphEdge
uMakerAi.RAG.Graph.GQL.pas       # MakerGQL query language for graphs
uMakerAi.RAG.Graph.Builder.pas   # Graph construction helpers
```

### Inheritance Hierarchy

```text
TAiEmbeddingNode (base embedding container)
├── TAiRagGraphNode (graph node with edges)
└── TAiRagGraphEdge (graph edge with weight)

TAIEmbeddingIndex (abstract index)
├── TAIBasicEmbeddingIndex (brute force)
├── THNSWIndex (HNSW approximate search)
└── TAIEuclideanDistanceIndex (L2 distance)

TAiVectorStoreDriverBase (abstract DB driver)
└── [PostgreSQL driver in uMakerAi.RAG.Vector.Driver.Postgres]

TAiRagGraphDriverBase (abstract graph DB driver)
└── [PostgreSQL driver in uMakerAi.RAG.Graph.Driver.Postgres]
```

## Key Patterns

### Vector Search Options

`TAiSearchOptions` controls hybrid search behavior:
- `UseEmbeddings`: Enable semantic vector search (cosine similarity)
- `UseBM25`: Enable lexical search (BM25 algorithm)
- `UseRRF`: Reciprocal Rank Fusion vs Weighted Score Fusion
- `UseReorderABC`: Lost-in-the-middle reordering for LLM context

### Filter Criteria System

`TAiFilterCriteria` supports recursive filter trees with operators:
- Basic: `foEqual`, `foNotEqual`, `foGreater`, `foLess`, etc.
- Text: `foContains`, `foLike`, `foILike`, `foStartsWith`, `foEndsWith`
- Lists: `foIn`, `foNotIn`, `foBetween`
- Existence: `foIsNull`, `foIsNotNull`, `foExists`

Logical grouping via `loAnd`/`loOr` with nested `AddGroup()`.

### VGQL Query Language (Vector)

SQL-like DSL for vector searches:
```sql
MATCH (documents)
SEARCH 'machine learning algorithms'
USING HYBRID WITH WEIGHTS(0.7, 0.3) FUSION RRF
WHERE category = 'tech' AND date > '2024-01-01'
RERANK 'neural networks' REGENERATE
THRESHOLD GLOBAL 0.5
OPTIMIZE REORDER ABC
RETURN TEXT, METADATA, SCORE
LIMIT 10
```

Execution flow: `TVGQLLexer` → `TVGQLParser` → `TVGQLQuery` (AST) → `TVGQLCompiler` → `TVGQLRequest` → `TAiRAGVector.ExecuteVQL()` (internally `ExecuteRequest`)

### MakerGQL Query Language (Graph)

MakerGQL — Graph Query Language based on ISO/IEC 39075:2024 (GQL standard). Clause order is fixed: `MATCH` → `DEPTH` → `WHERE` → `RETURN`. The exact grammar (and the block to paste into a system prompt) is `MakerGQL_SystemPrompt.md`; keep it in sync when the parser changes.
```gql
MATCH (p:Persona)-[r:TRABAJA_EN]->(e:Empresa)
DEPTH 2
WHERE p.ciudad = $ciudad AND e.empleados BETWEEN 100 AND 5000
RETURN p, r, e
```

Special commands:
- `SHOW SCHEMA` (labels with counts + `(:A)-[:TYPE]->(:B)` triples with counts), `SHOW PROPERTIES [OF label]`, `SHOW LABELS` / `SHOW EDGES`: Introspection
- `SHORTEST PATH (a) TO (b)` (sin `GET`): Dijkstra's algorithm; `ALL SHORTEST PATHS (a) TO (b)`: all minimal paths (directed)
- `GET CENTRALITY (node)` / `GET CENTRALITY TOP n`: Closeness centrality
- `GET DEGREES TOP n`: Hub detection
- `GET COMMUNITIES [TOP n]`: `DetectCommunities`, groups sorted by size
- `GET NEIGHBORS (node) [DEPTH n]`: neighbourhood as context text
- `EXPLAIN <query>`: the plan, without running it

**Revisión de MakerGQL (nov 23/2026, fases 1 y 2 del plan).** Errores corregidos: dos nodos sin variable en un patrón compartían la clave `''` y la consulta no devolvía nada (ahora el parser los nombra `#1`, `#2`…, nombres que el lexer no produce); los patrones con coma solo funcionaban si cada uno arrancaba en una variable ya enlazada (`Match` ahora recorre la cláusula desde el extremo enlazado, o enlaza un ancla nueva, y los patrones de nodo sueltos hacen producto cartesiano); `DEPTH` perdía una `TList` por consulta; `=` lanzaba `EVariantTypeCastError` entre texto y número; las comparaciones y `IN` dependían del separador decimal del Windows (ahora un texto `'42000.5'` compara como número en cualquier idioma, con fechas ISO y booleanos aparte); `LIKE` usaba `MatchesMask` (`[`, `*` y `?` del patrón eran comodines; ahora solo `%` y `_`); `ExecuteMakerGQL(ACode)` liberaba los diccionarios pero no los `TStringWrapper` (`TAiRagGraph.FreeGQLResults` libera ambos); `SHOW LABELS basura` se aceptaba; una cadena sin cerrar se aceptaba. Nuevo: parámetros `$x` (sobrecargas `ExecuteMakerGQL(ACode, ['nombre', valor, ...])` y con `TDictionary<string, Variant>`; el valor no pasa por el lexer, valen en `WHERE`, `{...}`, `IN $lista`, `DEPTH` y `TOP`), `BETWEEN`, `[NOT] STARTS WITH`/`ENDS WITH` (sin distinguir mayúsculas, como `CONTAINS`), `NOT` general y pospuesto (`NOT LIKE`, `NOT BETWEEN`…), `XOR`, negativos, `r.weight` en `WHERE`, escapes (`''`, `\'`, `\n`…), comentarios `//` y `/* */`, `;` final, `` `nombres con espacios` ``, palabras reservadas después del punto (`n.count`) y `EGQLParserError` (hija de `Exception`, con `Position`). `BETWEEN`, `XOR`, `STARTS`, `ENDS` y `WITH` se reconocen por contexto: no son reservadas. Cambio de comportamiento: dos textos numéricos se comparan como números (`'10' > '9'` es verdadero). Suite: `rag.gql.*` (10 casos). Escritura: fase 7 (ver abajo).

**MakerGQL fase 3 (nov 23/2026): funciones y aritmética.** Precedencia `OR < XOR < AND < NOT < comparación < + - < * / % < signo` (`ParseAdditive`/`ParseMultiplicative`/`ParseUnary`; los operandos de las comparaciones, `BETWEEN` e `IN` son expresiones aritméticas). Nodos nuevos del árbol: `TFunctionExpr` (nombre canónico en minúsculas, dueña de `Args`) y `TVariableExpr` (variable suelta; vale su ID). El catálogo está en `GQL_FUNCTIONS` (parser, `uMakerAi.RAG.Graph.GQL.pas`: nombre, aridad y si exige variable) y la evaluación en `TAiRagGraph.EvaluateGQLFunction`; **al agregar una función, tocar ambos** y la tabla de `MakerGQL_SystemPrompt.md`. Nombre desconocido, aridad errónea o una función de grafo con una propiedad (`id(n.name)`) son errores de parseo, para que el LLM reciba el motivo y no un resultado vacío. `+` suma si ambos lados son números (aunque vengan como texto) y si no concatena; `/` y `%` por cero dan NULL; `round` redondea mitades hacia afuera (no el bancario de `Round`); `substring` empieza en 0 como Cypher. `GQLToText` convierte números y fechas sin depender del idioma (`VarToStr(42000.5)` da `"42000,5"` en un Windows en español). `IN` acepta una expresión que dé lista (`'X' IN labels(n)`, `IN split(...)`). Suite: `rag.gql.funcs`, `rag.gql.arith`.

**MakerGQL fase 4 (nov 23/2026): `RETURN` real y `WITH`.** Decisión de Gustavo (opción 1): sin `RETURN` o con `RETURN` de solo variables (`RETURN c`, `RETURN *`) sigue el texto de contexto del subgrafo, sin cambios para el libro ni los demos; con columnas, funciones, alias, agregados, `DISTINCT`, `ORDER BY`, `SKIP`, `LIMIT` o algún `WITH` (`TGraphMatchQuery.Tabular`), el resultado es una tabla Markdown (`GQLRowsToText`, `(N rows)` al final) y `AResultObjects` trae una fila por diccionario: columna → nodo/arista del grafo (no es dueña) o `TGQLValue` (hija de `TStringWrapper`: `Value` el texto, `Data` el Variant; `FreeGQLResults` la libera). Cada `WITH` y el `RETURN` son una `TGQLProjection` (`Projections`); `RunGQLProjection` encadena las etapas: agrupación implícita por las columnas sin agregado, `TAggregateExpr.Value` llenado por grupo antes de evaluar la columna, `WHERE` de `WITH` sobre las columnas antes de ordenar (el `HAVING`), luego `DISTINCT`, `ORDER BY` (NULL al final en ASC, estable), `SKIP`, `LIMIT`. Las columnas de un `WITH` se ven en la etapa siguiente como variables (`TGQLValue` enlazado; `ekVariable` devuelve su `Data`). Agregados solo en `RETURN`/`WITH`/`ORDER BY` y sin anidar (`FAllowAggregates`); `min`/`max`/`collect` y `ORDER`, `BY`, `SKIP`, `LIMIT`, `DISTINCT`, `ASC`, `DESC`, `WITH` son palabras de contexto. El nombre de una columna sin alias es el texto fuente de la expresión (`FPrevEnd`). `DEPTH` no aplica a una tabla. Se quitaron las claves `rag.gql.function_arg` y `rag.gql.invalid_return` (eran del `RETURN` viejo). Suite: `rag.gql.return`, `rag.gql.aggregate`, `rag.gql.with`.

**MakerGQL fase 5 (nov 23/2026): patrones.** El buscador ahora es `TAiRagGraph.GQLSolve` (cláusulas, patrones a combinar al final, `WHERE`, estado inicial, lista de resultados y `AFirstOnly`) y lo usan el `MATCH` principal, cada `OPTIONAL MATCH` (`TGQLOptionalMatch`: sus cláusulas, los patrones nuevos que introdujo, marcados `Optional`, y su `WHERE`) y cada patrón del `WHERE` (`TPatternExpr`, dueña de su propia `TGraphMatchQuery`; `AFirstOnly`). Por eso el parser busca variables en la consulta que arma (`AQuery.NodePatternByVariable`), no en un diccionario global: un patrón del `WHERE` crea sus propios patrones para las variables de afuera, que llegan enlazadas en el estado. `[:A|B]` y `(n:A|B)` guardan la etiqueta con `|` (`GQLLabelMatches`, `GQLCandidates`; token `tkPipe`). Longitud variable: `TMatchEdgePattern.MinHops`/`MaxHops` (-1 = sin máximo, se acota a 10 saltos con `GQL_MAX_HOPS`), recorrido en profundidad sin repetir aristas, también desde el destino (el tramo se invierte al orden del patrón). `TGQLPath` (hija de `TGQLValue`: `Nodes`, `Edges`, texto `(A)-[:X]->(B)<-[:Y]-(C)`) es el tramo de una arista de longitud variable y la variable de camino `p = ...` (`TGraphMatchQuery.Paths`, solo en el `MATCH` principal; si hay caminos, cada arista se enlaza también como `#e<n>` para armarlos). **Propiedad de la memoria:** los tramos se crean en `Temps` de `GQLSolve` y cada fila final se queda con clones (`GQLCloneRow`, `TGQLValue.Clone` virtual), así ninguna fila comparte objetos; `GQLFreeRowValues` los libera. La detección de un patrón en el `WHERE` (`LooksLikePattern`) mira adelante y restaura el lexer: `(n)-[` es arista, `(n) - 1` es una resta. `length`/`size`/`nodes`/`relationships` sobre un camino; `exists(patrón)` devuelve el booleano. El texto de contexto y `DEPTH` incluyen los nodos de los caminos. Suite (fase 5): `rag.gql.alt`, `varlength`, `paths`, `optional`, `exists` y `rag.gql.no-leak` (repite todo el repertorio y exige 0 bloques nuevos por ejecución; detecta una fuga inducida en los caminos).

**MakerGQL fase 6 (nov 23/2026): comandos, `EXPLAIN` y búsqueda semántica.** Comandos nuevos en `TGraphCommandType` (`cmdShowSchema`, `cmdShowProperties` con `CommandLabel`, `cmdCommunities`, `cmdNeighbors`, `cmdCentralityTop`, `cmdAllShortestPaths`) ejecutados en `ExecuteMakerGQL`: texto para el LLM en inglés fijo (como el resto de los textos que lee el modelo) y filas con `'type'` + `TGQLValue`/`TGQLPath`. `SHOW SCHEMA`/`SHOW PROPERTIES` y `GET CENTRALITY TOP` recorren `FNodeRegistry` (lo cargado en memoria, no lo que solo está en el driver). La centralidad sale con punto decimal en cualquier idioma (antes `FormatFloat` sin formato invariante daba coma). `EXPLAIN` (`Parser.Explain`, `TAiRagGraph.GQLExplain`) simula el orden de enlace del buscador: ancla con su cantidad de candidatos, cada expansión (desde el origen o "backwards" desde un destino ya enlazado, saltos), productos cartesianos, caminos, `WHERE`, `OPTIONAL MATCH`, `DEPTH` y cada etapa `WITH`/`RETURN`. Semántica: `TSimilarExpr` (`n [NOT] SIMILAR TO 'texto' [> | >= umbral]`, por defecto `>= 0.5`; el destino debe ser una variable) y `similarity(n, 'texto')`, coseno entre `TAiEmbeddingNode.Data` y el embedding del texto, calculado una vez por expresión (`Cache`/`EmbedCache` en el árbol, sin estado compartido en el grafo); un nodo sin vector (en ceros) da NULL; sin `Embeddings` asignado, error `rag.gql.no_embeddings`. `TO` es palabra reservada: `SIMILAR TO` usa `Expect(tkTo)`. Trampa de Delphi: `Strict` no sirve como nombre de campo de clase (es `strict private`). Suite: `rag.gql.schema`, `algos`, `similar` (con `TFixtureHandlers.KeywordEmbedding`, vector por palabras clave), `explain`.

**MakerGQL fase 7 (nov 23/2026): escritura, apagada por defecto.** `TAiRagGraph.AllowWrite` (publicada, `False`): con `False` cualquier cláusula de escritura lanza `rag.gql.write_disabled`. Cláusulas (`TGQLWriteClause` en `TGraphMatchQuery.Writes`, en el orden escrito, después de `MATCH`/`WHERE`/`OPTIONAL MATCH` y antes de `WITH`/`RETURN`; una consulta puede empezar con `CREATE`/`MERGE`): `CREATE` (sus patrones van en una `TGraphMatchQuery` propia, `Pattern`, que no participa del `MATCH`; las variables ya enlazadas se reutilizan; una relación nueva necesita un tipo, una flecha y un salto: `rag.gql.create_edge`), `MERGE` (busca el patrón con `GQLSolve` partiendo de la fila y lo crea si no está; `ON CREATE SET`/`ON MATCH SET`), `SET n.prop = expr` / `SET n:Etiqueta` / `name`, `label`, `weight` (`id` no: `rag.gql.set_id`), `REMOVE n.prop`, `[DETACH] DELETE`. Ejecutor `TAiRagGraph.GQLApplyWrites` dentro de `BeginUpdate`/`EndUpdate` (los índices de nombre y etiqueta se reconstruyen una vez): cada cláusula corre por fila; los nodos nuevos llevan id GUID (o `{id: ...}`), texto con el formato de `TAiRagGraphBuilder` (`Entidad: X. Categoria: Y. La propiedad ...`, son datos para indexar) y embedding si hay `Embeddings`; los modificados recalculan texto y embedding y se guardan con el upsert del driver (`FDriver.AddNode`/`AddEdge`). Los borrados se juntan y se aplican al final: primero las relaciones (avisando al driver, porque `TAiRagGraph.DeleteEdge` no lo hace) y después los nodos (`DeleteNode(ID)`, que sí avisa); sin `DETACH`, un nodo con relaciones que no se borran es error (`rag.gql.delete_edges`). `DELETE` con `RETURN` es error de parseo (`rag.gql.delete_return`): las filas apuntarían a objetos liberados. Resultado: sin `RETURN`, el resumen `Nodes created: n, relationships created: n, properties set: n, labels set: n, nodes deleted: n, relationships deleted: n.` (y una fila `type = summary`); con `RETURN`, el resumen, un salto de línea y la tabla o el contexto. No hay transacciones: un error a mitad deja lo anterior aplicado. Suite: `rag.gql.write` (20 pasos: apagada, `CREATE` de nodo y de relación, `MERGE` que encuentra y que crea, `SET` con etiqueta y `RETURN`, `REMOVE`, `DELETE` sin y con `DETACH`, borrar relación y nodo, el nodo creado sale en `SIMILAR TO` y los tres errores de parseo/ejecución). **Criterio de fugas:** las escrituras reconstruyen índices y el conteo de bloques sube y baja unos pocos sin crecer con las vueltas (13 tras 100 ejecuciones de un `MERGE`), así que `rag.gql.no-leak` corre dos tandas de 10 y solo marca fuga si ambas pierden un bloque por ejecución (la fuga inducida de los caminos da `200+200`).

**`SearchText` (fix oct 23/2026).** Solo devolvía el encabezado "Contexto extraído del grafo de conocimiento:": el cuerpo había quedado como un esbozo y quien lo usaba (demo 025, o un chat que arma el contexto con él) le pasaba al LLM un contexto vacío. Ahora devuelve `GetContextualizedText` de los nodos encontrados (con sus vecinos hasta `ADepth`) y sus relaciones. Suite: `rag.graph.searchtext`.

**GraphML desde una consola (oct 24/2026).** `SaveToFile(..., gefGraphML)` usa `TXMLDocument`. En una app FMX/VCL el vendor DOM se registra solo; en un programa `{$APPTYPE CONSOLE}` falla con "Microsoft MSXML is not installed" si no se agregan `Xml.Win.msxmldom` al `uses` y `CoInitialize(nil)` al arrancar (ver `Demos/Libro/cap14-grafos/GrafosLadera.dpr`). `gefDOT` y `gefGraphMkai` no lo necesitan.

**Patrones convergentes en MakerGQL.** Dos aristas que llegan al mismo nodo se escriben con flecha invertida, `MATCH (p:Persona)-[:NO_LE_GUSTO]->(a:Acidez)<-[:TIENE_ACIDEZ]-(c:Producto) RETURN c`, o con coma reutilizando la variable, `MATCH (c)-[:TIENE_ACIDEZ]->(a), (p)-[:NO_LE_GUSTO]->(a)`. La forma con coma devolvía "No matching subgraphs found" hasta el arreglo de nov 23/2026 (caso `rag.gql.comma`).

## Thread Safety

- `TAiRAGVector` uses `TMultiReadExclusiveWriteSynchronizer` for concurrent access
- Search results use `TAiSearchResult` record to avoid modifying original nodes
- Index operations (HNSW) are write-locked during updates

## Component Registration

Both `TAiRAGVector` and `TAiRagGraph` register under the `MakerAI` palette category:
```pascal
RegisterComponents('MakerAI', [TAiRAGVector]);
RegisterComponents('MakerAI', [TAiRagGraph]);
```

## Graph Export Formats

`TAiRagGraph.SaveToFile()` supports:
- `gefDOT`: GraphViz format
- `gefGraphML`: Standard XML format (compatible with Gephi)
- `gefGraphMkai`: Native JSON format with embeddings

## Database Drivers

External persistence requires implementing:
- `TAiVectorStoreDriverBase.Add()`, `Search()`, `Delete()`, `Clear()`
- `TAiRagGraphDriverBase.FindNodeByID()`, `AddNode()`, `AddEdge()`, `SearchNodes()`, etc.

PostgreSQL implementations use pgvector extension for vector similarity.

## Reranker semántico (sep 27/2026)

`TAiRAGVector.Reranker: TAiRAGRerankerBase`. En `ExecuteRequest` (paso 5, `RERANK` en VQL) reemplaza el rerank por coseno: `RerankWith(query, reranker)` pone `Idx := puntaje` y ordena, sin tocar embeddings (tampoco los regenera). Después se descartan los nodos con puntaje negativo (el reranker los marca para eliminarlos siempre, p.ej. inyección de prompt) y, si `MinScore > 0`, los que quedan por debajo. **Si el reranker lanza, la búsqueda no falla: cae al rerank por coseno.** `RerankWith` es público para usarlo fuera de VQL. Contrato de `Score(query, textos)`: un puntaje por texto, en el mismo orden. Implementaciones: `TAiJevRAGReranker` (`Source/Tools/uMakerAi.Jev.RAG.pas`, evidencia + descarte de inyecciones) y `TAiQwenRAGReranker` (`Source/Tools/uMakerAi.Qwen.Rerank.pas`, cross-encoder `qwen3-rerank`, hasta 500 pasajes por llamada).

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for source directory overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
