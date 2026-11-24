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

**Revisión de VQL, fase 1 (nov 24/2026): resultados correctos.** Plan de 5 fases aprobado por Gustavo (memoria `project_vql_plan`). Errores corregidos, todos comprobados con una consola antes del cambio: (1) **`NOT LIKE`, `NOT BETWEEN`, `NOT CONTAINS`, `NOT STARTS_WITH`, `NOT ENDS_WITH` perdían el `NOT`** y devolvían lo contrario (el parser solo lo usaba delante de `IN`); ahora `TFilterCriterion.Negated` (nuevo, con `TAiFilterCriteria.AddNot`/`AddNotGroup`) y `NOT` antepuesto (`NOT condicion`, `NOT ( ... )`, `TNotExpression`, precedencia entre `AND` y la comparación). Semántica: el `NOT` pospuesto sigue a SQL (un campo que falta no se cumple ni negado, `EvaluateCriterion`); el antepuesto niega el grupo entero de dos valores (un campo que falta adentro cuenta como falso). Los tres drivers SQL lo traducen: condición negada `NOT (...)`, grupo negado `(CASE WHEN (...) THEN 1 ELSE 0 END = 0)` (así el NULL de un campo faltante cuenta como falso, igual que en memoria). (2) **Comparación en memoria** (`TAiEmbeddingMetaData.Evaluate`, la usan VQL y cualquier `Search` con `TAiFilterCriteria`): nuevas funciones públicas `AiCompareValues` (booleanos, fechas, números —también en texto, con punto decimal: `'10' > 9`— y textos sin distinguir mayúsculas y desempatando con ellas) y `AiLikeMatch` (`%`, `_` y `\` como en SQL; antes `MatchesMask`: no distinguía mayúsculas y tomaba `*`, `?` y `[` como comodines). `>=`/`<=` sobre texto nunca se cumplían. Decisión de Gustavo: mayúsculas como SQL (`=`, `IN` y `LIKE` distinguen; `ILIKE`, `CONTAINS`, `STARTS_WITH`, `ENDS_WITH` no). Un número contra un texto no numérico no se compara (falso). (3) **VQL ya no toca el componente**: `ExecuteRequest` copiaba sus opciones a `SearchOptions` (quedaban fijas), cambiaba `LexicalLanguage` y `FEntidad` durante la consulta (no seguro con hilos). Ahora arma un `TAiSearchOptions` por consulta y llama a `SearchWith` (nuevo, público: `Search` con opciones y entidad propias; `Search` lo llama con las del componente). Decisión de Gustavo: **sin `USING` se usan las `SearchOptions` del componente** (antes se imponía híbrido 70/30 aunque no hubiera índice BM25: una coincidencia perfecta puntuaba 0.7); `FUSION`, `THRESHOLD SEMANTIC/LEXICAL` y `OPTIMIZE` solo cambian lo que escriben. `LANGUAGE` ya no hace nada en memoria (el idioma lo fija el índice BM25 al indexar). `REORDER ABC` se aplica al final, sobre lo que se entrega (antes, dentro de la búsqueda, y el `RERANK`/`DISTINCT`/`OFFSET`/`LIMIT` cortaban los extremos que ABC había puesto ahí). (4) Literales decimales: `GetLiteralValue` usaba el separador de Windows: con coma decimal, `150.5` era 0. El lexer rechaza `1.2.3` (antes 0) y el compilador un campo o valor solo (`WHERE estado` se ignoraba); `LIMIT` entero > 0 (`LIMIT 0` se ignoraba y salían 10), `OFFSET` entero >= 0. (5) `ORDER BY` compara como los filtros (`'10'` después de `'9'`), lo que falta va al final y los empates conservan el orden de relevancia (el `Sort` de `TList` no es estable). `EXPLAIN` muestra el modo y la fusión que se van a usar (los del componente sin `USING`). Suite: `rag.vql.not`, `compare`, `like`, `errors`, `orderby`, `decimal`, `options`.

**VQL fase 2 (nov 24/2026): sintaxis.** Lexer: comillas `'...'`/`"..."` con la comilla repetida para escribirla (`'O''Brien'`, como SQL); **la barra invertida es un carácter más** (decisión por las rutas de archivo en los metadatos: `'C:\docs\nuevo'` llega tal cual; en `LIKE` sigue escapando `%` y `_`); cadena o comentario sin cerrar = error (antes se aceptaba hasta el final); negativos (`-5`); comentarios `--`, `//` y `/* */`; `;` final; `!=` como `<>`. Parser: después de `SEARCH` las cláusulas van **en cualquier orden y una vez cada una** (`rag.vql.duplicate_clause`; antes el orden era fijo y `RETURN ... ORDER BY` era error); `TOP n` = `LIMIT n`; las palabras de cláusula valen como nombre de campo donde se espera un campo (`VQL_WORD_TOKENS`: `WHERE language = 'es'`, `ORDER BY order`; `ParseFieldName`, también en `DISTINCT ON` y `ORDER BY`, con `.partes`); `EXISTS(campo)` (la clave está, aunque sea null), `campo EXISTS_ANY (...)` / `EXISTS_ALL (...)` (lista del campo con alguno / todos, `TInExpression.ListOp`) y `NOT` delante de ambos; `CONTAINS` sobre una lista en memoria es pertenencia (antes `VarToStr` de un arreglo lanzaba excepción). Mensajes: `Expect` ya no lleva texto a mano en español con el nombre del enumerado (`tkEOF`): `rag.vql.expected` con `VqlTokenDisplay`; el error del lexer es `EVGQLParserError` (antes `Exception` con la clave de GQL). Memoria: `ParseOr`/`ParseAnd`/`ParseComparison`/`ParsePrimary` liberan lo ya armado si la parte siguiente falla. **Fuga anterior encontrada con `rag.vql.no-leak`:** `TAIBm25Index.Destroy` no liberaba `FStopWords`, y cada `TAiRAGVector` (también el que crea cada `Search` para el resultado) trae su índice BM25: **toda búsqueda perdía 2 bloques** (servidores de vida larga, demo 037). Suite: `rag.vql.syntax`, `syntax-errors`, `messages`, `no-leak`.

**VQL fase 3 (nov 24/2026): parámetros `$x`.** Nuevas sobrecargas `TAiRAGVector.ExecuteVQL(AVql, ['nombre', valor, ...] [, out AResult])` y con `TDictionary<string, Variant>`; `TVGQLParser.Create(AText, AParams)` copia los parámetros a un diccionario propio sin distinguir mayúsculas (nombre con o sin `$`). Misma convención que MakerGQL. **El valor nunca pasa por el lexer**: una comilla o un `' OR '1'='1` en el texto del usuario se compara como texto, y `SEARCH $q` con `x' WHERE ...` busca ese texto entero sin agregar filtros. Valen en `SEARCH`, `RERANK`, `MATCH ($e)`, el `WHERE` (`TLiteralExpression.Data`/`HasData`: el valor llega con su tipo, sin pasar por texto ni por el separador decimal), `IN $lista` / `EXISTS_ANY $lista` / `EXISTS_ALL $lista` (una `VarArrayOf`; un valor suelto es una lista de uno; vacía = error), `BETWEEN $a AND $b`, `LIMIT`/`TOP`/`OFFSET`, `THRESHOLD`, `WITH WEIGHTS($a, $b)` y `MMR($l)`. Errores: parámetro sin valor (`rag.vql.param_missing`), lista fuera de `IN` (`param_list`), tipo equivocado (`param_type`), pares incompletos (`param_pairs`), `$` solo (error del lexer). Suite: `rag.vql.params` (15 consultas, incluidas las dos "inyecciones") y el mensaje en `rag.vql.messages`.

**VQL fase 4 (nov 24/2026): PostgreSQL = memoria.** Probado con `Tests/Integration/071-VqlPostgresTest` (48 consultas corridas en memoria y en la base; antes 21 diferían). `TJSONBFilterCondition.ToSQL` reescrito con la semántica de `AiCompareValues`/`Evaluate`, todo como literal y **sin el operador `?` de JSONB** (FireDAC lo toma por un parámetro: `EXISTS`, `EXISTS_ANY`/`ALL` fallaban con "Parameter [] data type is unknown"). Números: el texto del campo se convierte solo si parece número (expresión regular; si no, NULL y no se cumple); antes `::integer`/`::numeric` directo hacía fallar la consulta entera si una fila tenía `'N/A'`, y `::integer` fallaba con `42000.0`. Un texto que parece número compara como número si el campo también lo es (`CASE`); un texto que no es número no se compara con un número JSON (`jsonb_typeof`). Texto: `=` exacto; el orden con `upper(...) COLLATE "C"` y desempate con mayúsculas (como `CompareText`; la memoria solo pliega ASCII y `upper` de PostgreSQL también acentos: diferencia posible con letras acentuadas al ordenar). `CONTAINS`/`STARTS_WITH`/`ENDS_WITH` sin mayúsculas ni comodines (`position`, `left`, `right`; antes `CONTAINS` era `@>`: igualdad exacta del valor y nunca encontraba un fragmento). Listas: `CONTAINS` y `EXISTS_ANY`/`EXISTS_ALL` recorren los elementos con `jsonb_array_elements_text` (antes `?|`/`?&` miraban las claves del objeto). Fechas: un `TDateTime` (parámetro) compara como `timestamp` si el campo parece fecha. `NOT`: `((campo) IS NOT NULL AND NOT COALESCE(..., FALSE))`, como en memoria. Las claves van entre comillas escapadas (`BuildPath`/`BuildJsonPath`; antes se pegaban tal cual en el SQL). **Metadatos con listas:** `TAiEmbeddingMetaData.ToJSON` lanzaba `EVariantTypeCastError` con un `VarArrayOf` (un nodo con `tags` no se podía guardar en ningún driver) y `FromJSON` perdía los arreglos; ahora `AiVariantToJSON`/`AiJSONToVariant` (afecta también al grafo y a los archivos). **Pendiente:** los drivers de SQL Server y SQLite conservan la semántica vieja (`CONTAINS` como igualdad, `LIKE` sin distinguir mayúsculas por la collation); SQLite lo usan el demo 070 y el cap 13 del libro. Suite: `rag.metadata.json-list`.

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

**MakerGQL fase 6 (nov 23/2026): comandos, `EXPLAIN` y búsqueda semántica.** Comandos nuevos en `TGraphCommandType` (`cmdShowSchema`, `cmdShowProperties` con `CommandLabel`, `cmdCommunities`, `cmdNeighbors`, `cmdCentralityTop`, `cmdAllShortestPaths`) ejecutados en `ExecuteMakerGQL`: texto para el LLM en inglés fijo (como el resto de los textos que lee el modelo) y filas con `'type'` + `TGQLValue`/`TGQLPath`. `SHOW SCHEMA`/`SHOW PROPERTIES` y `GET CENTRALITY TOP` recorren `FNodeRegistry`; con driver, desde nov 24/2026 se cuentan en la base o se carga todo antes (ver "MakerGQL contra PostgreSQL"). La centralidad sale con punto decimal en cualquier idioma (antes `FormatFloat` sin formato invariante daba coma). `EXPLAIN` (`Parser.Explain`, `TAiRagGraph.GQLExplain`) simula el orden de enlace del buscador: ancla con su cantidad de candidatos, cada expansión (desde el origen o "backwards" desde un destino ya enlazado, saltos), productos cartesianos, caminos, `WHERE`, `OPTIONAL MATCH`, `DEPTH` y cada etapa `WITH`/`RETURN`. Semántica: `TSimilarExpr` (`n [NOT] SIMILAR TO 'texto' [> | >= umbral]`, por defecto `>= 0.5`; el destino debe ser una variable) y `similarity(n, 'texto')`, coseno entre `TAiEmbeddingNode.Data` y el embedding del texto, calculado una vez por expresión (`Cache`/`EmbedCache` en el árbol, sin estado compartido en el grafo); un nodo sin vector (en ceros) da NULL; sin `Embeddings` asignado, error `rag.gql.no_embeddings`. `TO` es palabra reservada: `SIMILAR TO` usa `Expect(tkTo)`. Trampa de Delphi: `Strict` no sirve como nombre de campo de clase (es `strict private`). Suite: `rag.gql.schema`, `algos`, `similar` (con `TFixtureHandlers.KeywordEmbedding`, vector por palabras clave), `explain`.

**MakerGQL fase 7 (nov 23/2026): escritura, apagada por defecto.** `TAiRagGraph.AllowWrite` (publicada, `False`): con `False` cualquier cláusula de escritura lanza `rag.gql.write_disabled`. Cláusulas (`TGQLWriteClause` en `TGraphMatchQuery.Writes`, en el orden escrito, después de `MATCH`/`WHERE`/`OPTIONAL MATCH` y antes de `WITH`/`RETURN`; una consulta puede empezar con `CREATE`/`MERGE`): `CREATE` (sus patrones van en una `TGraphMatchQuery` propia, `Pattern`, que no participa del `MATCH`; las variables ya enlazadas se reutilizan; una relación nueva necesita un tipo, una flecha y un salto: `rag.gql.create_edge`), `MERGE` (busca el patrón con `GQLSolve` partiendo de la fila y lo crea si no está; `ON CREATE SET`/`ON MATCH SET`), `SET n.prop = expr` / `SET n:Etiqueta` / `name`, `label`, `weight` (`id` no: `rag.gql.set_id`), `REMOVE n.prop`, `[DETACH] DELETE`. Ejecutor `TAiRagGraph.GQLApplyWrites` dentro de `BeginUpdate`/`EndUpdate` (los índices de nombre y etiqueta se reconstruyen una vez): cada cláusula corre por fila; los nodos nuevos llevan id GUID (o `{id: ...}`), texto con el formato de `TAiRagGraphBuilder` (`Entidad: X. Categoria: Y. La propiedad ...`, son datos para indexar) y embedding si hay `Embeddings`; los modificados recalculan texto y embedding y se guardan con el upsert del driver (`FDriver.AddNode`/`AddEdge`). Los borrados se juntan y se aplican al final: primero las relaciones y después los nodos (`DeleteEdge`/`DeleteNode` avisan al driver desde el fix de borrado de abajo); sin `DETACH`, un nodo con relaciones que no se borran es error (`rag.gql.delete_edges`). `DELETE` con `RETURN` es error de parseo (`rag.gql.delete_return`): las filas apuntarían a objetos liberados. Resultado: sin `RETURN`, el resumen `Nodes created: n, relationships created: n, properties set: n, labels set: n, nodes deleted: n, relationships deleted: n.` (y una fila `type = summary`); con `RETURN`, el resumen, un salto de línea y la tabla o el contexto. No hay transacciones: un error a mitad deja lo anterior aplicado. Suite: `rag.gql.write` (20 pasos: apagada, `CREATE` de nodo y de relación, `MERGE` que encuentra y que crea, `SET` con etiqueta y `RETURN`, `REMOVE`, `DELETE` sin y con `DETACH`, borrar relación y nodo, el nodo creado sale en `SIMILAR TO` y los tres errores de parseo/ejecución). **Criterio de fugas:** las escrituras reconstruyen índices y el conteo de bloques sube y baja unos pocos sin crecer con las vueltas (13 tras 100 ejecuciones de un `MERGE`), así que `rag.gql.no-leak` corre dos tandas de 10 y solo marca fuga si ambas pierden un bloque por ejecución (la fuga inducida de los caminos da `200+200`).

**Borrado de nodos y aristas (fix nov 23/2026, después de la 4.1.0).** `DeleteNode(AID)` liberaba las aristas del nodo con `UnregisterEdge` sin sacarlas de las listas del nodo del otro extremo: el vecino quedaba apuntando a memoria liberada (lo usaba el `DETACH DELETE` de MakerGQL). Ahora las dos sobrecargas de `DeleteNode` hacen lo mismo: borran cada arista con `DeleteEdge` (sale de los dos extremos, se libera y se avisa al driver), liberan el nodo y avisan al driver. Antes `DeleteEdge` nunca avisaba al driver (la fila de PostgreSQL quedaba, porque el `DeleteNode` del driver borra solo la fila del nodo), `DeleteNode(ANode)` no avisaba del nodo, y una arista de un nodo a sí mismo (está en sus dos listas) se liberaba dos veces. Las listas `OutgoingEdges`/`IncomingEdges` del nodo no son dueñas; el dueño es `FEdges`. Suite: `rag.graph.delete` (driver falso `TSuiteGraphDriver` que anota los borrados; falla sin el fix con `b.in=2`).

**MakerGQL contra PostgreSQL (nov 24/2026).** Probado con `Tests/Integration/070-GqlPostgresTest` (50 pasos). Errores corregidos en `TAiRagGraphPostgresDriver`: (1) los `SELECT` de nodos y aristas leían la columna `embedding` como blob (FireDAC mapea el tipo `vector` a `ftBlob`) y `StringToEmbedding` fabricaba un vector de un elemento: `SIMILAR TO`/`similarity()` no encontraban nada con nodos traídos de la base, y un `SET` sobre una relación cargada fallaba al guardarla ("expected 4 dimensions, not 1"); ahora `embedding::text`, el mismo arreglo que tuvo el driver de vectores en sep 2026. (2) El upsert de `AddNode`/`AddEdge` no actualizaba la etiqueta (ni el nombre de la arista): `SET n:Etiqueta` cambiaba la memoria pero no la base. **Operaciones sobre todo el grafo con driver** (decisión de Gustavo): el grafo con driver es una caché perezosa (por etiqueta y por las aristas de cada nodo), así que `MATCH (n)` sin etiqueta, `SHOW SCHEMA` y los algoritmos de todo el grafo veían solo lo cargado (en un grafo recién abierto, nada, y el modelo creía que el grafo estaba vacío). Ahora `SHOW SCHEMA` y `SHOW PROPERTIES` se cuentan en la base con dos virtuales opcionales de `TAiRagGraphDriverBase` (`GetSchemaCounts`, `GetPropertyCounts`; la base devuelve `False` y entonces se carga todo y se cuenta en memoria, así un driver propio sigue funcionando), y `MATCH (n)`, `GET COMMUNITIES`, `GET CENTRALITY`/`CENTRALITY TOP` y `GET DEGREES TOP` llaman a `GQLEnsureFullGraph`, que hace `TAiRagGraph.LoadFromDriver` (nuevo, público: todos los nodos por etiqueta y sus aristas, unas dos consultas por nodo) una vez por consulta (`FGQLFullyLoaded` se reinicia en `ExecuteMakerGQL`). Los métodos públicos `DetectCommunities`, `GetNodesByDegree`, `GetClosenessCentrality` siguen usando solo lo cargado: quien los llame directamente con driver debe llamar antes a `LoadFromDriver`. Los números que llegan por MakerGQL se guardan como `42000.0` en el jsonb (`Variant` Double): para SQL externo, comparar con `::numeric`.

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
