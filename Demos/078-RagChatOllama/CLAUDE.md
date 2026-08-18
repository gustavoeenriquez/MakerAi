# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 078 — **RAG completo 100% local: pgvector + Ollama (embeddings *y* chat)**. Sin API keys, sin salir de la máquina.

El [demo 077](../077-RagPostgresConsole/CLAUDE.md) llega hasta recuperar los fragmentos relevantes. Este cierra el ciclo: recupera, arma el contexto y se lo pasa a un LLM local para que responda.

**Cada pregunta se hace dos veces contra el mismo modelo**, y esa es toda la gracia del demo:

| | Qué pasa |
|---|---|
| **SIN contexto** | El modelo no puede conocer un catálogo privado, pero responde igual: inventa títulos, directores y años con total aplomo |
| **CON contexto** | Se recuperan las fichas relevantes de PostgreSQL y se inyectan en el system prompt. Responde sobre datos reales y cita |

## Requisito previo

**Correr antes el demo 077**, que crea el esquema vectorial, siembra películas e indexa. Este demo solo consulta; si la tabla no existe o está vacía, aborta con un mensaje que lo explica.

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `RagChatOllamaDemo.dproj`, build Win64.

```bash
RagChatOllamaDemo.exe                              # 4 preguntas de ejemplo
RagChatOllamaDemo.exe --ask "que peliculas de terror hay?"
RagChatOllamaDemo.exe --chat granite4.1:8b         # otro modelo de chat
RagChatOllamaDemo.exe --embed mxbai-embed-large --dim 1024
RagChatOllamaDemo.exe --think                      # deja el razonamiento activo
RagChatOllamaDemo.exe --debug                      # vuelca request/response a demo078.log
RagChatOllamaDemo.exe --otel
```

Variables de entorno: las mismas que 077 (`PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD`, `OLLAMA_URL`).

## La trampa de los modelos de razonamiento

**Este es el hallazgo importante del demo.** Con `qwen3.5:4b` la respuesta llegaba **vacía**, sin error, tras 40-80 segundos.

La causa: el driver de Ollama traduce `Max_tokens` a **`num_predict`**, y en Ollama ese límite cuenta **todos** los tokens generados — el *thinking* incluido. Con el default de 3000, qwen consumía el presupuesto entero razonando y `message.content` volvía como `""`. Medido con `--debug`: 8325 chunks de `thinking` frente a 10 de `content`, y el chunk final `done:true` con `content` vacío.

Dos salidas, ambas en el demo:

```pascal
// Para un RAG no interesa la cadena de pensamiento sino la respuesta
Chat.Max_tokens := 2000;
Chat.ModelConfig.ModelExtraBodyParams := '{"think": false}';

// O, si se quiere el razonamiento, hay que presupuestarlo
Chat.Max_tokens := 16000;
```

`ModelExtraBodyParams` se mezcla en la raíz del request vía `ApplyExtraBodyParams`, así que sirve para cualquier parámetro que el driver no exponga.

> Ojo: el driver solo envía `think: true` cuando el modelo está registrado con `cap_Reasoning` **y** `ThinkingLevel <> tlDefault`. Nunca envía `think: false`. Para un modelo que no está en el registry (como `qwen3.5:4b`) y que en Ollama razona por defecto, el resultado es una respuesta vacía silenciosa. Vale la pena tenerlo presente al integrar modelos nuevos.

## Resultados observados (qwen3.5:4b, 25 películas)

Corridas reales, no teoría:

| Pregunta | Sin contexto | Con contexto |
|---|---|---|
| *¿Qué películas de terror dirigió Carla Mendez?* | Inventa una biografía completa: "directora argentina más reconocida del género, autora de *Boca* (2017)". En otra corrida la llamó "Carla Fernández", productora mexicana | Las tres correctas: El Bosque Silencioso, Noche de Tormenta, El Susurro del Pozo |
| *¿De qué trata Orbita Cero?* | Película española de Miguel Ángel García, estrenada en 2023, sobre IA y redes sociales | 2024, Marco Velez, tripulación que pierde contacto con la Tierra |
| *Recomiéndame una de animación* | *Spirited Away* — no está en el catálogo | *Mundos Animados* (Estudio Pixelia, 2022) |
| *¿Quién dirigió El Padrino?* (no está en el catálogo) | Coppola, más películas inventadas como suyas | **"No tengo esa información en el catálogo"** — la regla del system prompt funciona |

**Limitaciones honestas de un modelo de 4B:** dice "género terrorista" en vez de "terror", a veces añade una coletilla espuria ("¿No tienes más preguntas?"), y en la pregunta fuera de catálogo acierta la negativa pero luego divaga citando títulos inexactos. Con `--chat granite4.1:8b` la redacción sale más limpia. La recuperación es la misma; lo que cambia es la generación.

## El system prompt es la mitad del trabajo

Las tres reglas de `SYSTEM_RAG` son las que evitan el fallo más común, que el modelo complete con lo que "sabe" cuando el contexto no alcanza:

1. Responder **únicamente** con la información del catálogo.
2. Si no está, decir una frase exacta y predecible.
3. No inventar títulos, directores ni años; citar lo que se use.

Sin la regla 2 el modelo tiende a rellenar; con ella, la respuesta fuera de catálogo es limpia y detectable por código.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiOllamaChat` | `Source/Chat/uMakerAi.Chat.Ollama.pas` |
| `TAiRAGVector.VectorToContextText` | `Source/RAG/uMakerAi.RAG.Vectors.pas` |
| `TAiRAGVectorPostgresDriver` | `Source/RAG/uMakerAi.RAG.Vector.Driver.Postgres.pas` |
| `TAiOllamaEmbeddings` | `Source/Embeddings/uMakerAi.Embeddings.Ollama.pas` |
| `MakerAiDebugLogEnabled` (flag `--debug`) | `Source/Core/uMakerAi.Chat.pas` |

## Notas

- La recuperación va en **híbrido** (embeddings + BM25 con fusión RRF): el sentido lo aportan los vectores y los nombres propios los rescata el léxico.
- `TOP_K = 4`: con un catálogo de 25 alcanza. En un corpus grande conviene subirlo y añadir reranking.
- `VectorToContextText(Res, True, False)` incluye metadatos pero **omite los scores**: al modelo no le aportan y gastan tokens.
- Por defecto usa `nomic-embed-text`, que en las pruebas del 077 se comportó mejor que `mxbai-embed-large` sobre este corpus en español.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
> See [../077-RagPostgresConsole/CLAUDE.md](../077-RagPostgresConsole/CLAUDE.md) for the indexing half.
