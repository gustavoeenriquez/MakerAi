# 070 - ChatRAGTools

Demo FMX que combina, en un solo formulario, las piezas centrales de MakerAI:

- **Chat multi-proveedor** (`TAiChatConnection`) con selector de **proveedor** y **modelo**.
- **Memos** de prompt (entrada) y de chat (salida), con streaming.
- **RAG local con SQLite** (`TAiRAGVector` + `TAiRAGVectorSQLiteDriver` + `TFDConnection`).
  Búsqueda vectorial en Delphi puro (no requiere la extensión nativa `vec0`).
  Embeddings vía OpenAI (`text-embedding-3-small`).
- **TAiFunctions** con dos funciones que la IA puede invocar:
  - `GetFechaHora` → devuelve la fecha/hora actual (ISO 8601).
  - `GraficarSerie` → recibe `{titulo, puntos:[{fecha,valor}]}` y dibuja la serie en el TeeChart.
- **TabControl** con tres paneles:
  - **Web (Edge)** → `TWebBrowser` (WebView2/Edge en Windows).
  - **Gráfica (TeeChart)** → `TChart` + `TLineSeries` con eje X de tiempo.
  - **Log** → `TMemo` con la traza (tokens, llamadas a tools, RAG).

> El formulario se construye **"code-first"**: todos los componentes se crean y
> cablean en `FormCreate` (ver `uMain.pas`), por lo que el `.fmx` es mínimo y el
> wiring queda visible y didáctico.

## Requisitos para ejecutar

- Variable de entorno **`OPENAI_API_KEY`** (para embeddings del RAG y, si usas
  OpenAI como proveedor de chat). Otros proveedores requieren su propia key
  (`@CLAUDE_API_KEY`, `@GEMINI_API_KEY`, etc.) — el demo usa la convención `@VAR`.
- Para el panel Web: **WebView2 Runtime** (Edge) instalado (estándar en Windows 10/11).

## Uso

1. Compila y ejecuta (`Win64`).
2. Elige **proveedor** y **modelo** en los combos.
3. Escribe un prompt y pulsa **Enviar**. Prueba:
   - *"¿Qué fecha y hora es?"* → la IA llama a `GetFechaHora`.
   - *"Grafica la temperatura: hoy 20, mañana 22, pasado 19"* → la IA llama a
     `GraficarSerie` y el resultado aparece en la pestaña **Gráfica**.
4. **RAG**: pega texto en el prompt y pulsa **"Cargar texto/archivo a RAG..."**
   (o elige un `.txt`). Luego marca **"Usar RAG"** y haz una pregunta: el demo
   recupera el contexto desde SQLite y lo antepone al prompt.

## Archivos

- `ChatRAGTools.dpr` / `.dproj` — proyecto.
- `uMain.pas` — toda la lógica (UI + MakerAI + RAG + tools).
- `uMain.fmx` — formulario mínimo (`OnCreate`).
- `rag_demo.db` — base SQLite del RAG (se crea junto al ejecutable).
