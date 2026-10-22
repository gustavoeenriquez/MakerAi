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

## Prueba automática y grabación de video (oct 22/2026)

El formulario acepta un gancho para manejarlo sin teclado ni mouse (lo usa el skill de videos de la serie de Café La Ladera):

```bash
ChatRAGTools.exe --driver OpenAi --model gpt-6-sol --prueba --tecleo --pausa 4000 --tamano 1280x720 \
  --marcas marcas.txt "Grafica las bolsas de café que vendimos la semana pasada: lunes 12, ..." "¿Qué fecha es hoy?"
```

Una entrada `"@rag archivo.txt"` en la lista pega el texto del archivo en el prompt, lo indexa como el botón **Cargar texto/archivo a RAG...** y marca **Usar RAG** para las preguntas siguientes. Así se grabó el episodio 8 (RAG), con el manual ficticio `ladera_tienda.txt` y la base borrada antes (`rag_demo.db` se acumula entre corridas):

```bash
ChatRAGTools.exe --driver OpenAi --model gpt-6-sol --prueba --tecleo --pausa 4000 --tamano 1280x720 \
  "¿Cuánto cuesta el envío a Medellín y en cuántos días llega?" "@rag C:\ruta\ladera_tienda.txt" \
  "¿Cuánto cuesta el envío a Medellín y en cuántos días llega?"
```

`--tecleo` escribe cada pregunta letra a letra, `--pausa` espera tras cada respuesta, `--tamano` fija el área cliente y `--marcas` anota (hora UTC en ms) cuándo se teclea, se envía y termina cada turno. Al terminar la lista, la ventana se cierra.

Arreglos de esa fecha: `Tool_Active` por el canal tipado (antes por `Params`); las fechas de la gráfica ya no se corren al convertirlas a hora local (`TryISO8601ToDate(..., True)`); la gráfica muestra puntos, una marca por día y sin leyenda; el log de `GetFechaHora` copia el valor antes de encolarse (salía vacío). El esquema de `GraficarSerie` (arreglo de objetos) daba 400 en la API Responses: era un bug del framework, corregido en `EnforceStrictSchema`. Después, para el episodio 8: prompt de sistema que pide texto plano (el chat es un `TMemo` y los `**` del Markdown se veían tal cual) y el log de tokens suma los del caché (`Prompt_tokens` son solo los nuevos).
