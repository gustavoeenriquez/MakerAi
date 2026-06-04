# TAiChatMode — Modos de operación del chat

**MakerAI v3.3 — Documentación técnica**
Última actualización: abril 2026

---

## Índice

1. [Introducción](#1-introducción)
2. [Referencia del enum](#2-referencia-del-enum)
3. [El flujo interno de RunNew: tres fases](#3-el-flujo-interno-de-runnew-tres-fases)
4. [cmConversation — Modo inteligente (default)](#4-cmconversation--modo-inteligente-default)
5. [Modos forzados — Llamada directa sin gap analysis](#5-modos-forzados--llamada-directa-sin-gap-analysis)
   - [cmImageGeneration](#51-cmimagegeneración)
   - [cmVideoGeneration](#52-cmvideogeneration)
   - [cmSpeechGeneration](#53-cmspeechgeneration)
   - [cmTranscription](#54-cmtranscription)
   - [cmWebSearch](#55-cmwebsearch)
   - [cmReportGeneration](#56-cmreportgeneration)
6. [cmSmartDispatch — Despacho inteligente](#6-cmsmartdispatch--despacho-inteligente)
7. [Tabla comparativa](#7-tabla-comparativa)
8. [Cómo elegir el modo correcto](#8-cómo-elegir-el-modo-correcto)
9. [Cambio de modo en tiempo de ejecución](#9-cambio-de-modo-en-tiempo-de-ejecución)
10. [Referencia de archivos fuente](#10-referencia-de-archivos-fuente)

---

## 1. Introducción

`TAiChatMode` es la propiedad que controla cómo `TAiChat.RunNew` procesa cada mensaje. Determina:

- Qué **bridges de entrada** se activan (transcripción, descripción de imagen, extracción PDF)
- Qué **función de salida** se ejecuta (completions, generación de imagen, TTS, búsqueda web, etc.)
- Si se aplica o no el **gap analysis** de capacidades (`ModelCaps`/`SessionCaps`)

Se configura en `TAiChat.ChatMode` o en `TAiChatConnection.ChatMode`. El valor por defecto es `cmConversation`.

```pascal
Conn.ChatMode := cmConversation;    // default — orquestación automática
Conn.ChatMode := cmImageGeneration; // forzar: siempre genera imagen
Conn.ChatMode := cmSpeechGeneration;// forzar: siempre genera voz
Conn.ChatMode := cmSmartDispatch;   // despacho para modelos sin function calling
```

---

## 2. Referencia del enum

Definido en `Source/Core/uMakerAi.Chat.pas`:

```pascal
TAiChatMode = (
  cmConversation,      // Orquestación inteligente con gap analysis (default)
  cmImageGeneration,   // Forzar generación de imagen (salta gap analysis)
  cmVideoGeneration,   // Forzar generación de video (salta gap analysis)
  cmSpeechGeneration,  // Forzar texto a voz / TTS (salta gap analysis)
  cmTranscription,     // Forzar transcripción de audio (salta gap analysis)
  cmWebSearch,         // Forzar búsqueda web (salta gap analysis)
  cmReportGeneration,  // Forzar generación de reporte PDF/HTML/XLSX
  cmSmartDispatch      // Despacho en dos pasos para modelos sin function calling
);
```

### Clasificación

| Categoría | Modos |
|-----------|-------|
| **Inteligente** | `cmConversation` |
| **Forzado** (directo, sin gap analysis) | `cmImageGeneration`, `cmVideoGeneration`, `cmSpeechGeneration`, `cmTranscription`, `cmWebSearch`, `cmReportGeneration` |
| **Despacho** | `cmSmartDispatch` |

---

## 3. El flujo interno de RunNew: tres fases

Todos los modos pasan por el mismo método `TAiChat.RunNew`. El `ChatMode` controla qué partes se ejecutan:

```text
RunNew(AskMsg)
  │
  ├── FASE 1: Bridge de entrada  ←── solo cmConversation
  │   Procesa archivos adjuntos con gap analysis:
  │   • Audio sin cap_Audio  → InternalRunTranscription
  │   • Imagen sin cap_Image → InternalRunImageDescription
  │   • PDF sin cap_Pdf      → InternalRunPDFDescription
  │
  ├── FASE 2: Grounding web  ←── todos excepto cmSmartDispatch
  │   Si cap_WebSearch en Gap → InternalRunWebSearch
  │
  └── FASE 3: Orquestación de salida  ←── todos los modos
      cmConversation:    gap analysis → rama correcta
      cmImageGeneration: InternalRunImageGeneration (directo)
      cmVideoGeneration: InternalRunImageVideoGeneration (directo)
      cmSpeechGeneration:InternalRunSpeechGeneration (directo)
      cmTranscription:   InternalRunTranscription (directo)
      cmWebSearch:       InternalRunWebSearch (directo)
      cmReportGeneration:InternalRunReport (directo)
      cmSmartDispatch:   InternalRunSmartDispatch (dos pasos)
```

**Modos forzados** (Fase 3) saltan directamente a su función sin pasar por gap analysis ni Fases 1 o 2.

---

## 4. cmConversation — Modo inteligente (default)

### Qué hace

El modo por defecto. Aplica las tres fases completas y usa el **gap analysis** (`SessionCaps − ModelCaps`) para decidir automáticamente qué función ejecutar. Es el modo más flexible y el recomendado para aplicaciones de producción.

### Gap analysis en Fase 3

```text
cap_GenVideo  en Gap  →  InternalRunImageVideoGeneration
cap_GenImage  en Gap  →  InternalRunImageGeneration
cap_GenAudio  en Gap  →  InternalRunSpeechGeneration
cap_GenReport en Gap  →  InternalRunReport
(ninguno)             →  InternalRunCompletions  ← caso más común
```

### Cuándo usarlo

- Aplicaciones de chat conversacional
- Cuando el modelo puede generar texto, imágenes o audio según la configuración de capacidades
- Cuando se quiere que el framework decida automáticamente el bridge correcto

### Ejemplo

```pascal
Conn.DriverName := 'OpenAI';
Conn.Params.Values['Model']  := 'gpt-4.1';
Conn.ChatMode := cmConversation;  // default — no es necesario especificarlo

// Con SessionCaps=[cap_GenImage], el gap analysis activa generación de imagen
// automáticamente cuando el modelo no tiene cap_GenImage nativo
Conn.Params.Values['SessionCaps'] := '[cap_GenImage]';
```

### Archivos adjuntos con gap analysis (Fase 1)

`cmConversation` es el único modo que procesa archivos automáticamente en Fase 1:

```pascal
// Si el modelo no tiene cap_Audio (gap), el framework transcribe el audio antes de enviarlo
Conn.ChatMode := cmConversation;
Resp := Conn.AddMessageAndRun('Transcribe esto', 'user', [AudioMediaFile]);
// → Internamente: transcripción → insertar texto en prompt → completions
```

---

## 5. Modos forzados — Llamada directa sin gap analysis

Los modos forzados **saltan el gap analysis** y llaman directamente a la función de salida correspondiente. Son útiles cuando:

- Sabes de antemano qué tipo de salida quieres
- Quieres una API más explícita sin depender de la configuración de capacidades
- Estás construyendo una interfaz dedicada (p.ej., sólo TTS, sólo generación de imágenes)

> **Importante:** Los modos forzados también **saltan la Fase 1** (bridge de entrada de archivos). Si necesitas que el framework procese archivos adjuntos automáticamente, usa `cmConversation`.

### 5.1 cmImageGeneration

Genera una imagen a partir del mensaje del usuario. Requiere un `ImageTool` asignado en `ChatTools`, o que el modelo tenga capacidad nativa de generación de imágenes (`cap_GenImage` en `ModelCaps`).

```pascal
Conn.ChatMode := cmImageGeneration;
Conn.ChatTools.ImageTool := MiDalleImageTool;

// Cada llamada genera una imagen, sin importar el contenido del prompt
Resp := Conn.AddMessageAndRun('Un paisaje montañoso al atardecer', 'user', []);
// → InternalRunImageGeneration directo
```

**Requiere:** `ChatTools.ImageTool` asignado, o modelo con `cap_GenImage` nativo (ej: `gemini-2.5-flash-image`).

---

### 5.2 cmVideoGeneration

Genera un video a partir del mensaje. Requiere un `VideoTool` asignado (`TAiGeminiVideoTool`, `TAiSoraGenerator`, etc.).

```pascal
Conn.ChatMode := cmVideoGeneration;
Conn.ChatTools.VideoTool := MiVideoTool;

Resp := Conn.AddMessageAndRun('Una ola rompiendo en la orilla', 'user', []);
// → InternalRunImageVideoGeneration directo
```

**Nota:** La generación de video es inherentemente asíncrona y puede tardar minutos. El resultado llega vía `OnReceiveDataEnd`.

---

### 5.3 cmSpeechGeneration

Convierte el texto del mensaje a audio (TTS). Requiere un `SpeechTool` asignado o modelo con TTS nativo.

```pascal
Conn.ChatMode := cmSpeechGeneration;
Conn.ChatTools.SpeechTool := MiSpeechTool;

Resp := Conn.AddMessageAndRun('Bienvenido a MakerAI', 'user', []);
// → InternalRunSpeechGeneration directo
// El audio llega en ResMsg.MediaFiles
```

**Diferencia con cmConversation:** En `cmConversation`, el texto pasa primero por el LLM y la respuesta generada se convierte a audio. En `cmSpeechGeneration`, el texto del mensaje del usuario se convierte directamente sin pasar por completions.

---

### 5.4 cmTranscription

Transcribe el primer archivo de audio encontrado en los archivos adjuntos del mensaje.

```pascal
Conn.ChatMode := cmTranscription;
Conn.ChatTools.SpeechTool := MiWhisperTool;

Resp := Conn.AddMessageAndRun('', 'user', [AudioMediaFile]);
// → InternalRunTranscription directo sobre el primer archivo de audio
// La transcripción llega en Resp y en AudioMediaFile.Transcription
```

**Nota:** Procesa sólo el **primer** archivo de audio encontrado y se detiene. Si hay varios archivos, usa `cmConversation` con gap analysis para procesar todos.

---

### 5.5 cmWebSearch

Ejecuta directamente una búsqueda web con el texto del mensaje como consulta.

```pascal
Conn.ChatMode := cmWebSearch;
Conn.ChatTools.WebSearchTool := MiGeminiWebSearchTool;

Resp := Conn.AddMessageAndRun(
  'últimas noticias inteligencia artificial 2025', 'user', []);
// → InternalRunWebSearch directo
// Las citas llegan en ResMsg.Citations
```

**Diferencia con cmConversation:** En `cmConversation` con `cap_WebSearch` en gap, primero busca en la web y luego el LLM sintetiza. En `cmWebSearch`, la respuesta del tool de búsqueda llega directamente al usuario sin síntesis adicional.

---

### 5.6 cmReportGeneration

Genera un reporte (PDF, HTML, XLSX) a partir del contenido del mensaje. Requiere un `ReportTool` asignado.

```pascal
Conn.ChatMode := cmReportGeneration;
Conn.ChatTools.ReportTool := MiReportTool;

Resp := Conn.AddMessageAndRun('Genera un reporte de ventas del Q1', 'user', []);
// → InternalRunReport directo
// El reporte llega en ResMsg.MediaFiles
```

---

## 6. cmSmartDispatch — Despacho inteligente

Modo de dos pasos para modelos sin function calling nativo. El LLM clasifica la solicitud en una etiqueta de texto plano (`[IMAGEGEN]`, `[WEBSEARCH]`, `[CHAT]`, etc.) y el framework ejecuta la herramienta correspondiente.

**Requisito exclusivo:** `Asynchronous = False`.

```pascal
Conn.ChatMode := cmSmartDispatch;
Conn.Params.Values['Asynchronous'] := 'False';  // obligatorio
```

> Ver documentación completa en [uMakerAi-ChatTools-SmartDispatch.md](uMakerAi-ChatTools-SmartDispatch.md).

---

## 7. Tabla comparativa

| Modo | Gap analysis | Fase 1 (archivos) | Fase 2 (web) | Async soportado | Uso típico |
|------|:---:|:---:|:---:|:---:|------------|
| `cmConversation` | ✅ Completo | ✅ | ✅ | ✅ | Chat general, producción |
| `cmImageGeneration` | ❌ Directo | ❌ | ❌ | ✅ | UI dedicada de imágenes |
| `cmVideoGeneration` | ❌ Directo | ❌ | ❌ | ✅ | UI dedicada de video |
| `cmSpeechGeneration` | ❌ Directo | ❌ | ❌ | ✅ | UI de TTS, lector de textos |
| `cmTranscription` | ❌ Directo | ❌ | ❌ | ✅ | UI de transcripción |
| `cmWebSearch` | ❌ Directo | ❌ | ❌ | ✅ | Buscador dedicado |
| `cmReportGeneration` | ❌ Directo | ❌ | ❌ | ✅ | Generador de reportes |
| `cmSmartDispatch` | ❌ (routing LLM) | ❌ | ❌ | ❌ **No** | Modelos locales sin function calling |

---

## 8. Cómo elegir el modo correcto

### ¿El usuario puede hacer preguntas de cualquier tipo?
→ **`cmConversation`** — el framework decide qué herramienta usar

### ¿La función de tu aplicación es específica (sólo imágenes, sólo TTS)?
→ **Modo forzado** correspondiente — más explícito, sin overhead de gap analysis

### ¿Usas un modelo local pequeño sin function calling (Ollama, LM Studio)?
→ **`cmSmartDispatch`** — con `Asynchronous = False`

### ¿Necesitas que el LLM sintetice el resultado de una búsqueda antes de responder?
→ **`cmConversation`** con `cap_WebSearch` en gap — el LLM recibe el resultado web y genera la respuesta final

### ¿Necesitas el resultado crudo de la búsqueda directamente?
→ **`cmWebSearch`** — el resultado del tool llega sin síntesis adicional

### Árbol de decisión rápido

```
¿Aplicación general de chat?
  └─ SÍ → cmConversation

¿Función única y conocida de antemano?
  ├─ Solo imágenes   → cmImageGeneration
  ├─ Solo video      → cmVideoGeneration
  ├─ Solo TTS        → cmSpeechGeneration
  ├─ Solo transcribir→ cmTranscription
  ├─ Solo buscar web → cmWebSearch
  └─ Solo reportes   → cmReportGeneration

¿Modelo local sin function calling?
  └─ cmSmartDispatch (con Asynchronous=False)
```

---

## 9. Cambio de modo en tiempo de ejecución

`ChatMode` se puede cambiar entre llamadas. Es útil para aplicaciones con múltiples funciones:

```pascal
// Botón "Chat normal"
procedure TForm1.BtnChatClick(Sender: TObject);
begin
  Conn.ChatMode := cmConversation;
  Conn.AddMessageAndRun(EdInput.Text, 'user', []);
end;

// Botón "Generar imagen"
procedure TForm1.BtnImageClick(Sender: TObject);
begin
  Conn.ChatMode := cmImageGeneration;
  Conn.AddMessageAndRun(EdInput.Text, 'user', []);
end;

// Botón "Leer en voz alta"
procedure TForm1.BtnTTSClick(Sender: TObject);
begin
  Conn.ChatMode := cmSpeechGeneration;
  Conn.AddMessageAndRun(EdInput.Text, 'user', []);
end;
```

> **Nota:** Al cambiar de modo no se borra el historial. Si quieres evitar que mensajes de diferentes modos se mezclen en el contexto del LLM, llama a `Conn.Messages.Clear` antes del cambio.

---

## 10. Referencia de archivos fuente

| Archivo | Contenido relevante |
|---------|---------------------|
| `Source/Core/uMakerAi.Chat.pas` | `TAiChatMode` enum (línea ~63), `RunNew` con las tres fases (línea ~2840) |
| `Source/Chat/uMakerAi.Chat.AiConnection.pas` | `TAiChatConnection.ChatMode` property, `SetChatMode` |
| `Source/Chat/uMakerAi.Chat.Tools.pas` | Clases base de ChatTools compatibles con todos los modos |

---

> Ver [uMakerAi-CapabilitySystem.md](uMakerAi-CapabilitySystem.md) para entender el gap analysis que usa `cmConversation`.
> Ver [uMakerAi-ChatTools-SmartDispatch.md](uMakerAi-ChatTools-SmartDispatch.md) para la documentación completa de `cmSmartDispatch`.
> Ver [uMakerAi-FunctionCalling-SmartDispatch.md](uMakerAi-FunctionCalling-SmartDispatch.md) para la comparación con function calling nativo.
