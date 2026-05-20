# cmSmartDispatch — Despacho inteligente de herramientas para modelos pequeños

**MakerAI v3.3 — Documentación técnica**
Última actualización: abril 2026

---

## Índice

1. [Propósito](#1-propósito)
2. [El problema: modelos sin function calling](#2-el-problema-modelos-sin-function-calling)
3. [Cómo funciona: flujo en dos pasos](#3-cómo-funciona-flujo-en-dos-pasos)
4. [Etiquetas disponibles](#4-etiquetas-disponibles)
5. [Configuración básica](#5-configuración-básica)
6. [Herramientas compatibles](#6-herramientas-compatibles)
7. [Ejemplo completo con herramientas simuladas](#7-ejemplo-completo-con-herramientas-simuladas)
8. [Ejemplo con herramientas reales](#8-ejemplo-con-herramientas-reales)
9. [Prompt de despacho interno](#9-prompt-de-despacho-interno)
10. [Limitaciones y consideraciones](#10-limitaciones-y-consideraciones)
11. [Comparación con otros modos](#11-comparación-con-otros-modos)
12. [Referencia de archivos fuente](#12-referencia-de-archivos-fuente)

---

## 1. Propósito

`cmSmartDispatch` es un modo de chat de MakerAI que permite usar herramientas (`ImageTool`, `WebSearchTool`, `SpeechTool`, `VideoTool`) con **cualquier modelo LLM**, incluidos los que no tienen soporte nativo de function calling.

La idea es resolver un problema concreto: modelos locales pequeños como Gemma 3:4b, LLaMA 3.2:3b o Phi-3-mini procesan lenguaje natural perfectamente, pero no entienden el protocolo JSON de function calling que usa la API. Con `cmSmartDispatch`, el framework les enseña a elegir herramientas usando etiquetas de texto plano (`[IMAGEGEN]`, `[WEBSEARCH]`, etc.) — un protocolo que cualquier modelo puede seguir.

---

## 2. El problema: modelos sin function calling

### Modelos con function calling nativo

Un modelo como `gpt-4.1` o `qwen2.5:7b` puede recibir definiciones de herramientas en formato JSON, evaluar cuál aplicar y devolver una respuesta estructurada que el framework parsea automáticamente.

```
Usuario: "genera una imagen de un gato"
  → El modelo devuelve: { "tool_use": "ImageTool", "input": { "prompt": "un gato" } }
  → El framework ejecuta ImageTool.ExecuteImageGeneration("un gato")
```

### Modelos sin function calling

Un modelo como `gemma3:4b` o `phi3:mini` recibe la misma solicitud pero simplemente responde en lenguaje natural:

```
Usuario: "genera una imagen de un gato"
  → El modelo devuelve: "Lo siento, no puedo generar imágenes directamente."
```

No hay forma de indicarle que use una herramienta mediante el protocolo JSON estándar.

### La solución: protocol de etiquetas

`cmSmartDispatch` resuelve esto con un **paso de despacho previo**: le pide al modelo que en lugar de responder directamente, clasifique la solicitud en una etiqueta de texto plano. El modelo aprende a responder así:

```
Usuario: "genera una imagen de un gato"
  → Pass 1: [IMAGEGEN] a cat
  → Pass 2: el framework ejecuta ImageTool.ExecuteImageGeneration("a cat")
```

---

## 3. Cómo funciona: flujo en dos pasos

```text
Usuario envía mensaje
       │
       ▼
┌─────────────────────────────────────────────┐
│ PASO 1 — Clasificación (silencioso)          │
│                                             │
│  El LLM recibe:                             │
│  • system: prompt de despacho               │
│  • user:   mensaje original del usuario     │
│                                             │
│  El LLM responde:                           │
│  • "[TAG] contenido reescrito"              │
│                                             │
│  Nota: este paso NO se agrega al historial  │
│  y usa contexto aislado (sin history).      │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
          Parsear [TAG] y contenido
                   │
          ┌────────┴────────┐
          │                 │
         CHAT          TOOL (IMAGEGEN,
          │            WEBSEARCH, etc.)
          │                 │
          ▼                 ▼
  Usa la respuesta   Ejecuta la herramienta
  del Paso 1         correspondiente
  directamente
          │                 │
          └────────┬────────┘
                   │
                   ▼
        Resultado devuelto al usuario
        y agregado al historial
```

### Detalles del aislamiento de contexto

El Paso 1 se ejecuta con un contexto completamente aislado: sólo contiene el prompt de despacho (como mensaje `system`) y el mensaje actual del usuario. El historial de conversación no se incluye deliberadamente para evitar que respuestas anteriores de herramientas (`[IMAGEN SIMULADA] ...`) confundan la clasificación de nuevas solicitudes.

---

## 4. Etiquetas disponibles

Las etiquetas disponibles en el Paso 1 dependen de qué herramientas están asignadas a la conexión:

| Etiqueta | Se genera cuando | Herramienta ejecutada |
|----------|------------------|-----------------------|
| `[IMAGEGEN]` | `ChatTools.ImageTool` está asignado | `ImageTool.ExecuteImageGeneration()` |
| `[VIDEOGEN]` | `ChatTools.VideoTool` está asignado | `VideoTool.ExecuteVideoGeneration()` |
| `[TTS]` | `ChatTools.SpeechTool` está asignado | `SpeechTool.ExecuteSpeechGeneration()` |
| `[WEBSEARCH]` | `ChatTools.WebSearchTool` está asignado | `WebSearchTool.ExecuteSearch()` |
| `[CHAT]` | Siempre disponible (fallback) | Respuesta directa del LLM (Paso 1) |

El prompt de despacho se construye dinámicamente: sólo incluye las etiquetas de las herramientas que están realmente asignadas.

---

## 5. Configuración básica

### Requisitos

- `ChatMode := cmSmartDispatch` en la conexión
- `Asynchronous = False` en los parámetros — el modo asíncrono no está soportado
- Al menos una herramienta asignada en `ChatTools` (si no hay herramientas, todas las solicitudes irán a `[CHAT]`)
- Las herramientas deben asignarse **antes** de establecer `DriverName`

### Configuración mínima

```pascal
Conn := TAiChatConnection.Create(nil);

// 1. Asignar herramientas ANTES del driver
Conn.ChatTools.ImageTool     := MiImageTool;
Conn.ChatTools.WebSearchTool := MiWebSearchTool;

// 2. Configurar driver
Conn.DriverName := 'Ollama';
Conn.Params.Values['URL']          := 'http://localhost:11434/';
Conn.Params.Values['Model']        := 'gemma3:4b';
Conn.Params.Values['Asynchronous'] := 'False';   // REQUERIDO
Conn.Params.Values['Max_Tokens']   := '512';

// 3. Activar modo
Conn.ChatMode      := cmSmartDispatch;
Conn.SystemPrompt.Text := 'Eres un asistente útil. Responde en español.';
```

> **Nota:** El `SystemPrompt` se usa en el Paso 2 (respuesta final), no en el Paso 1 de despacho. El Paso 1 siempre usa el prompt de despacho interno del framework.

---

## 6. Herramientas compatibles

`cmSmartDispatch` es compatible con las mismas herramientas `ChatTools` que usa el modo `cmConversation`. Para usarlas con SmartDispatch basta con implementar la clase base correspondiente:

| Clase base | Tag generado | Método a sobrescribir |
|------------|--------------|----------------------|
| `TAiImageToolBase` | `[IMAGEGEN]` | `ExecuteImageGeneration(APrompt, ResMsg, AskMsg)` |
| `TAiVideoToolBase` | `[VIDEOGEN]` | `ExecuteVideoGeneration(APrompt, ResMsg, AskMsg)` |
| `TAiSpeechToolBase` | `[TTS]` | `ExecuteSpeechGeneration(AText, ResMsg, AskMsg)` |
| `TAiWebSearchToolBase` | `[WEBSEARCH]` | `ExecuteSearch(AQuery, ResMsg, AskMsg)` |

### Patrón de implementación

```pascal
type
  TMiImageTool = class(TAiImageToolBase)
  protected
    procedure ExecuteImageGeneration(const APrompt: string;
      ResMsg, AskMsg: TAiChatMessage); override;
  end;

procedure TMiImageTool.ExecuteImageGeneration(const APrompt: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  // APrompt: descripción ya reescrita por el dispatcher
  // ResMsg:  rellenar con el resultado
  ResMsg.Prompt := GenerarImagen(APrompt);   // llamada real a la API
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;
```

### Combinación con herramientas reales del framework

Las herramientas integradas de MakerAI (`TAiDalle`, `TAiGeminiImageTool`, `TAiBraveSearch`, etc.) implementan las mismas clases base y funcionan sin cambios en modo SmartDispatch:

```pascal
// ImageTool real con DALL-E
ImageTool        := TAiDalleImageTool.Create(nil);
ImageTool.ApiKey := '@OPENAI_API_KEY';
Conn.ChatTools.ImageTool := ImageTool;

// WebSearchTool real con Brave Search
SearchTool        := TAiBraveSearchTool.Create(nil);
SearchTool.ApiKey := '@BRAVE_API_KEY';
Conn.ChatTools.WebSearchTool := SearchTool;

// El modelo puede ser local (Ollama) y las herramientas ser APIs externas
Conn.DriverName := 'Ollama';
Conn.ChatMode   := cmSmartDispatch;
```

---

## 7. Ejemplo completo con herramientas simuladas

El demo `Demos/Console/Demos02-ChatTools/08-SmartDispatch/` usa herramientas simuladas (mock) para validar el routing sin llamadas reales a APIs.

```pascal
program SmartDispatch;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Ollama;

const
  DRIVER     = 'Ollama';
  MODEL      = 'gemma3:4b';
  OLLAMA_URL = 'http://localhost:11434/';

// --- Herramientas simuladas ---

type
  TMockImageTool = class(TAiImageToolBase)
  protected
    procedure ExecuteImageGeneration(const APrompt: string;
      ResMsg, AskMsg: TAiChatMessage); override;
  end;

  TMockWebSearchTool = class(TAiWebSearchToolBase)
  protected
    procedure ExecuteSearch(const AQuery: string;
      ResMsg, AskMsg: TAiChatMessage); override;
  end;

procedure TMockImageTool.ExecuteImageGeneration(const APrompt: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  Writeln('  >> [ImageTool] prompt: "', APrompt, '"');
  ResMsg.Prompt := '[IMAGEN SIMULADA] ' + APrompt;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

procedure TMockWebSearchTool.ExecuteSearch(const AQuery: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  Writeln('  >> [WebSearchTool] consulta: "', AQuery, '"');
  ResMsg.Prompt := '[BUSQUEDA WEB SIMULADA] ' + AQuery;
  ResMsg.Role   := 'assistant';
  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

// --- Demo ---

procedure RunDemo;
var
  Conn      : TAiChatConnection;
  ImageTool : TMockImageTool;
  SearchTool: TMockWebSearchTool;
  Resp      : String;
begin
  ImageTool  := TMockImageTool.Create(nil);
  SearchTool := TMockWebSearchTool.Create(nil);
  Conn       := TAiChatConnection.Create(nil);
  try
    Conn.ChatTools.ImageTool     := ImageTool;
    Conn.ChatTools.WebSearchTool := SearchTool;

    Conn.DriverName := DRIVER;
    Conn.Params.Values['URL']          := OLLAMA_URL;
    Conn.Params.Values['Model']        := MODEL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '512';

    Conn.ChatMode          := cmSmartDispatch;
    Conn.SystemPrompt.Text := 'Eres un asistente útil. Responde en español.';

    // CHAT — pregunta directa
    Resp := Conn.AddMessageAndRun('¿Cuál es la capital de Francia?', 'user', []);
    Writeln('CHAT: ', Resp);

    // IMAGEGEN — solicitud de imagen
    Resp := Conn.AddMessageAndRun('Genera una imagen de un gato con sombrero', 'user', []);
    Writeln('IMAGEGEN: ', Resp);

    // WEBSEARCH — solicitud de búsqueda
    Resp := Conn.AddMessageAndRun('Busca las últimas noticias de IA en 2025', 'user', []);
    Writeln('WEBSEARCH: ', Resp);

  finally
    Conn.Free;
    ImageTool.Free;
    SearchTool.Free;
  end;
end;

begin
  RunDemo;
  Readln;
end.
```

### Salida esperada

```
CHAT: París es la capital de Francia.
  >> [ImageTool] prompt: "a cat wearing a hat"
IMAGEGEN: [IMAGEN SIMULADA] a cat wearing a hat
  >> [WebSearchTool] consulta: "latest AI news 2025"
WEBSEARCH: [BUSQUEDA WEB SIMULADA] latest AI news 2025
```

---

## 8. Ejemplo con herramientas reales

Combinación de modelo local Ollama con herramientas reales de APIs externas:

```pascal
uses
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Ollama,
  uMakerAi.OpenAi.Dalle,        // TAiDalleImageTool
  uMakerAi.Gemini.WebSearch;    // TAiGeminiWebSearchTool (o cualquier WebSearch)

procedure ConfigurarSmartDispatch(Conn: TAiChatConnection);
var
  ImageTool : TAiDalleImageTool;
  SearchTool: TAiGeminiWebSearchTool;
begin
  // Herramientas reales
  ImageTool        := TAiDalleImageTool.Create(nil);
  ImageTool.ApiKey := '@OPENAI_API_KEY';
  ImageTool.Model  := 'dall-e-3';

  SearchTool        := TAiGeminiWebSearchTool.Create(nil);
  SearchTool.ApiKey := '@GEMINI_API_KEY';

  // Asignar ANTES del driver
  Conn.ChatTools.ImageTool     := ImageTool;
  Conn.ChatTools.WebSearchTool := SearchTool;

  // Modelo local sin function calling
  Conn.DriverName := 'Ollama';
  Conn.Params.Values['URL']          := 'http://localhost:11434/';
  Conn.Params.Values['Model']        := 'llama3.2:3b';
  Conn.Params.Values['Asynchronous'] := 'False';

  Conn.ChatMode := cmSmartDispatch;
end;
```

En este patrón, el razonamiento y clasificación los hace el modelo local (sin costo de API), mientras que la generación de imágenes o la búsqueda web las hacen servicios externos.

---

## 9. Prompt de despacho interno

El framework construye automáticamente el prompt de despacho en `TAiChat.BuildSmartDispatchPrompt`. Su estructura es:

```
You are a task dispatcher. Your ONLY job is to classify the user request
and output exactly one line.
Output format: [TAG] rewritten_request

Available tags:
  [IMAGEGEN] <image description> — use when user asks to create, draw,
    or generate an image
  [WEBSEARCH] <optimized search query> — use when user asks for recent
    news, current info, or web search
  [CHAT] <your answer> — use for general conversation, questions,
    calculations, or anything else

STRICT RULES:
  - Output ONLY the single line [TAG] content. No other text.
  - Do NOT say you cannot generate images — just use the appropriate tag.
  - Always pick exactly ONE tag. [CHAT] is the fallback.

Examples:
  User: draw a red cat => [IMAGEGEN] a red cat
  User: latest AI news => [WEBSEARCH] latest artificial intelligence news 2025
  User: what is 2+2 => [CHAT] 4
```

> **Nota de idioma:** El prompt de despacho está en inglés por diseño. Los modelos multilingues clasifican con más precisión cuando el protocolo de etiquetas está en inglés, independientemente del idioma del usuario.

El `SystemPrompt` que el desarrollador establece en la conexión **no interfiere** con el despacho — sólo se aplica en el Paso 2 (respuesta o ejecución de herramienta).

---

## 10. Limitaciones y consideraciones

### Modo asíncrono no soportado

`cmSmartDispatch` requiere que `Asynchronous = False`. El Paso 1 es síncrono por diseño: necesita la respuesta del clasificador antes de poder ejecutar la herramienta en el Paso 2. Si `Asynchronous = True`, el framework lanza una excepción en `RunNew`.

```pascal
// CORRECTO
Conn.Params.Values['Asynchronous'] := 'False';

// INCORRECTO — lanza excepción
Conn.Params.Values['Asynchronous'] := 'True';
Conn.ChatMode := cmSmartDispatch;
```

### Modelos que ignoran el protocolo

Algunos modelos muy pequeños (1b-2b parámetros) pueden producir salidas mal formateadas que no incluyen ninguna etiqueta. En ese caso, `ParseSmartDispatchResponse` cae al tag `[CHAT]` y devuelve la respuesta literal del modelo.

Se recomienda un modelo de al menos 3b-4b parámetros para resultados confiables. Modelos probados:

| Modelo | Resultados |
|--------|-----------|
| `gemma3:4b` (Ollama) | Excelente clasificación |
| `gemma4:e4b` (Ollama) | Excelente — soporta también function calling nativo |
| `llama3.2:3b` (Ollama) | Buena clasificación en casos claros |
| `phi3:mini` (Ollama) | Variable — depende del prompt |
| `gemma3:1b` (Ollama) | No recomendado — ignora el protocolo frecuentemente |

### Latencia adicional

El Paso 1 es una llamada LLM completa (aunque con un contexto mínimo de 2 mensajes y respuesta muy corta ~10-30 tokens). El overhead típico es de 0.5-2 segundos dependiendo del modelo y hardware. Para casos de uso en tiempo real, considerar `cmConversation` con un modelo que soporte function calling nativo.

### Historial de conversación

El historial se mantiene correctamente. Cada intercambio agrega exactamente un par `user/assistant` al historial. El Paso 1 de despacho es transparente — no aparece en el historial visible.

### Sin soporte para herramientas encadenadas

SmartDispatch despacha exactamente **una herramienta por mensaje**. No hay soporte para encadenamiento de herramientas (llamar WebSearch y luego ImageGen en un mismo turno). Para flujos multi-herramienta, usar el modo `cmConversation` con un modelo que soporte function calling nativo y múltiples tool calls.

---

## 11. Comparación con otros modos

| Característica | `cmConversation` + tools | `cmSmartDispatch` |
|---------------|--------------------------|-------------------|
| Requiere function calling | Sí | No |
| Modelos compatibles | OpenAI, Claude, Gemini, Qwen, LLaMA... | Cualquier modelo |
| Herramientas múltiples en un turno | Sí (tool_use loop) | No (1 herramienta por turno) |
| Historial contaminado por tool | No | No (aislamiento en Paso 1) |
| Modo asíncrono | Sí | No |
| Latencia extra | Mínima | +1 llamada LLM (Paso 1) |
| Precisión del routing | Alta (JSON estructurado) | Alta en modelos ≥4b |
| Casos de uso ideales | Producción, multi-step | Modelos locales, prototipos |

> Ver [uMakerAi-FunctionCalling-SmartDispatch.md](uMakerAi-FunctionCalling-SmartDispatch.md) para una comparación extendida con guía de selección.

---

## 12. Referencia de archivos fuente

| Archivo | Contenido relevante |
|---------|---------------------|
| `Source/Core/uMakerAi.Chat.pas` | `InternalRunSmartDispatch`, `BuildSmartDispatchPrompt`, `ParseSmartDispatchResponse` |
| `Source/Core/uMakerAi.Core.pas` | `TAiChatMode` enum (valor `cmSmartDispatch`) |
| `Source/Chat/uMakerAi.Chat.AiConnection.pas` | `TAiChatConnection.ChatMode` property |
| `Source/Chat/uMakerAi.Chat.Tools.pas` | `TAiImageToolBase`, `TAiWebSearchToolBase`, `TAiSpeechToolBase`, `TAiVideoToolBase` |
| `Demos/Console/Demos02-ChatTools/08-SmartDispatch/` | Demo completo con mock tools |

---

> Ver [uMakerAi-CapabilitySystem.md](uMakerAi-CapabilitySystem.md) para la configuración de `ModelCaps`/`SessionCaps`.
> Ver [uMakerAi-FunctionCalling-SmartDispatch.md](uMakerAi-FunctionCalling-SmartDispatch.md) para la comparación con function calling nativo.
