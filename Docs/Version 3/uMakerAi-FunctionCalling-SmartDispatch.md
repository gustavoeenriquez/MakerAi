# Function Calling vs cmSmartDispatch — Guía de selección

**MakerAI v3.3 — Documentación técnica**
Última actualización: abril 2026

---

## Índice

1. [Introducción](#1-introducción)
2. [Function Calling nativo](#2-function-calling-nativo)
3. [cmSmartDispatch](#3-cmsmartdispatch)
4. [Tabla comparativa](#4-tabla-comparativa)
5. [Árbol de decisión](#5-árbol-de-decisión)
6. [Compatibilidad de modelos](#6-compatibilidad-de-modelos)
7. [Escenarios recomendados](#7-escenarios-recomendados)
8. [Migración entre modos](#8-migración-entre-modos)

---

## 1. Introducción

MakerAI ofrece dos mecanismos para que un LLM invoque herramientas externas:

1. **Function calling nativo** — el modelo recibe definiciones de herramientas en JSON y responde con bloques `tool_use` estructurados. Requiere soporte explícito del modelo y del proveedor.

2. **cmSmartDispatch** — el framework le enseña al modelo a elegir herramientas usando etiquetas de texto plano (`[IMAGEGEN]`, `[WEBSEARCH]`, etc.). Funciona con cualquier modelo capaz de seguir instrucciones.

Ambos mecanismos producen el mismo resultado final (la herramienta correcta se ejecuta, el resultado llega al usuario), pero difieren en requisitos, capacidades y casos de uso óptimos.

---

## 2. Function Calling nativo

### Cómo funciona

El framework envía al modelo las definiciones de herramientas disponibles junto con el mensaje del usuario. El modelo responde con un bloque JSON indicando qué herramienta invocar y con qué parámetros. El framework parsea ese bloque, ejecuta la herramienta y envía el resultado de vuelta al modelo para que genere la respuesta final.

```pascal
// Activar function calling nativo
Conn.DriverName := 'OpenAI';
Conn.Params.Values['Model']       := 'gpt-4.1';
Conn.Params.Values['Tool_Active'] := 'True';  // habilitado por defecto en la mayoría de modelos
```

### Capacidades

- **Múltiples herramientas por turno**: el modelo puede llamar a 2 o más herramientas en un mismo mensaje antes de responder al usuario.
- **Parámetros ricos**: cada herramienta puede recibir parámetros estructurados con validación de esquema JSON.
- **Iteración de herramientas**: el loop `tool_use → result → tool_use → result → response` se gestiona automáticamente.
- **Modo asíncrono**: compatible con `Asynchronous = True` y streaming.

### Requisitos

- El modelo debe soportar function calling (ver sección 6).
- `Tool_Active = True` en los parámetros de la conexión (es el default en la mayoría de modelos capaces).
- Para herramientas `ChatTools` (Image, WebSearch, Speech, Video): no se requiere configuración adicional; se activan vía el sistema de `ModelCaps`/`SessionCaps`.
- Para herramientas personalizadas (`TAiFunctions`): definir el esquema JSON con `TAiToolsFunction`.

---

## 3. cmSmartDispatch

### Cómo funciona

El framework ejecuta **dos llamadas LLM** por mensaje. La primera (Paso 1) usa un contexto aislado con un prompt de despacho especializado para que el modelo clasifique la solicitud en una etiqueta. La segunda (Paso 2) ejecuta la herramienta o devuelve la respuesta directa.

```pascal
// Activar SmartDispatch
Conn.DriverName := 'Ollama';
Conn.Params.Values['Model']        := 'gemma3:4b';
Conn.Params.Values['Asynchronous'] := 'False';  // obligatorio
Conn.ChatMode := cmSmartDispatch;
```

### Capacidades

- **Sin requisitos del modelo**: funciona con cualquier LLM que siga instrucciones básicas.
- **Herramientas ChatTools**: `ImageTool`, `WebSearchTool`, `SpeechTool`, `VideoTool`.
- **Herramientas simuladas**: facilita el desarrollo y testing con implementaciones mock.
- **Modelo local + herramientas cloud**: el clasificador puede ser un modelo económico local mientras las herramientas usan APIs premium.

### Limitaciones

- Una sola herramienta por turno.
- No soporta `Asynchronous = True`.
- Latencia adicional de ~0.5-2 s por la segunda llamada LLM.
- Herramientas personalizadas vía `TAiFunctions` no disponibles (sólo las 4 herramientas `ChatTools`).

---

## 4. Tabla comparativa

| Criterio | Function Calling nativo | cmSmartDispatch |
|----------|------------------------|-----------------|
| **Soporte del modelo requerido** | Sí — JSON tool_use | No — cualquier modelo |
| **Herramientas por turno** | Múltiples (ilimitado) | Una |
| **Herramientas disponibles** | ChatTools + TAiFunctions custom | Sólo ChatTools (Image, Video, Speech, Web) |
| **Parámetros de herramienta** | JSON esquemado, validado | Texto plano (prompt/query reescrito) |
| **Modo asíncrono / streaming** | Sí | No |
| **Latencia adicional** | Mínima | +1 LLM call (~0.5-2 s) |
| **Historial contaminado** | No | No |
| **Precisión del routing** | Muy alta (estructura JSON) | Alta con modelos ≥4b |
| **Costo de inferencia** | 1 llamada por turno | 2 llamadas por turno |
| **Modelos locales pequeños** | No (≥7b recomendado con tools) | Sí (≥3b recomendado) |
| **Desarrollo/prototipado** | Requiere API real | Mock tools facilitan testing |
| **Producción multi-step** | Óptimo | No recomendado |

---

## 5. Árbol de decisión

```
¿El modelo soporta function calling?
│
├─ NO (modelo local pequeño, Phi-3, Gemma 3:4b, etc.)
│   └─ Usar cmSmartDispatch
│
└─ SÍ
    │
    ├─ ¿Necesitas herramientas personalizadas (TAiFunctions)?
    │   └─ SÍ → Function calling nativo (cmConversation)
    │
    ├─ ¿Necesitas múltiples herramientas en un mismo turno?
    │   └─ SÍ → Function calling nativo (cmConversation)
    │
    ├─ ¿Necesitas streaming / modo asíncrono?
    │   └─ SÍ → Function calling nativo (cmConversation)
    │
    ├─ ¿El modelo es local y quieres usar herramientas cloud externas?
    │   └─ cmSmartDispatch (clasifica local, ejecuta cloud)
    │
    └─ Cualquier otro caso → cmConversation (default recomendado)
```

---

## 6. Compatibilidad de modelos

### Modelos con function calling nativo (usar cmConversation)

| Provider | Modelos | Notas |
|----------|---------|-------|
| OpenAI | `gpt-4.1`, `gpt-4o`, `o3`, `o4-mini` | Tool_Active=True (default) |
| Claude | `claude-opus-4-6`, `claude-sonnet-4-6`, `claude-haiku-4-5` | Tool_Active=True (default) |
| Gemini | `gemini-2.5-flash`, `gemini-2.5-pro`, `gemini-3-pro-preview` | Tool_Active=True (default) |
| Groq | `llama-4-scout`, `llama-4-maverick`, `qwen3-32b` | Tool_Active=True (default) |
| Ollama | `qwen2.5:7b`, `qwen3:8b`, `llama3.3:70b` | Tool_Active=True por modelo |
| Ollama | `gemma4:e2b`, `gemma4:e4b` | Tool_Active=True — también admiten SmartDispatch |

### Modelos sin function calling (usar cmSmartDispatch)

| Provider | Modelos | Notas |
|----------|---------|-------|
| Ollama | `gemma3:1b/4b/12b/27b` | Sin function calling vía Ollama |
| Ollama | `llama3.2:3b` | Function calling limitado/poco confiable |
| Ollama | `phi3:mini`, `phi3:medium` | Sin function calling |
| Ollama | `mistral:7b` | Sin function calling en Ollama |
| LM Studio | Cualquier GGUF pequeño | Depende del modelo cargado |

> **Nota Gemma 4:** Los modelos Gemma 4 (`gemma4:e2b`, `gemma4:e4b`) sí soportan function calling nativo vía Ollama (`Tool_Active=True`). Se pueden usar con ambos modos; el modo nativo es preferible para producción.

---

## 7. Escenarios recomendados

### Escenario A: Aplicación de producción con OpenAI/Claude

```pascal
// Function calling nativo — óptimo para producción
Conn.DriverName := 'OpenAI';
Conn.Params.Values['Model']       := 'gpt-4.1';
// Tool_Active=True ya es el default para gpt-4.1
Conn.ChatMode := cmConversation;  // default
```

**Por qué:** Máxima precisión, soporte de herramientas múltiples, streaming disponible.

---

### Escenario B: Prototipo offline con modelo local

```pascal
// SmartDispatch — modelo local, sin necesidad de API key
Conn.DriverName := 'Ollama';
Conn.Params.Values['Model']        := 'gemma3:4b';
Conn.Params.Values['Asynchronous'] := 'False';
Conn.ChatMode := cmSmartDispatch;

// Mock tools para desarrollo rápido
Conn.ChatTools.ImageTool     := TMockImageTool.Create(nil);
Conn.ChatTools.WebSearchTool := TMockWebSearchTool.Create(nil);
```

**Por qué:** Desarrollo sin costo de API, testing del flujo completo con mocks.

---

### Escenario C: Modelo local + herramientas premium

```pascal
// SmartDispatch: clasificación gratuita + herramientas de calidad
Conn.DriverName := 'Ollama';
Conn.Params.Values['Model']        := 'llama3.2:3b';
Conn.Params.Values['Asynchronous'] := 'False';
Conn.ChatMode := cmSmartDispatch;

// Herramientas reales de calidad premium
ImageTool        := TAiDalleImageTool.Create(nil);
ImageTool.ApiKey := '@OPENAI_API_KEY';
ImageTool.Model  := 'gpt-image-1';
Conn.ChatTools.ImageTool := ImageTool;

SearchTool        := TAiBraveSearchTool.Create(nil);
SearchTool.ApiKey := '@BRAVE_API_KEY';
Conn.ChatTools.WebSearchTool := SearchTool;
```

**Por qué:** Costo de inferencia mínimo (modelo local para clasificación), herramientas premium sólo cuando se necesitan.

---

### Escenario D: Aplicación multi-herramienta compleja

```pascal
// Function calling nativo — soporta múltiples tools por turno
Conn.DriverName := 'Claude';
Conn.Params.Values['Model'] := 'claude-sonnet-4-6';
Conn.ChatMode := cmConversation;

// Múltiples herramientas ChatTools
Conn.ChatTools.WebSearchTool := MiWebSearch;
Conn.ChatTools.ImageTool     := MiImageGen;

// Herramientas personalizadas adicionales via TAiFunctions
Conn.AiChat.Functions.AddFunction('GetWeather', 'Obtiene el clima actual', ...);
Conn.AiChat.Functions.AddFunction('SendEmail', 'Envía un correo electrónico', ...);
```

**Por qué:** El modelo puede llamar `GetWeather` y `WebSearch` en el mismo turno antes de responder; SmartDispatch no puede hacer esto.

---

## 8. Migración entre modos

### De cmSmartDispatch a function calling nativo

Cuando el modelo lo permita (cambio a un provider con soporte), la migración es trivial — basta con cambiar dos líneas:

```pascal
// Antes (SmartDispatch con modelo sin function calling)
Conn.DriverName := 'Ollama';
Conn.Params.Values['Model']        := 'gemma3:4b';
Conn.Params.Values['Asynchronous'] := 'False';
Conn.ChatMode := cmSmartDispatch;

// Después (function calling nativo)
Conn.DriverName := 'OpenAI';
Conn.Params.Values['Model'] := 'gpt-4.1';
// Eliminar la línea de Asynchronous=False si no es necesaria
Conn.ChatMode := cmConversation;  // o simplemente omitir, es el default

// Las herramientas ChatTools se mantienen igual — son compatibles con ambos modos
```

Las herramientas `ChatTools` (`ImageTool`, `WebSearchTool`, etc.) son completamente compatibles con ambos modos. No requieren cambios al migrar.

### De function calling nativo a cmSmartDispatch

```pascal
// Antes (function calling nativo)
Conn.DriverName := 'OpenAI';
Conn.ChatMode   := cmConversation;

// Después (SmartDispatch para pruebas con modelo local)
Conn.DriverName := 'Ollama';
Conn.Params.Values['Model']        := 'gemma3:4b';
Conn.Params.Values['Asynchronous'] := 'False';   // añadir obligatoriamente
Conn.ChatMode := cmSmartDispatch;

// Nota: las TAiFunctions personalizadas NO se transferirán — sólo ChatTools funciona
```

> **Restricción:** Las herramientas personalizadas definidas con `TAiFunctions` no están disponibles en `cmSmartDispatch`. Si la aplicación las usa, no es posible migrar directamente a SmartDispatch sin eliminar esa funcionalidad.

---

> Ver [uMakerAi-ChatTools-SmartDispatch.md](uMakerAi-ChatTools-SmartDispatch.md) para la documentación completa de `cmSmartDispatch`.
> Ver [uMakerAi.ToolFuncions.docx](uMakerAi.ToolFuncions.docx) para la documentación de `TAiFunctions` y function calling nativo.
