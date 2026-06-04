# Sistema de Capacidades: ModelCaps y SessionCaps

**MakerAI v3.3 — Documentación técnica**
Última actualización: marzo 2026

---

## Índice

1. [Introducción](#1-introducción)
2. [El enum TAiCapability](#2-el-enum-taicapability)
3. [ModelCaps vs SessionCaps](#3-modelcaps-vs-sessioncaps)
4. [Gap Analysis: el motor central](#4-gap-analysis-el-motor-central)
5. [Configuración via TAiChatFactory](#5-configuración-via-taichatfactory)
6. [Configuración en tiempo de ejecución](#6-configuración-en-tiempo-de-ejecución)
7. [Compatibilidad con el sistema legacy](#7-compatibilidad-con-el-sistema-legacy)
8. [Patrones de configuración frecuentes](#8-patrones-de-configuración-frecuentes)
9. [Referencia de capacidades por provider](#9-referencia-de-capacidades-por-provider)
10. [Añadir un nuevo provider](#10-añadir-un-nuevo-provider)
11. [Referencia de archivos fuente](#11-referencia-de-archivos-fuente)

---

## 1. Introducción

MakerAI v3.3 introduce el **sistema unificado de capacidades** (`TAiCapabilities`), que reemplaza los cuatro parámetros legacy dispersos (`NativeInputFiles`, `NativeOutputFiles`, `ChatMediaSupports`, `EnabledFeatures`) por dos propiedades ortogonales y simples:

| Propiedad | Significado |
|-----------|-------------|
| `ModelCaps` | Lo que el modelo sabe hacer **de forma nativa** vía completions |
| `SessionCaps` | Lo que la sesión **necesita** (puede superar las capacidades nativas) |

La diferencia entre ambas (`Gap = SessionCaps - ModelCaps`) determina qué bridges y herramientas activa el orquestador automáticamente antes de llamar a la API.

**Ventajas sobre el sistema anterior:**
- Una sola línea describe completamente un modelo
- El bridge correcto se activa solo, sin código adicional
- Compatible hacia atrás: los modelos configurados con el sistema legacy siguen funcionando sin cambios

---

## 2. El enum TAiCapability

Definido en `Source/Core/uMakerAi.Core.pas`:

```pascal
TAiCapability = (
  // ---- Entrada / Comprensión (completions nativo) ----
  cap_Image,            // el modelo entiende imágenes entrantes
  cap_Audio,            // el modelo entiende/transcribe audio entrante
  cap_Video,            // el modelo entiende video entrante
  cap_Pdf,              // el modelo entiende PDFs entrantes
  cap_WebSearch,        // el modelo puede buscar en la web
  cap_Reasoning,        // el modelo tiene razonamiento extendido (CoT/thinking)
  cap_CodeInterpreter,  // el modelo puede ejecutar código
  cap_Memory,           // el modelo tiene memoria persistente
  cap_TextEditor,       // el modelo puede editar archivos de texto
  cap_ComputerUse,      // el modelo puede controlar el ordenador
  cap_Shell,            // el modelo puede ejecutar comandos shell

  // ---- Salida / Generación (gap -> activa bridge automático) ----
  cap_GenImage,         // producir una imagen como output
  cap_GenAudio,         // producir audio como output (TTS)
  cap_GenVideo,         // producir video como output
  cap_GenReport,        // producir un reporte (PDF, HTML, XLSX)
  cap_ExtractCode       // post-procesar: extraer bloques de código de la respuesta
);

TAiCapabilities = set of TAiCapability;
```

### Clasificación conceptual

**Capacidades de entrada** (`cap_Image` .. `cap_Shell`): describen qué tipos de contenido entiende el modelo de forma nativa en el endpoint de completions. Si el modelo no tiene una capacidad de entrada pero la sesión la necesita, el orquestador ejecuta un **bridge de entrada** (transcripción, descripción de imagen, extracción de texto de PDF) antes de enviar el prompt.

**Capacidades de generación** (`cap_GenImage` .. `cap_ExtractCode`): describen qué tipo de contenido produce la sesión como salida. Cuando hay gap en estas capacidades, el orquestador redirige la llamada al endpoint especializado (TTS, generación de imágenes, etc.) en lugar de completions.

---

## 3. ModelCaps vs SessionCaps

### ModelCaps — capacidades nativas del modelo

Representa exactamente lo que el modelo puede hacer a través del endpoint de completions **sin intervención externa**. Es un hecho fijo del modelo: no cambia según las necesidades del usuario.

Ejemplos:
- `GPT-4.1`: puede procesar imágenes → `ModelCaps = [cap_Image]`
- `dall-e-3`: solo genera imágenes, no hace completions → `ModelCaps = []`
- `gemini-2.5-flash`: multimodal completo → `ModelCaps = [cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]`

### SessionCaps — capacidades deseadas en la sesión

Representa lo que el **usuario quiere** que la sesión sea capaz de hacer. Puede coincidir con `ModelCaps` (sin gap, llamada directa) o superarla (con gap, se activan bridges).

Ejemplos:
- Quiero usar GPT-4o para generar audio TTS: `SessionCaps = [cap_Image, cap_GenAudio]` → Gap = `[cap_GenAudio]` → usa endpoint TTS de OpenAI
- Quiero usar Ollama (sin visión) con imágenes: `SessionCaps = [cap_Image]` + `ModelCaps = []` → Gap = `[cap_Image]` → bridge de descripción visual antes de completions

### Relación con los parámetros legacy

Al asignar `ModelCaps` y `SessionCaps`, el sistema sincroniza automáticamente los parámetros legacy:

| Propiedad nueva | Sincroniza (legacy) |
|-----------------|---------------------|
| `ModelCaps` | `NativeInputFiles` + `ChatMediaSupports` |
| `SessionCaps` | `NativeOutputFiles` + `EnabledFeatures` |

Esta sincronización ocurre en los setters `SetModelCaps` y `SetSessionCaps` (llamados tanto desde código como via RTTI al aplicar params de factory).

---

## 4. Gap Analysis: el motor central

El método `TAiChat.RunNew` calcula el gap en su primera línea:

```pascal
Gap := FSessionCaps - FModelCaps;  // resta de conjuntos
```

Luego ejecuta tres fases:

### Fase 1: Bridge de entrada (solo en `cmConversation`)

Para cada archivo adjunto al mensaje que el modelo no soporta nativamente:

| Gap contiene | Tipo de archivo | Bridge activado |
|---|---|---|
| `cap_Audio` | `.mp3`, `.wav`, etc. | `InternalRunTranscription` → convierte a texto |
| `cap_Image` | `.png`, `.jpg`, etc. | `InternalRunImageDescription` → describe la imagen |
| `cap_Pdf` | `.pdf` | `InternalRunPDFDescription` → extrae/describe el PDF |

Los archivos ya procesados (`MF.Procesado = True`) se saltan. También hay prioridad 1: si el evento `OnProcessMediaFile` está asignado, se usa antes del bridge automático.

### Fase 2: Grounding (siempre, sin guarda de modo)

| Gap contiene | Acción |
|---|---|
| `cap_WebSearch` | `InternalRunWebSearch` — busca en la web e inyecta resultados en el contexto |

### Fase 3: Orquestación de salida (en `cmConversation`)

El gap determina qué endpoint se usa. Se evalúa en orden de prioridad:

| Gap contiene | Método invocado |
|---|---|
| `cap_GenVideo` | `InternalRunImageVideoGeneration` |
| `cap_GenImage` | `InternalRunImageGeneration` |
| `cap_GenAudio` | `InternalRunSpeechGeneration` |
| `cap_GenReport` | `InternalRunReport` |
| (vacío) | `InternalRunCompletions` (conversación normal) |

`cap_ExtractCode` no redirige el endpoint: lo gestiona internamente `InternalRunCompletions` mediante el parámetro legacy `Tfc_ExtractTextFile in NativeOutputFiles` (sincronizado por `SyncLegacyFromSessionCaps`).

### Modos forzados

Si `ChatMode` es distinto de `cmConversation`, el Gap se ignora en Fase 1 y Fase 3, y se llama al método correspondiente directamente:

```pascal
cmImageGeneration  → InternalRunImageGeneration  (siempre)
cmVideoGeneration  → InternalRunImageVideoGeneration
cmSpeechGeneration → InternalRunSpeechGeneration
cmWebSearch        → InternalRunWebSearch
cmReportGeneration → InternalRunReport
cmTranscription    → InternalRunTranscription (primer audio del mensaje)
```

---

## 5. Configuración via TAiChatFactory

La forma estándar de configurar capacidades es en `Source/Chat/uMakerAi.Chat.Initializations.pas`, usando `TAiChatFactory.Instance.RegisterUserParam`.

### Niveles de configuración

Los parámetros tienen tres niveles (de menor a mayor prioridad):

1. **Defaults del driver** — `RegisterDefaultParams` en la clase del driver
2. **Defaults globales del provider** — `RegisterUserParam(Driver, Param, Value)`
3. **Override por modelo** — `RegisterUserParam(Driver, Model, Param, Value)`

Un override de modelo siempre gana sobre el global del driver.

### Sintaxis

```pascal
// Default global para todo el provider
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'ModelCaps',   '[cap_Image]');
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'SessionCaps', '[cap_Image]');

// Override para un modelo específico
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'nombre-del-modelo', 'ModelCaps',   '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'nombre-del-modelo', 'SessionCaps', '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('DriverName', 'nombre-del-modelo', 'ThinkingLevel', 'tlMedium');
```

### Formato de la string de capacidades

La string sigue el formato de un set de Pascal, usando los nombres exactos del enum `TAiCapability`:

```
'[]'                                        // conjunto vacío
'[cap_Image]'                               // una capacidad
'[cap_Image, cap_Reasoning]'                // varias capacidades
'[cap_Image, cap_Audio, cap_Video, cap_Pdf]'  // multimedia completo
```

El parser (en `ApplyParamsToChat`, vía RTTI `tkSet`) es sensible a mayúsculas/minúsculas: los nombres deben coincidir exactamente con los valores del enum.

### Parámetro ThinkingLevel

Para modelos con razonamiento extendido, se configura el nivel con `ThinkingLevel`:

| Valor | Descripción |
|-------|-------------|
| `tlDefault` | Deja al provider elegir (generalmente Medium) |
| `tlLow` | Razonamiento mínimo — respuesta rápida, bajo costo |
| `tlMedium` | Balance calidad/velocidad — valor recomendado |
| `tlHigh` | Razonamiento máximo — mayor calidad, más lento y costoso |

`ThinkingLevel` solo tiene efecto si `cap_Reasoning` está en `ModelCaps`. Si el modelo no tiene `cap_Reasoning`, el parámetro se ignora.

### Perfiles personalizados (aa_*)

Para crear variantes de un modelo con configuración diferente (ej. nivel de thinking distinto, caps reducidas), usar `RegisterCustomModel` + override:

```pascal
// Crea 'aa_o3-high' que usa internamente el modelo 'o3'
TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_o3-high', 'o3');
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'aa_o3-high', 'ThinkingLevel', 'tlHigh');

// Crea 'aa_gemini-3-pro-fast' con thinking reducido para respuestas rápidas
TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-3-pro-fast', 'gemini-3-pro-preview');
TAiChatFactory.Instance.RegisterUserParam('Gemini', 'aa_gemini-3-pro-fast', 'ThinkingLevel', 'tlLow');
TAiChatFactory.Instance.RegisterUserParam('Gemini', 'aa_gemini-3-pro-fast', 'ModelCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');  // sin cap_Reasoning
TAiChatFactory.Instance.RegisterUserParam('Gemini', 'aa_gemini-3-pro-fast', 'SessionCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');
```

El prefijo `aa_` es una convención del proyecto para perfiles personalizados; no es obligatorio pero ayuda a distinguirlos de los modelos oficiales en listas desplegables.

---

## 6. Configuración en tiempo de ejecución

Además de la factory, se pueden asignar `ModelCaps` y `SessionCaps` directamente sobre la instancia en código:

### Sobre TAiChatConnection

```pascal
// En tiempo de diseño (Inspector de Objetos) o en código
AiConnection.DriverName := 'OpenAi';
AiConnection.Model := 'gpt-4.1';

// Sobreescribir caps para esta sesión específica
AiConnection.ModelCaps   := [cap_Image, cap_Pdf];
AiConnection.SessionCaps := [cap_Image, cap_Pdf, cap_GenAudio];
// Gap = [cap_GenAudio] -> próximo Run() usará endpoint TTS
```

### Sobre TAiChat directamente

```pascal
var Chat: TAiOpenChat;
Chat := TAiOpenChat.Create(nil);
Chat.ApiKey := '@OPENAI_API_KEY';
Chat.Model  := 'gpt-image-1';
Chat.ModelCaps   := [];
Chat.SessionCaps := [cap_GenImage];
// Gap = [cap_GenImage] -> Run() llamará a InternalRunImageGeneration
```

**Importante:** Al asignar `ModelCaps` o `SessionCaps` directamente, se activa `FNewSystemConfigured = True`, lo que impide que `EnsureNewSystemConfig` sobreescriba estos valores con la traducción automática desde parámetros legacy.

### Consultar las caps actuales

```pascal
// Leer caps efectivas (después de aplicar params de factory)
var Gap: TAiCapabilities;
Gap := AiConnection.SessionCaps - AiConnection.ModelCaps;

if cap_GenAudio in Gap then
  ShowMessage('Esta sesión generará audio via TTS');

if cap_Reasoning in AiConnection.ModelCaps then
  ShowMessage('Este modelo tiene razonamiento nativo');
```

---

## 7. Compatibilidad con el sistema legacy

### Parámetros legacy

El sistema anterior configuraba las capacidades mediante cuatro parámetros:

| Parámetro legacy | Tipo | Descripción |
|---|---|---|
| `NativeInputFiles` | `TAiFileCategories` | Tipos de archivo que el modelo acepta nativamente |
| `NativeOutputFiles` | `TAiFileCategories` | Tipos de archivo que el modelo genera |
| `ChatMediaSupports` | `TAiChatMediaSupports` | Capacidades lógicas nativas del modelo |
| `EnabledFeatures` | `TAiChatMediaSupports` | Capacidades deseadas en la sesión |

### Traducción automática

Si un modelo fue configurado con el sistema legacy y **no** se asignaron `ModelCaps`/`SessionCaps` explícitamente (`FNewSystemConfigured = False`), el método `EnsureNewSystemConfig` (llamado al inicio de cada `Run`) traduce automáticamente:

```
ChatMediaSupports + NativeInputFiles/OutputFiles  →  ModelCaps
EnabledFeatures   + NativeOutputFiles             →  SessionCaps
```

Esta traducción ocurre una sola vez por sesión y es invisible para el usuario. El sistema legacy sigue funcionando sin ningún cambio de código.

### Prioridad de configuración

```
1. ModelCaps/SessionCaps asignados explícitamente  (FNewSystemConfigured=True)  ← Mayor prioridad
2. Traducción automática desde legacy params         (FNewSystemConfigured=False)
3. Defaults del driver (RegisterDefaultParams)                                   ← Menor prioridad
```

### Guía de migración

Para migrar un driver existente al nuevo sistema:

```pascal
// Antes (legacy)
TAiChatFactory.Instance.RegisterUserParam('MiDriver', 'ChatMediaSupports', 'Tcm_Image,Tcm_Pdf');
TAiChatFactory.Instance.RegisterUserParam('MiDriver', 'EnabledFeatures',   'Tcm_Image,Tcm_Pdf');
TAiChatFactory.Instance.RegisterUserParam('MiDriver', 'NativeInputFiles',  'Tfc_Image,Tfc_Pdf');

// Después (nuevo sistema v3.3)
TAiChatFactory.Instance.RegisterUserParam('MiDriver', 'ModelCaps',   '[cap_Image, cap_Pdf]');
TAiChatFactory.Instance.RegisterUserParam('MiDriver', 'SessionCaps', '[cap_Image, cap_Pdf]');
```

No es necesario eliminar los parámetros legacy existentes si ya se configuran los nuevos: cuando `ModelCaps`/`SessionCaps` están presentes en los params de factory, el RTTI los aplica vía los setters que activan `FNewSystemConfigured = True`, ignorando la traducción automática.

---

## 8. Patrones de configuración frecuentes

### Patrón 1: Modelo de texto puro (sin capacidades especiales)

```pascal
// Solo texto, sin tools
RegisterUserParam('Driver', 'ModelCaps',   '[]');
RegisterUserParam('Driver', 'SessionCaps', '[]');
RegisterUserParam('Driver', 'Tool_Active', 'False');
```

Resultado: `Gap = []` → `InternalRunCompletions` directo.

### Patrón 2: Modelo con visión nativa

```pascal
// El modelo puede ver imágenes directamente en completions
RegisterUserParam('Driver', 'ModelCaps',   '[cap_Image]');
RegisterUserParam('Driver', 'SessionCaps', '[cap_Image]');
RegisterUserParam('Driver', 'Tool_Active', 'True');
```

Resultado: `Gap = []` → las imágenes van directas al API de completions.

### Patrón 3: Modelo multimodal completo (Gemini 2.5 Flash)

```pascal
RegisterUserParam('Gemini', 'gemini-2.5-flash', 'ModelCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
RegisterUserParam('Gemini', 'gemini-2.5-flash', 'SessionCaps',
  '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
```

Resultado: `Gap = []` → todo va directo al completions nativo de Gemini.

### Patrón 4: Modelo con razonamiento (CoT/thinking)

```pascal
RegisterUserParam('Driver', 'modelo-reasoning', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
RegisterUserParam('Driver', 'modelo-reasoning', 'SessionCaps',  '[cap_Image, cap_Reasoning]');
RegisterUserParam('Driver', 'modelo-reasoning', 'ThinkingLevel', 'tlMedium');
```

Resultado: el driver activa el modo de razonamiento extendido según el nivel configurado.

### Patrón 5: TTS via endpoint dedicado

```pascal
// ModelCaps vacío = no hace completions / no entiende inputs
// Gap = [cap_GenAudio] → InternalRunSpeechGeneration
RegisterUserParam('Driver', 'modelo-tts', 'ModelCaps',    '[]');
RegisterUserParam('Driver', 'modelo-tts', 'SessionCaps',  '[cap_GenAudio]');
RegisterUserParam('Driver', 'modelo-tts', 'Tool_Active',  'False');
RegisterUserParam('Driver', 'modelo-tts', 'Voice',        'alloy');
```

### Patrón 6: Generación de imagen via endpoint dedicado

```pascal
// ModelCaps vacío = usa endpoint de imagen, no completions
// Gap = [cap_GenImage] → InternalRunImageGeneration
RegisterUserParam('Driver', 'modelo-imagen', 'ModelCaps',   '[]');
RegisterUserParam('Driver', 'modelo-imagen', 'SessionCaps', '[cap_GenImage]');
RegisterUserParam('Driver', 'modelo-imagen', 'Tool_Active', 'False');
```

### Patrón 7: Generación de imagen NATIVA via completions (Gemini)

```pascal
// cap_GenImage en ModelCaps Y SessionCaps = el modelo devuelve imagen en la respuesta de completions
// Gap = [] → InternalRunCompletions (el modelo genera imagen inline en la respuesta)
RegisterUserParam('Gemini', 'gemini-2.5-flash-image', 'ModelCaps',   '[cap_Image, cap_GenImage]');
RegisterUserParam('Gemini', 'gemini-2.5-flash-image', 'SessionCaps', '[cap_Image, cap_GenImage]');
RegisterUserParam('Gemini', 'gemini-2.5-flash-image', 'Tool_Active', 'False');
```

La diferencia con el Patrón 6: aquí `cap_GenImage` está en **ambos** `ModelCaps` y `SessionCaps`, por lo que no hay gap. El propio completions devuelve la imagen inline. En el Patrón 6, `cap_GenImage` solo está en `SessionCaps`, creando el gap que redirige al endpoint de imagen dedicado.

### Patrón 8: STT (transcripción) via endpoint dedicado

```pascal
// cap_Audio en ModelCaps (soporta audio) + Tool_Active=False (sin tools)
// Usar con ChatMode = cmTranscription
RegisterUserParam('Driver', 'whisper', 'ModelCaps',   '[cap_Audio]');
RegisterUserParam('Driver', 'whisper', 'SessionCaps', '[cap_Audio]');
RegisterUserParam('Driver', 'whisper', 'Tool_Active', 'False');
```

### Patrón 9: Modelo con bridge de visión para texto puro

```pascal
// El modelo (ej. Ollama texto puro) no tiene visión nativa
// El usuario quiere enviar imágenes → bridge automático las describe antes del prompt
RegisterUserParam('Ollama', 'ModelCaps',   '[]');          // no ve imágenes
RegisterUserParam('Ollama', 'SessionCaps', '[cap_Image]'); // sesión requiere imágenes
// Gap = [cap_Image] → en Fase 1, InternalRunImageDescription se ejecuta automáticamente
// La imagen se describe en texto y se adjunta al prompt
```

### Patrón 10: Generación de video

```pascal
// ModelCaps=[cap_Image]: acepta imagen como input (text-to-video o image-to-video)
// SessionCaps agrega cap_GenVideo: Gap=[cap_GenVideo] → InternalRunImageVideoGeneration
RegisterUserParam('Gemini', 'aa_veo-3.0-generate-preview', 'ModelCaps',   '[cap_Image]');
RegisterUserParam('Gemini', 'aa_veo-3.0-generate-preview', 'SessionCaps', '[cap_Image, cap_GenVideo]');
RegisterUserParam('Gemini', 'aa_veo-3.0-generate-preview', 'Tool_Active', 'False');
```

---

## 9. Referencia de capacidades por provider

### OpenAI

| Modelo | ModelCaps | SessionCaps | Tool_Active | Notas |
|--------|-----------|-------------|-------------|-------|
| gpt-4.1 / 4.1-mini / 4.1-nano | `[cap_Image]` | `[cap_Image]` | True | 1M ctx, 32K output |
| gpt-4o / gpt-4o-mini | `[cap_Image]` | `[cap_Image]` | True | |
| o3 | `[cap_Image, cap_Reasoning]` | `[cap_Image, cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| o3-pro | `[cap_Image, cap_Reasoning]` | `[cap_Image, cap_Reasoning]` | True | ThinkingLevel=tlHigh |
| o4-mini | `[cap_Image, cap_Reasoning]` | `[cap_Image, cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| o3/o4-mini-deep-research | `[cap_Reasoning, cap_WebSearch, cap_CodeInterpreter]` | idem | True | |
| gpt-4o-search-preview | `[cap_WebSearch]` | `[cap_WebSearch]` | False | |
| gpt-image-1 / dall-e-3 / dall-e-2 | `[]` | `[cap_GenImage]` | False | Gap → endpoint imagen |
| gpt-4o-mini-tts | `[]` | `[cap_GenAudio]` | False | Gap → endpoint TTS |
| gpt-4o-audio-preview | `[cap_Audio, cap_GenAudio]` | `[cap_Audio, cap_GenAudio]` | False | Audio I/O nativo en completions |
| gpt-4o-transcribe / mini-transcribe | `[cap_Audio]` | `[cap_Audio]` | False | STT nativo |
| aa_gpt-4.1-pdf | `[cap_Image, cap_Pdf]` | `[cap_Image, cap_Pdf]` | True | Perfil con PDF nativo |

### Gemini (Google)

| Modelo | ModelCaps | SessionCaps | Tool_Active | Notas |
|--------|-----------|-------------|-------------|-------|
| gemini-2.5-flash | `[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]` | idem | True | 1M ctx, 65K output |
| gemini-2.5-flash-lite | `[cap_Image, cap_Audio, cap_Video, cap_Pdf]` | idem | True | Budget/rápido |
| gemini-2.5-pro | `[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]` | idem | True | |
| gemini-3-pro-preview | idem que 2.5-pro | idem | True | ThinkingLevel=tlHigh |
| gemini-3.1-pro-preview | idem | idem | True | ThinkingLevel=tlHigh |
| gemini-2.5-flash-image | `[cap_Image, cap_GenImage]` | idem | False | Gen imagen nativa en completions |
| gemini-3-pro-image-preview | `[cap_Image, cap_GenImage]` | idem | False | Sin ThinkingLevel |
| gemini-2.5-flash-preview-tts | `[]` | `[cap_GenAudio]` | False | Gap → TTS |
| gemini-2.5-pro-preview-tts | `[]` | `[cap_GenAudio]` | False | Gap → TTS |
| aa_veo-2.0/3.0/3.1 | `[cap_Image]` | `[cap_Image, cap_GenVideo]` | False | Gap=[cap_GenVideo] → video |

### Claude (Anthropic)

| Configuración | ModelCaps | SessionCaps | Tool_Active |
|---|---|---|---|
| Global (todos los modelos) | `[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]` | idem | False |

Todos los modelos Claude actuales (Opus 4.6, Sonnet 4.6/4.5, Haiku 4.5) comparten las mismas capacidades nativas: visión, PDF, reasoning y web search.

### Groq

| Modelo | ModelCaps | SessionCaps | Tool_Active |
|--------|-----------|-------------|-------------|
| Global (default) | `[]` | `[]` | True |
| llama-3.1/3.3 | `[]` (hereda global) | `[]` | True |
| qwen/qwen-3-32b | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| deepseek-r1-distill-llama-70b | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| llama-4-scout / llama-4-maverick | `[cap_Image]` | `[cap_Image]` | True | |
| compound-beta / mini | `[cap_WebSearch, cap_CodeInterpreter]` | idem | False | Nativo, no tool calls |
| whisper-large-v3 / turbo | `[cap_Audio]` | `[cap_Audio]` | False | STT |
| canopylabs/orpheus-v1-english | `[]` | `[cap_GenAudio]` | False | Gap → TTS |

### DeepSeek

| Modelo | ModelCaps | SessionCaps | Tool_Active | Notas |
|--------|-----------|-------------|-------------|-------|
| deepseek-chat | `[]` | `[]` | True | Texto + tools, 128K ctx |
| deepseek-reasoner | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |

### Kimi (Moonshot AI)

| Modelo | ModelCaps | SessionCaps | Tool_Active |
|--------|-----------|-------------|-------------|
| kimi-k2 | `[]` | `[]` | True |
| kimi-k2.5 | `[cap_Image, cap_Pdf, cap_Reasoning]` | idem | True |
| kimi-k2-thinking | `[cap_Reasoning]` | `[cap_Reasoning]` | True |
| moonshot-v1-* | `[]` | `[]` | False |
| moonshot-v1-*-vision | `[cap_Image]` | `[cap_Image]` | False |

### xAI Grok

| Modelo | ModelCaps | SessionCaps | Tool_Active |
|--------|-----------|-------------|-------------|
| grok-3 | `[]` | `[]` | True |
| grok-3-mini | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlLow |
| grok-4-fast-reasoning | `[cap_Image, cap_Reasoning]` | idem | True | 2M ctx |
| grok-2-image-1212 / grok-imagine-* | `[]` | `[cap_GenImage]` | False | Gap → imagen |
| grok-imagine-video | `[]` | `[cap_GenVideo]` | False | Gap → video |

### Mistral

| Configuración | ModelCaps | SessionCaps | Tool_Active |
|---|---|---|---|
| Global (default) | `[cap_Image]` | `[cap_Image]` | True |
| magistral-medium/small | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| devstral-latest | `[]` | `[cap_Pdf, cap_Image]` | True | Sin visión propia |
| voxtral-mini/small | `[cap_Audio]` | `[cap_Audio]` | False | STT vía completions |
| mistral-ocr-latest | `[cap_Pdf]` | `[cap_Pdf]` | False | OCR vía /v1/ocr |

### Cohere

| Modelo | ModelCaps | SessionCaps | Tool_Active |
|--------|-----------|-------------|-------------|
| command-a-03-2025 | `[]` | `[]` | True |
| command-a-reasoning-08-2025 | `[cap_Reasoning]` | `[cap_Reasoning]` | True |
| command-a-vision-07-2025 | `[cap_Image]` | `[cap_Image]` | False |
| c4ai-aya-vision-8b/32b | `[cap_Image]` | `[cap_Image]` | False |

### Ollama (modelos locales)

| Configuración | ModelCaps | SessionCaps | Tool_Active |
|---|---|---|---|
| Global (default) | `[]` | `[]` | False |
| llama3.3 / qwen2.5 | `[]` | `[]` | True |
| qwen3:latest | `[cap_Reasoning]` | `[cap_Reasoning]` | True | ThinkingLevel=tlMedium |
| deepseek-r1:latest | `[cap_Reasoning]` | `[cap_Reasoning]` | False | ThinkingLevel=tlMedium |
| llama3.2-vision | `[cap_Image]` | `[cap_Image]` | False | |
| qwen2.5vl | `[cap_Image]` | `[cap_Image]` | True | |
| gemma3:1b/4b/12b/27b | `[cap_Image]` | `[cap_Image]` | True | |

---

## 10. Añadir un nuevo provider

Al crear un nuevo driver (`TAi[Provider]Chat` heredando de `TAiChat`), configurar las capacidades en `uMakerAi.Chat.Initializations.pas`:

```pascal
// 1. Configurar defaults globales del provider
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'Max_Tokens',  '16000');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'Tool_Active', 'True');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'ModelCaps',   '[cap_Image]');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'SessionCaps', '[cap_Image]');

// 2. Overrides por modelo
// Modelo básico (solo texto)
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-basic', 'ModelCaps',   '[]');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-basic', 'SessionCaps', '[]');

// Modelo con reasoning
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-think', 'ModelCaps',    '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-think', 'SessionCaps',  '[cap_Image, cap_Reasoning]');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-think', 'ThinkingLevel', 'tlMedium');

// Modelo TTS
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-tts', 'ModelCaps',   '[]');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-tts', 'SessionCaps', '[cap_GenAudio]');
TAiChatFactory.Instance.RegisterUserParam('MiProvider', 'mi-model-tts', 'Tool_Active', 'False');
```

### Preguntas de diseño al configurar un nuevo modelo

1. **¿Qué tipos de entrada acepta el endpoint de completions?**
   → Esas son las `ModelCaps` de entrada (`cap_Image`, `cap_Audio`, etc.)

2. **¿El endpoint de completions puede generar imágenes/audio/video inline?**
   → Añadir la `cap_Gen*` correspondiente a `ModelCaps` (y `SessionCaps`). Gap = 0, va directo.

3. **¿El modelo tiene endpoint separado de TTS/imagen/video?**
   → `ModelCaps = []`, `SessionCaps = [cap_Gen*]`. Gap activa el bridge.

4. **¿El modelo tiene razonamiento extendido?**
   → `cap_Reasoning` en `ModelCaps` y `SessionCaps`, más `ThinkingLevel`.

5. **¿El modelo soporta tool calling?**
   → `Tool_Active = True`.

---

## 11. Referencia de archivos fuente

| Propósito | Archivo |
|-----------|---------|
| Definición de `TAiCapability` y `TAiCapabilities` | `Source/Core/uMakerAi.Core.pas` (línea 65) |
| Declaración de `ModelCaps`/`SessionCaps` en `TAiChat` | `Source/Core/uMakerAi.Chat.pas` (línea 430) |
| Implementación de setters y sync con legacy | `Source/Core/uMakerAi.Chat.pas` (línea 2886) |
| Gap analysis y fases de orquestación (`RunNew`) | `Source/Core/uMakerAi.Chat.pas` (línea 3004) |
| Aplicación de params via RTTI (`ApplyParamsToChat`) | `Source/Chat/uMakerAi.Chat.AiConnection.pas` (línea 509) |
| Configuración de modelos por provider | `Source/Chat/uMakerAi.Chat.Initializations.pas` |

---

*Documentación generada para MakerAI v3.3 — marzo 2026*
*Fuente oficial del proyecto: https://makerai.cimamaker.com*
