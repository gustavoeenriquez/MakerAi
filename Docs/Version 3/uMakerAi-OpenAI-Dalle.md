# TAiDalle — Generación y Edición de Imágenes con OpenAI

**Unidad:** `uMakerAi.OpenAi.Dalle`  
**Componentes:** `TAiDalle`, `TAiDalleImageTool`  
**Versión MakerAI:** 3.4+  
**Última actualización:** 28/04/2026

---

## Contenido

1. [Descripción general](#1-descripción-general)
2. [Tipos y enumeraciones](#2-tipos-y-enumeraciones)
3. [TAiDalleImage — objeto resultado](#3-taidalleimage--objeto-resultado)
4. [TAiDalle — componente principal](#4-taidalle--componente-principal)
   - 4.1 [Propiedades](#41-propiedades)
   - 4.2 [Método Generate](#42-método-generate)
   - 4.3 [Método Edit](#43-método-edit)
   - 4.4 [Método Variation](#44-método-variation)
   - 4.5 [Método Upscale](#45-método-upscale)
   - 4.6 [Streaming](#46-streaming)
5. [TAiDalleImageTool — integración con TAiChatConnection](#5-taidalleimagingtool--integración-con-taichatconnection)
6. [Compatibilidad por modelo](#6-compatibilidad-por-modelo)
7. [API Key](#7-api-key)
8. [Ejemplos completos](#8-ejemplos-completos)

---

## 1. Descripción general

`TAiDalle` es un componente Delphi (`TComponent`) que encapsula la API de imágenes de OpenAI (`/v1/images/`). Puede usarse de dos formas:

**Modo standalone** — llamadas directas a `Generate`, `Edit`, `Variation` sin necesidad de chat:
```pascal
Dalle := TAiDalle.Create(nil);
Dalle.Model  := imGptImage2;
Dalle.ApiKey := '@OPENAI_API_KEY';
Img := Dalle.Generate('a red panda coding', '', is1024x1024);
```

**Modo integrado** — conectado a `TAiChatConnection` a través de `TAiDalleImageTool`. Cuando el modelo tiene `SessionCaps=[cap_GenImage]` y `ModelCaps=[]`, el framework detecta el gap y delega la generación al tool automáticamente:
```pascal
Tool := TAiDalleImageTool.Create(nil);
Tool.ApiKey := '@OPENAI_API_KEY';
Tool.Model  := imGptImage2;
AiConn.ImageTool := Tool;
AiConn.Run('Genera una imagen de un gato astronauta');
```

---

## 2. Tipos y enumeraciones

### TAiImageModel — modelos disponibles

| Valor | API string | Notas |
|-------|-----------|-------|
| `imDallE2` | `dall-e-2` | Deprecado mayo 2026. Soporta Variation |
| `imDallE3` | `dall-e-3` | Deprecado mayo 2026. n=1 siempre |
| `imGptImage1` | `gpt-image-1` | Actual. Soporta streaming |
| `imGptImage1Mini` | `gpt-image-1-mini` | Más rápido y económico. Soporta streaming |
| `imGptImage15` | `gpt-image-1.5` | Intermedio. Soporta streaming |
| `imGptImage2` | `gpt-image-2` | **Más reciente.** Sin streaming. n=1..8 máx |
| `imChatGptImageLatest` | `chatgpt-image-latest` | Alias al modelo más reciente. Soporta streaming |
| `imSDXL` | `sdxl` | Endpoint personalizado (no OpenAI oficial) |

### TAiImageSize — tamaños de imagen

| Valor | Resolución | Compatible con |
|-------|-----------|----------------|
| `is256x256` | 256×256 | dall-e-2 |
| `is512x512` | 512×512 | dall-e-2 |
| `is768x768` | 768×768 | SDXL |
| `is832x1216` | 832×1216 | SDXL portrait |
| `is1024x1024` | 1024×1024 | Todos |
| `is1024x1536` | 1024×1536 | gpt-image-* portrait |
| `is1024x1792` | 1024×1792 | dall-e-3 portrait |
| `is1216x832` | 1216×832 | SDXL landscape |
| `is1536x1024` | 1536×1024 | gpt-image-* landscape |
| `is1792x1024` | 1792×1024 | dall-e-3 landscape |
| `isAuto` | auto | Solo GPT Image (la API elige) |
| `is3840x2160` | 3840×2160 | gpt-image-2 (4K, experimental) |

### TAiImageQuality — calidad de generación

| Valor | API string | Compatible con |
|-------|-----------|----------------|
| `iqAuto` | *(no envía)* | Todos (default) |
| `iqStandard` | `standard` | dall-e-3 |
| `iqHD` | `hd` | dall-e-3 |
| `iqHigh` | `high` | gpt-image-* |
| `iqMedium` | `medium` | gpt-image-* |
| `iqLow` | `low` | gpt-image-* |

> **dall-e-3:** usa `iqHD` para la máxima calidad (mapeado a `'hd'`). `iqHigh` también activa HD.  
> **gpt-image-*:** usa `iqHigh`/`iqMedium`/`iqLow`. `iqAuto` deja elegir a la API.

### TAiImageOutputFormat — formato de salida

| Valor | Formato | Notas |
|-------|---------|-------|
| `ifPng` | PNG | Default. No requiere enviar parámetro |
| `ifJpeg` | JPEG | Soporta `OutputCompression` |
| `ifWebp` | WebP | Soporta `OutputCompression` |

> Solo disponible en modelos GPT Image. dall-e-2 y dall-e-3 siempre devuelven PNG.

### TAiImageBackground — fondo de imagen

| Valor | API string | Notas |
|-------|-----------|-------|
| `ibAuto` | *(no envía)* | Default |
| `ibTransparent` | `transparent` | Solo gpt-image-1/mini/1.5. **NO gpt-image-2** |
| `ibOpaque` | `opaque` | Todos los GPT Image |

### TAiImageStyle — estilo (solo dall-e-3)

| Valor | API string |
|-------|-----------|
| `isVivid` | `vivid` |
| `isNatural` | `natural` |

### TAiImageModeration — moderación de contenido

| Valor | API string | Efecto |
|-------|-----------|--------|
| `imodAuto` | *(no envía)* | Default — moderación estándar |
| `imodLow` | `low` | Moderación reducida para casos de uso específicos |

### TAiInputFidelity — fidelidad en edición (solo Edit)

| Valor | API string | Efecto |
|-------|-----------|--------|
| `ifdDefault` | *(no envía)* | Default |
| `ifdHigh` | `high` | Preserva al máximo la imagen original |
| `ifdLow` | `low` | Permite más creatividad en la edición |

> **gpt-image-2:** `InputFidelity` es ignorado — la API siempre aplica alta fidelidad automáticamente.

### TAiImageResponseFormat — formato de respuesta (solo dall-e-2/3)

| Valor | Efecto |
|-------|--------|
| `irfUrl` | Responde con URL temporal |
| `irfBase64Json` | Responde con imagen en base64 |

> Los modelos GPT Image **siempre** responden en base64. Este parámetro se ignora para ellos.

---

## 3. TAiDalleImage — objeto resultado

`TAiDalleImage` es la clase que contiene una imagen generada. Es devuelta por `Generate`, `Edit` y `Variation`.

**No liberar manualmente** — pertenece al array interno `FImages` del `TAiDalle` y se libera al próxima llamada o al destruir `TAiDalle`.

### Propiedades de TAiDalleImage

| Propiedad | Tipo | Descripción |
|-----------|------|-------------|
| `Image` | `TMemoryStream` | Stream con los bytes de la imagen. Si se recibió por URL, descarga al acceder. Siempre posicionado en 0 |
| `Base64` | `string` | Representación base64 de la imagen |
| `UrlFile` | `string` | URL temporal (solo dall-e-2/3 con `irfUrl`). Vacío en GPT Image |
| `RevisedPrompt` | `string` | Prompt revisado/expandido por la API (dall-e-3) |
| `Background` | `string` | Valor de background recibido en la respuesta |
| `OutputFormat` | `string` | Formato de salida recibido (`png`, `jpeg`, `webp`) |
| `Quality` | `string` | Calidad recibida en la respuesta |
| `Size` | `string` | Resolución recibida (e.g. `"1024x1024"`) |
| `Usage` | `TJSONObject` | Token usage si la API lo incluye. Puede ser `nil` |

### Guardar la imagen a disco

```pascal
var
  Img: TAiDalleImage;
  FS: TFileStream;
begin
  Img := Dalle.Generate('un dragón azul', '', is1024x1024);
  if Assigned(Img) and (Img.Image.Size > 0) then
  begin
    FS := TFileStream.Create('dragon.png', fmCreate);
    try
      Img.Image.Position := 0;
      FS.CopyFrom(Img.Image, Img.Image.Size);
    finally
      FS.Free;
    end;
  end;
end;
```

---

## 4. TAiDalle — componente principal

### 4.1 Propiedades

| Propiedad | Tipo | Default | Descripción |
|-----------|------|---------|-------------|
| `ApiKey` | `string` | — | API key de OpenAI. Soporta `@VAR_NAME` para leer de variable de entorno |
| `Url` | `string` | `https://api.openai.com/v1/` | Base URL. Cambiar para proxies o endpoints compatibles |
| `Model` | `TAiImageModel` | `imDallE3` | Modelo a usar |
| `Quality` | `TAiImageQuality` | `iqAuto` | Calidad de generación |
| `Style` | `TAiImageStyle` | `isVivid` | Estilo visual (solo dall-e-3) |
| `ResponseFormat` | `TAiImageResponseFormat` | `irfUrl` | URL o base64 (solo dall-e-2/3) |
| `Background` | `TAiImageBackground` | `ibAuto` | Fondo (solo GPT Image, no gpt-image-2 transparent) |
| `OutputFormat` | `TAiImageOutputFormat` | `ifPng` | Formato de salida (solo GPT Image) |
| `OutputCompression` | `Integer` | `0` | Compresión 0-100 para jpeg/webp. 0 = no enviar |
| `Moderation` | `TAiImageModeration` | `imodAuto` | Nivel de moderación (solo GPT Image) |
| `InputFidelity` | `TAiInputFidelity` | `ifdDefault` | Fidelidad en edición (no aplicable a gpt-image-2) |
| `Stream` | `Boolean` | `False` | Activar streaming SSE (no disponible en gpt-image-2) |
| `PartialImages` | `Integer` | `1` | Número de imágenes parciales en streaming (0-3) |
| `NegativePrompt` | `TStrings` | — | Prompt negativo (solo SDXL) |
| `User` | `string` | `'user'` | Identificador de usuario para auditoría |
| `Prompt` | `string` | — | Read-only. Último prompt enviado |

**Propiedades SDXL** (solo endpoint personalizado):

| Propiedad | Tipo | Default | Descripción |
|-----------|------|---------|-------------|
| `Steps` | `Integer` | `30` | Pasos de difusión |
| `GuidanceScale` | `Single` | `7.0` | Escala de guía |
| `Seed` | `Int64` | `-1` | Semilla (-1 = aleatoria) |
| `UseRefiner` | `Boolean` | `False` | Usar refinador SDXL-XL |
| `Strength` | `Single` | `0` | Fuerza de img2img |
| `LoraPath` | `string` | — | Ruta a adaptador LoRA |
| `AutoUpscale` | `Boolean` | `False` | Upscale automático |
| `EnhanceFace` | `Boolean` | `False` | Mejora de rostros |

### 4.2 Método Generate

```pascal
function Generate(
  const aPrompt        : string;         // Prompt de generación
  const aNegativePrompt: string;         // Prompt negativo (solo SDXL; vacío para OpenAI)
  ASize                : TAiImageSize;   // Resolución deseada
  N                    : Integer = 1;    // Número de imágenes (ver límites por modelo)
  aImage               : TMemoryStream = nil  // Imagen de referencia (solo SDXL img2img)
): TAiDalleImage;
```

Retorna la **primera imagen** generada. Para acceder a múltiples imágenes (N>1) usar el array interno — aunque en la mayoría de casos N=1 es suficiente.

**Límite de N por modelo:**

| Modelo | Máximo N |
|--------|---------|
| dall-e-2 | 10 |
| dall-e-3 | 1 (siempre) |
| gpt-image-1/mini/1.5 | 10 |
| **gpt-image-2** | **8** |
| chatgpt-image-latest | 10 |

**Ejemplo básico:**
```pascal
var
  Dalle: TAiDalle;
  Img  : TAiDalleImage;
begin
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey  := '@OPENAI_API_KEY';
    Dalle.Model   := imGptImage2;
    Dalle.Quality := iqHigh;

    Img := Dalle.Generate(
      'A futuristic Delphi IDE with holographic code floating in the air',
      '',          // negative prompt — vacío para OpenAI
      is1024x1024  // tamaño
    );

    if Assigned(Img) and (Img.Image.Size > 0) then
      Img.Image.SaveToFile('output.png');
  finally
    Dalle.Free;
  end;
end;
```

**Ejemplo con WebP + compresión:**
```pascal
Dalle.Model             := imGptImage1;
Dalle.OutputFormat      := ifWebp;
Dalle.OutputCompression := 80;  // 80% calidad
Dalle.Quality           := iqMedium;
Img := Dalle.Generate('landscape at sunset', '', is1536x1024);
```

**Ejemplo 4K con gpt-image-2:**
```pascal
Dalle.Model   := imGptImage2;
Dalle.Quality := iqHigh;
Img := Dalle.Generate('epic mountain range', '', is3840x2160);
// Nota: 4K es experimental en gpt-image-2 y puede ser más lento
```

### 4.3 Método Edit

Edita una o más imágenes existentes usando un prompt. Opcionalmente acepta una máscara para inpainting.

```pascal
function Edit(
  aMediaFiles: TAiMediaFiles;   // Imágenes de entrada (TAiMediaFiles = TList<TAiMediaFile>)
  aMaskFile  : TAiMediaFile;    // Máscara opcional (PNG con canal alfa). nil = sin máscara
  const aPrompt: string;        // Descripción de la edición
  ASize      : TAiImageSize;    // Tamaño de salida
  N          : Integer = 1      // Número de variantes
): TAiDalleImage;
```

**Compatibilidad de modelos para Edit:**
- `imDallE2` — 1 imagen, con o sin máscara, solo PNG
- `imGptImage1`, `imGptImage1Mini`, `imGptImage15`, `imGptImage2`, `imChatGptImageLatest` — hasta 10 imágenes de entrada, PNG/JPEG/WebP

**Máscara para inpainting:**
- Debe ser PNG con canal alfa (RGBA)
- Áreas **transparentes** (alfa=0) = zonas a editar
- Áreas **opacas** (alfa=255) = zonas que se preservan

**Preparar imágenes con TAiMediaFile:**
```pascal
uses uMakerAi.Core;

var
  Dalle     : TAiDalle;
  MediaFiles: TAiMediaFiles;
  Img, Mask : TAiMediaFile;
  Result    : TAiDalleImage;
begin
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey := '@OPENAI_API_KEY';
    Dalle.Model  := imGptImage2;

    MediaFiles := TAiMediaFiles.Create;
    Img  := TAiMediaFile.Create;
    Img.LoadFromFile('foto.jpg');
    MediaFiles.Add(Img);

    // Sin máscara — edición global
    Result := Dalle.Edit(MediaFiles, nil,
      'Remove the background and replace with a sunny beach',
      is1024x1024);

    if Assigned(Result) then
      Result.Image.SaveToFile('edited.png');
  finally
    MediaFiles.Free;  // libera los TAiMediaFile internos
    Dalle.Free;
  end;
end;
```

**Con máscara (inpainting):**
```pascal
Mask := TAiMediaFile.Create;
Mask.LoadFromFile('mask.png');  // PNG RGBA: transparente=editar, opaco=preservar

Result := Dalle.Edit(MediaFiles, Mask,
  'Replace the masked area with a golden sunset sky',
  is1024x1024);
Mask.Free;
```

> **gpt-image-2 + InputFidelity:** Este modelo ignora la propiedad `InputFidelity` — siempre procesa en alta fidelidad automáticamente. No es necesario configurarla.

### 4.4 Método Variation

Genera variaciones de una imagen existente. **Solo disponible para dall-e-2.**

```pascal
function Variation(
  aImageFile: TAiMediaFile;  // Imagen PNG cuadrada (256, 512 o 1024px)
  ASize     : TAiImageSize;  // Tamaño de salida
  N         : Integer = 1    // Número de variaciones
): TAiDalleImage;
```

```pascal
Dalle.Model := imDallE2;

var OriginalImg := TAiMediaFile.Create;
OriginalImg.LoadFromFile('original.png');

var Variant := Dalle.Variation(OriginalImg, is1024x1024, 3);
// Retorna la primera variación; las otras quedan en el array interno

OriginalImg.Free;
```

### 4.5 Método Upscale

Aumenta la resolución de una imagen usando Real-ESRGAN (endpoint personalizado — no disponible en la API oficial de OpenAI).

```pascal
function Upscale(
  aImage     : TAiMediaFile;
  AScale     : Integer = 2;         // Factor de escala (2x, 4x)
  AFaceEnhance: Boolean = False     // Mejora específica de rostros
): TAiMediaFile;
```

```pascal
Dalle.Url := 'http://mi-servidor-custom/v1/';  // endpoint personalizado
var Upscaled := Dalle.Upscale(OriginalMediaFile, 4, True);
if Assigned(Upscaled) then
begin
  Upscaled.Content.SaveToFile('upscaled.png');
  Upscaled.Free;
end;
```

> `Upscale` devuelve un `TAiMediaFile` nuevo que **debe liberarse** por el caller.

### 4.6 Streaming

El streaming SSE permite recibir imágenes parciales durante la generación. Disponible en gpt-image-1, gpt-image-1-mini, gpt-image-1.5 y chatgpt-image-latest. **gpt-image-2 NO soporta streaming.**

**Activación:**
```pascal
Dalle.Model         := imGptImage1;
Dalle.Stream        := True;
Dalle.PartialImages := 2;  // 0-3 imágenes intermedias
```

**Eventos de streaming:**

```pascal
// Se dispara por cada imagen parcial recibida
Dalle.OnPartialImageReceived := procedure(Sender: TObject;
  const APartialImage: TAiDalleImage; AIndex: Integer)
begin
  // AIndex: 0, 1, 2... según el número de parciales configurado
  // APartialImage.Image tiene el estado intermedio de la imagen
  TThread.Queue(nil, procedure
  begin
    // Actualizar UI con la imagen parcial
    ImageControl.Bitmap.LoadFromStream(APartialImage.Image);
    APartialImage.Free;  // el evento transfiere ownership al handler
  end);
end;

// Se dispara cuando la imagen final está lista
Dalle.OnStreamCompleted := procedure(Sender: TObject;
  const AFinalImage: TAiDalleImage)
begin
  TThread.Queue(nil, procedure
  begin
    ImageControl.Bitmap.LoadFromStream(AFinalImage.Image);
    AFinalImage.Image.SaveToFile('final.png');
    AFinalImage.Free;
  end);
end;

// Se dispara si hay un error durante el streaming
Dalle.OnStreamError := procedure(Sender: TObject; const AErrorMessage: string)
begin
  TThread.Queue(nil, procedure
  begin
    ShowMessage('Error streaming: ' + AErrorMessage);
  end);
end;
```

> El streaming de `TAiDalle.Generate` es **bloqueante** en el hilo que lo llama. Ejecutarlo en un hilo separado o usar `TTask.Run`.

---

## 5. TAiDalleImageTool — integración con TAiChatConnection

`TAiDalleImageTool` hereda de `TAiImageToolBase` y actúa como bridge entre el chat conversacional y `TAiDalle`. Cuando `TAiChatConnection` detecta el gap `cap_GenImage`, delega la generación a este tool automáticamente.

### Propiedades de TAiDalleImageTool

Todas las propiedades de `TAiDalle` relevantes están expuestas como delegates:

| Propiedad | Tipo | Descripción |
|-----------|------|-------------|
| `ApiKey` | `string` | API key (soporta `@VAR_NAME`) |
| `Url` | `string` | Base URL |
| `Model` | `TAiImageModel` | Modelo a usar |
| `Quality` | `TAiImageQuality` | Calidad |
| `Style` | `TAiImageStyle` | Estilo (dall-e-3) |
| `OutputFormat` | `TAiImageOutputFormat` | Formato de salida |
| `Background` | `TAiImageBackground` | Fondo |
| `OutputCompression` | `Integer` | Compresión jpeg/webp |
| `Moderation` | `TAiImageModeration` | Moderación |
| `InputFidelity` | `TAiInputFidelity` | Fidelidad de edición |
| `NegativePrompt` | `TStrings` | Prompt negativo |
| `ImageSize` | `TAiImageSize` | Tamaño de imagen generada |
| `Dalle` | `TAiDalle` | Permite inyectar un `TAiDalle` pre-configurado |

### Registro en TAiChatConnection

Para que el framework use `TAiDalleImageTool`, el modelo debe estar registrado en `uMakerAi.Chat.Initializations.pas` con `SessionCaps=[cap_GenImage]` y `ModelCaps=[]`:

```pascal
// Ya registrado para modelos OpenAI:
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-image-2',
  'ModelCaps',   '[]');
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-image-2',
  'SessionCaps', '[cap_GenImage]');
TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'gpt-image-2',
  'Tool_Active', 'False');
```

### Uso básico con TAiChatConnection

```pascal
var
  AiConn : TAiChatConnection;
  Tool   : TAiDalleImageTool;
begin
  AiConn := TAiChatConnection.Create(nil);
  Tool   := TAiDalleImageTool.Create(nil);
  try
    // Configurar el tool
    Tool.ApiKey    := '@OPENAI_API_KEY';
    Tool.Model     := imGptImage2;
    Tool.Quality   := iqHigh;
    Tool.ImageSize := is1024x1024;

    // Conectar al chat
    AiConn.DriverName  := 'OpenAi';
    AiConn.Model       := 'gpt-image-2';
    AiConn.ApiKey      := '@OPENAI_API_KEY';
    AiConn.ImageTool   := Tool;
    AiConn.Asynchronous := True;

    // El chat interpreta el prompt y genera la imagen
    AiConn.OnReceiveDataEnd := procedure(...)
    begin
      // ResMsg.MediaFiles[0] contiene la imagen generada
    end;

    AiConn.Run('Generate a photorealistic image of a mountain at sunset');
  finally
    AiConn.ImageTool := nil;
    Tool.Free;
    AiConn.Free;
  end;
end;
```

### Inyección de TAiDalle pre-configurado

```pascal
var
  LDalle: TAiDalle;
  LTool : TAiDalleImageTool;
begin
  LDalle        := TAiDalle.Create(nil);
  LDalle.ApiKey := '@OPENAI_API_KEY';
  LDalle.Model  := imGptImage2;
  LDalle.Quality := iqMedium;

  LTool       := TAiDalleImageTool.Create(nil);
  LTool.Dalle := LDalle;       // Inyectar el TAiDalle configurado
  LTool.ImageSize := is1536x1024;

  AiConn.ImageTool := LTool;
  // ...

  // Al liberar: LTool libera el TAiDalle interno original (component framework).
  // LDalle debe liberarse por el caller explícitamente.
  LTool.Free;
  LDalle.Free;
end;
```

### Modo asíncrono (requerido para integración con chat)

`TAiDalleImageTool.ExecuteImageGeneration` llama a `TAiDalle.Generate` de forma **bloqueante** dentro del hilo background del framework. Esto requiere que `TAiChatConnection` esté en modo asíncrono:

```pascal
AiConn.Asynchronous := True;  // OBLIGATORIO para image generation con tool
// o bien:
AiConn.Params.Values['Asynchronous'] := 'True';
```

En modo síncrono, `TTask.Run` haría que `RunNew` dispare `DoDataEnd` antes de que la imagen esté lista.

---

## 6. Compatibilidad por modelo

| Característica | dall-e-2 | dall-e-3 | gpt-image-1 | gpt-image-2 |
|----------------|---------|---------|------------|------------|
| Generate | ✅ | ✅ | ✅ | ✅ |
| Edit | ✅ (1 img) | ❌ | ✅ (hasta 10) | ✅ (hasta 10) |
| Variation | ✅ | ❌ | ❌ | ❌ |
| Streaming | ❌ | ❌ | ✅ | ❌ |
| Máx imágenes (N) | 10 | 1 | 10 | **8** |
| Formato webp/jpeg | ❌ | ❌ | ✅ | ✅ |
| Fondo transparente | ❌ | ❌ | ✅ | ❌ |
| Fondo opaco | ❌ | ❌ | ✅ | ✅ |
| response_format | ✅ | ✅ | ❌ | ❌ |
| input_fidelity (Edit) | ❌ | ❌ | ✅ | ❌ (auto) |
| Moderación | ❌ | ❌ | ✅ | ✅ |
| isAuto (tamaño) | ❌ | ❌ | ✅ | ✅ |
| is3840x2160 (4K) | ❌ | ❌ | ❌ | ✅ (exp.) |
| Deprecado mayo 2026 | ✅ | ✅ | ❌ | ❌ |

---

## 7. API Key

`TAiDalle` soporta dos formas de configurar la API key:

```pascal
// Literal (no recomendado en código fuente)
Dalle.ApiKey := 'sk-proj-XXXXXXXXXXXXXXXX';

// Variable de entorno (recomendado)
Dalle.ApiKey := '@OPENAI_API_KEY';
// Resuelve GetEnvironmentVariable('OPENAI_API_KEY') en runtime
```

La resolución ocurre en el getter de `ApiKey`, transparente para el caller.

---

## 8. Ejemplos completos

### Ejemplo 1: Generación simple con gpt-image-2

```pascal
procedure TForm1.ButtonGenerateClick(Sender: TObject);
var
  Dalle: TAiDalle;
  Img  : TAiDalleImage;
begin
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey  := '@OPENAI_API_KEY';
    Dalle.Model   := imGptImage2;
    Dalle.Quality := iqHigh;

    Img := Dalle.Generate(
      'A serene Japanese garden with cherry blossoms and a koi pond at golden hour',
      '',
      is1536x1024
    );

    if Assigned(Img) and (Img.Image.Size > 0) then
    begin
      Image1.Bitmap.LoadFromStream(Img.Image);
      Img.Image.SaveToFile(TPath.GetTempPath + 'garden.png');
    end;
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
  Dalle.Free;
end;
```

### Ejemplo 2: Edición con máscara (inpainting)

```pascal
procedure EditWithMask(const ASourceFile, AMaskFile: string; const APrompt: string);
var
  Dalle    : TAiDalle;
  Files    : TAiMediaFiles;
  Source   : TAiMediaFile;
  Mask     : TAiMediaFile;
  Result   : TAiDalleImage;
begin
  Dalle  := TAiDalle.Create(nil);
  Files  := TAiMediaFiles.Create;
  Source := TAiMediaFile.Create;
  Mask   := TAiMediaFile.Create;
  try
    Dalle.ApiKey := '@OPENAI_API_KEY';
    Dalle.Model  := imGptImage2;

    Source.LoadFromFile(ASourceFile);
    Files.Add(Source);
    Mask.LoadFromFile(AMaskFile);  // PNG con alfa: transparente = editar

    Result := Dalle.Edit(Files, Mask, APrompt, is1024x1024);

    if Assigned(Result) then
      Result.Image.SaveToFile('edited_result.png');
  finally
    Files.Free;  // libera Source internamente
    Mask.Free;
    Dalle.Free;
  end;
end;
```

### Ejemplo 3: Streaming con gpt-image-1

```pascal
procedure TForm1.GenerateWithStreaming;
var
  Dalle: TAiDalle;
begin
  Dalle := TAiDalle.Create(nil);

  Dalle.ApiKey      := '@OPENAI_API_KEY';
  Dalle.Model       := imGptImage1;
  Dalle.Stream      := True;
  Dalle.PartialImages := 2;

  Dalle.OnPartialImageReceived :=
    procedure(Sender: TObject; const Img: TAiDalleImage; Idx: Integer)
    begin
      TThread.Queue(nil, procedure
      begin
        StatusBar1.SimpleText := Format('Imagen parcial %d recibida...', [Idx + 1]);
        ImagePreview.Bitmap.LoadFromStream(Img.Image);
        Img.Free;
      end);
    end;

  Dalle.OnStreamCompleted :=
    procedure(Sender: TObject; const Img: TAiDalleImage)
    begin
      TThread.Queue(nil, procedure
      begin
        ImageFinal.Bitmap.LoadFromStream(Img.Image);
        Img.Image.SaveToFile('streaming_result.png');
        StatusBar1.SimpleText := 'Generación completada.';
        Img.Free;
      end);
    end;

  // Ejecutar en hilo separado porque Generate es bloqueante
  TTask.Run(procedure
  begin
    try
      Dalle.Generate('A cyberpunk city at night with neon lights', '', is1024x1024);
    finally
      TThread.Queue(nil, procedure begin Dalle.Free; end);
    end;
  end);
end;
```

### Ejemplo 4: Integración completa con TAiChatConnection

```pascal
procedure TForm1.SetupImageChat;
var
  Tool: TAiDalleImageTool;
begin
  // Tool: configurar una vez, reutilizar
  Tool := TAiDalleImageTool.Create(Self);  // Self como owner para gestión automática
  Tool.ApiKey    := '@OPENAI_API_KEY';
  Tool.Model     := imGptImage2;
  Tool.Quality   := iqHigh;
  Tool.ImageSize := is1024x1024;

  // Chat: configurar
  AiConn.DriverName   := 'OpenAi';
  AiConn.Model        := 'gpt-image-2';
  AiConn.ApiKey       := '@OPENAI_API_KEY';
  AiConn.ImageTool    := Tool;
  AiConn.Asynchronous := True;  // REQUERIDO para image generation

  AiConn.OnReceiveDataEnd :=
    procedure(Sender: TObject; ResMsg: TAiChatMessage; const Data: string)
    begin
      // Si hay imagen generada, está en ResMsg.MediaFiles[0]
      if ResMsg.MediaFiles.Count > 0 then
      begin
        var MediaFile := ResMsg.MediaFiles[0];
        Image1.Bitmap.LoadFromStream(MediaFile.Content);
        Caption1.Text := ResMsg.Prompt;  // revised prompt o el original
      end;
    end;

  // El usuario escribe naturalmente; el LLM decide si generar imagen
  AiConn.Run('Genera una imagen de un astronauta tocando guitarra en la Luna');
end;
```

### Ejemplo 5: Variación con dall-e-2

```pascal
procedure TForm1.ButtonVariationClick(Sender: TObject);
var
  Dalle   : TAiDalle;
  Original: TAiMediaFile;
  Variant : TAiDalleImage;
begin
  Dalle    := TAiDalle.Create(nil);
  Original := TAiMediaFile.Create;
  try
    Dalle.ApiKey := '@OPENAI_API_KEY';
    Dalle.Model  := imDallE2;

    Original.LoadFromFile('original.png');  // PNG cuadrado obligatorio

    Variant := Dalle.Variation(Original, is1024x1024);

    if Assigned(Variant) then
      Variant.Image.SaveToFile('variation.png');
  finally
    Original.Free;
    Dalle.Free;
  end;
end;
```

---

## Dependencias de unidades

Para usar `TAiDalle` o `TAiDalleImageTool`, agregar al `uses`:

```pascal
uses
  uMakerAi.OpenAi.Dalle,   // TAiDalle, TAiDalleImageTool y todos los tipos/enums
  uMakerAi.Core,            // TAiMediaFile, TAiMediaFiles (para Edit/Variation)
  uMakerAi.Chat.Messages;   // TAiChatMessage (solo si se extiende TAiImageToolBase)
```

Para uso con `TAiChatConnection`:
```pascal
uses
  uMakerAi.Chat.AiConnection,      // TAiChatConnection
  uMakerAi.Chat.Initializations,   // Registro de drivers (carga gpt-image-2)
  uMakerAi.OpenAi.Dalle;           // TAiDalleImageTool
```

---

*Documentación generada para MakerAI v3.4 — uMakerAi.OpenAi.Dalle.pas*  
*Autor: Gustavo Enríquez — gustavoeenriquez@gmail.com*
