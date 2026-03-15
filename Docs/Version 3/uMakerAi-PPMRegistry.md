# Integración con PPM Registry

**MakerAI v3.3 — Documentación Técnica**
Última actualización: Marzo 2026

---

## Tabla de Contenidos

1. [¿Qué es PPM?](#1-qué-es-ppm)
2. [TAiPrompts — Integración con Prompts](#2-taiprompts--integración-con-prompts)
   - [Propiedad PPMRegistryUrl](#21-propiedad-ppmregistryurl)
   - [SearchPPM — Buscar prompts](#22-searchppm--buscar-prompts)
   - [LoadFromPPM — Importar un prompt](#23-loadfromppm--importar-un-prompt)
   - [Conversión de placeholders](#24-conversión-de-placeholders)
3. [TAiFunctions — Integración con Herramientas MCP](#3-taifunctions--integración-con-herramientas-mcp)
   - [SearchPPMMCP — Descubrir herramientas](#31-searchppmmcp--descubrir-herramientas)
   - [ImportMCPFromPPM — Registrar una herramienta](#32-importmcpfromppm--registrar-una-herramienta)
4. [Ejemplos completos](#4-ejemplos-completos)
5. [Referencia de la API REST de PPM](#5-referencia-de-la-api-rest-de-ppm)
6. [Archivos fuente](#6-archivos-fuente)

---

## 1. ¿Qué es PPM?

**PPM (PascalAI Package Manager)** es un registry público de paquetes para el ecosistema Pascal/Delphi AI. Permite publicar y consumir:

| Tipo | Descripción |
|------|-------------|
| `prompt` | Plantillas de prompts reutilizables con variables `{{nombre}}` |
| `mcp` | Definiciones de herramientas MCP con JSON Schema |
| `pai` | Paquetes de librerías Pascal AI |
| `clib` | Bindings de librerías C |

**URL oficial del registry:** `https://registry.pascalai.org`

MakerAI se integra con los tipos `prompt` y `mcp` a través de `TAiPrompts` y `TAiFunctions` respectivamente. Los endpoints públicos de búsqueda y descarga **no requieren autenticación**.

---

## 2. TAiPrompts — Integración con Prompts

Archivo fuente: `Source/Core/uMakerAi.Prompts.pas`

### 2.1 Propiedad PPMRegistryUrl

```pascal
property PPMRegistryUrl: String;  // published
```

URL base del registry PPM. Por defecto apunta al registry oficial. Solo es necesario cambiarla si se usa una instancia privada del servidor.

```pascal
// Usar el registry oficial (por defecto, no hace falta asignarlo)
AiPrompts1.PPMRegistryUrl := 'https://registry.pascalai.org';

// Usar un registry privado interno
AiPrompts1.PPMRegistryUrl := 'http://mi-servidor-interno:8080';
```

La propiedad es visible en el Object Inspector del IDE y se puede configurar en tiempo de diseño.

---

### 2.2 SearchPPM — Buscar prompts

```pascal
function SearchPPM(
  const AQuery: String;
  const AType: String = 'prompt';
  APage: Integer = 1;
  APerPage: Integer = 20
): TJSONObject;
```

Busca paquetes en el registry. Devuelve el JSON de resultados. **El llamador es responsable de liberar el objeto devuelto.**

**Parámetros:**

| Parámetro | Tipo | Descripción |
|-----------|------|-------------|
| `AQuery` | String | Texto de búsqueda (busca en nombre y descripción) |
| `AType` | String | Filtro de tipo: `'prompt'`, `'mcp'`, `'pai'`, `'clib'` |
| `APage` | Integer | Página de resultados (base 1) |
| `APerPage` | Integer | Resultados por página (máximo 100) |

**Estructura del JSON devuelto:**

```json
{
  "packages": [
    {
      "name": "code-review",
      "type": "prompt",
      "description": "Prompt para revisión de código con análisis de seguridad",
      "version": "1.2.0",
      "author": "gustavoeenriquez",
      "downloads": 850
    }
  ],
  "total": 42,
  "page": 1,
  "per_page": 20
}
```

**Ejemplo de uso:**

```pascal
var
  LResult: TJSONObject;
  LPackages: TJSONArray;
  I: Integer;
begin
  LResult := AiPrompts1.SearchPPM('code review');
  if not Assigned(LResult) then
  begin
    ShowMessage('Error al conectar con el registry.');
    Exit;
  end;
  try
    LPackages := LResult.GetValue<TJSONArray>('packages');
    for I := 0 to LPackages.Count - 1 do
    begin
      var LPkg := LPackages.Items[I] as TJSONObject;
      Memo1.Lines.Add(Format('%s v%s — %s',
        [LPkg.GetValue<String>('name'),
         LPkg.GetValue<String>('version'),
         LPkg.GetValue<String>('description')]));
    end;
  finally
    LResult.Free;
  end;
end;
```

---

### 2.3 LoadFromPPM — Importar un prompt

```pascal
function LoadFromPPM(
  const AName: String;
  const AVersion: String = ''
): TAiPromptItem;
```

Descarga un prompt del registry y lo agrega a la colección `Items` del componente. Si ya existe un prompt con el mismo nombre, lo actualiza en lugar de duplicarlo.

**Parámetros:**

| Parámetro | Tipo | Descripción |
|-----------|------|-------------|
| `AName` | String | Nombre exacto del paquete en el registry |
| `AVersion` | String | Versión a descargar. Si está vacío, resuelve automáticamente la última versión disponible |

**Retorno:** `TAiPromptItem` cargado, o `nil` si el paquete no existe o hay un error de red.

**Ejemplo básico:**

```pascal
var
  LItem: TAiPromptItem;
begin
  // Cargar la última versión disponible
  LItem := AiPrompts1.LoadFromPPM('code-review');
  if Assigned(LItem) then
    ShowMessage('Prompt cargado: ' + LItem.Nombre)
  else
    ShowMessage('No se pudo cargar el prompt.');
end;
```

**Ejemplo con versión específica:**

```pascal
LItem := AiPrompts1.LoadFromPPM('sql-generator', '2.1.0');
```

**Usar el prompt después de importarlo:**

```pascal
// Después de LoadFromPPM, el prompt está disponible como cualquier otro:
var LTexto := AiPrompts1.GetTemplate('code-review', ['language=Delphi', 'focus=security']);

// Con TStringList:
var LParams := TStringList.Create;
try
  LParams.Values['language'] := 'Delphi';
  LParams.Values['focus'] := 'performance';
  LTexto := AiPrompts1.GetTemplate('code-review', LParams);
finally
  LParams.Free;
end;
```

---

### 2.4 Conversión de placeholders

Los prompts en PPM usan la sintaxis `{{nombre_variable}}`. Al importarlos con `LoadFromPPM`, MakerAI los convierte **automáticamente** al formato nativo `<#nombre_variable>`.

| Formato PPM | Formato MakerAI | Descripción |
|-------------|-----------------|-------------|
| `{{language}}` | `<#language>` | Variable de texto simple |
| `{{code_to_review}}` | `<#code_to_review>` | Variable con guión bajo |
| `{{focus}}` | `<#focus>` | Variable de foco |

**Ejemplo de prompt en PPM (`code-review.prompt`):**

```
Eres un revisor de código experto en {{language}}.

Analiza el siguiente código:

{{code_to_review}}

Aspectos a revisar: {{focus}}
```

**Después de `LoadFromPPM`, el prompt queda en MakerAI como:**

```
Eres un revisor de código experto en <#language>.

Analiza el siguiente código:

<#code_to_review>

Aspectos a revisar: <#focus>
```

---

## 3. TAiFunctions — Integración con Herramientas MCP

Archivo fuente: `Source/Tools/uMakerAi.Tools.Functions.pas`

Los paquetes MCP en PPM contienen un **JSON Schema** que describe qué parámetros acepta una herramienta. A diferencia de los prompts, la herramienta MCP en sí (el servidor que la ejecuta) no está incluida en PPM — el registry funciona como **catálogo de descubrimiento**. El desarrollador debe configurar la URL o comando del servidor MCP por separado.

---

### 3.1 SearchPPMMCP — Descubrir herramientas

```pascal
function SearchPPMMCP(
  const AQuery: String;
  APage: Integer = 1;
  APerPage: Integer = 20;
  const ARegistryUrl: String = 'https://registry.pascalai.org'
): TJSONObject;
```

Busca herramientas MCP disponibles en el registry. El resultado incluye el JSON Schema de cada herramienta inline. **El llamador es responsable de liberar el objeto devuelto.**

**Ejemplo de JSON devuelto:**

```json
{
  "tools": [
    {
      "name": "mcp-web-search",
      "description": "Búsqueda web con Brave Search API",
      "version": "1.2.0",
      "downloads": 3500,
      "schema": {
        "type": "object",
        "title": "Web Search",
        "properties": {
          "query": { "type": "string", "description": "Término de búsqueda" },
          "count": { "type": "integer", "description": "Número de resultados" }
        },
        "required": ["query"]
      }
    }
  ],
  "total": 47,
  "page": 1,
  "per_page": 20
}
```

**Ejemplo de uso:**

```pascal
var
  LResult: TJSONObject;
  LTools: TJSONArray;
begin
  LResult := AiFunctions1.SearchPPMMCP('web search');
  if not Assigned(LResult) then Exit;
  try
    LTools := LResult.GetValue<TJSONArray>('tools');
    for var I := 0 to LTools.Count - 1 do
    begin
      var LTool := LTools.Items[I] as TJSONObject;
      ListBox1.Items.Add(Format('%s — %s',
        [LTool.GetValue<String>('name'),
         LTool.GetValue<String>('description')]));
    end;
  finally
    LResult.Free;
  end;
end;
```

---

### 3.2 ImportMCPFromPPM — Registrar una herramienta

```pascal
function ImportMCPFromPPM(
  const AName: String;
  const AVersion: String = '';
  const ARegistryUrl: String = 'https://registry.pascalai.org'
): TMCPClientItem;
```

Registra una herramienta MCP del registry como un nuevo `TMCPClientItem` en la colección `MCPClients` del componente. El item se crea con:

- `TransportType = tpHttp`
- `URL = ''` (vacía — requiere configuración)
- `Enabled = False` (deshabilitado hasta configurar el servidor)

Si ya existe un cliente con el mismo nombre, devuelve el existente sin duplicar.

**Parámetros:**

| Parámetro | Tipo | Descripción |
|-----------|------|-------------|
| `AName` | String | Nombre del paquete MCP en el registry |
| `AVersion` | String | Versión específica, o vacío para la última |
| `ARegistryUrl` | String | URL del registry (por defecto: oficial) |

**Flujo de uso típico:**

```pascal
var
  LItem: TMCPClientItem;
begin
  // 1. Importar del registry (crea stub deshabilitado)
  LItem := AiFunctions1.ImportMCPFromPPM('mcp-web-search');
  if not Assigned(LItem) then
  begin
    ShowMessage('Herramienta no encontrada en el registry.');
    Exit;
  end;

  // 2. Configurar la URL del servidor MCP real
  LItem.Params.Values['URL'] := 'http://localhost:3000/mcp';

  // 3. Habilitar y sincronizar
  LItem.Enabled := True;
  LItem.UpdateClientProperties;

  // 4. Inicializar la conexión
  if LItem.MCPClient <> nil then
    LItem.MCPClient.Initialize;
end;
```

---

## 4. Ejemplos completos

### Ejemplo 1 — Cargar un prompt de PPM y usarlo en un chat

```pascal
procedure TForm1.BtnCargarPromptClick(Sender: TObject);
var
  LItem: TAiPromptItem;
  LPrompt: String;
begin
  // Importar desde PPM
  LItem := AiPrompts1.LoadFromPPM('delphi-code-review');
  if not Assigned(LItem) then
  begin
    ShowMessage('No se encontró el prompt en PPM.');
    Exit;
  end;

  // Sustituir variables y enviar al LLM
  LPrompt := AiPrompts1.GetTemplate('delphi-code-review',
    ['language=Delphi', 'focus=memory management', 'code=' + Memo1.Text]);

  AiConnection1.NewChat;
  AiConnection1.Run(LPrompt);
end;
```

---

### Ejemplo 2 — Explorar el catálogo de prompts en una lista

```pascal
procedure TForm1.BtnBuscarClick(Sender: TObject);
var
  LResult: TJSONObject;
  LPackages: TJSONArray;
begin
  ListBox1.Clear;
  LResult := AiPrompts1.SearchPPM(EdtBusqueda.Text, 'prompt', 1, 50);
  if not Assigned(LResult) then
  begin
    ShowMessage('Error al conectar con registry.pascalai.org');
    Exit;
  end;
  try
    LPackages := LResult.GetValue<TJSONArray>('packages');
    for var I := 0 to LPackages.Count - 1 do
    begin
      var LPkg := LPackages.Items[I] as TJSONObject;
      // Guardar el nombre del paquete en el Data del item
      ListBox1.Items.AddObject(
        Format('[v%s] %s — %s', [
          LPkg.GetValue<String>('version'),
          LPkg.GetValue<String>('name'),
          LPkg.GetValue<String>('description')
        ]),
        TObject(LPackages.Items[I])  // referencia temporal
      );
    end;
    LblTotal.Text := Format('%d prompts encontrados', [LResult.GetValue<Integer>('total')]);
  finally
    LResult.Free;
  end;
end;

// Al hacer doble clic en la lista, importar el prompt seleccionado
procedure TForm1.ListBox1DblClick(Sender: TObject);
var
  LNombre: String;
  LItem: TAiPromptItem;
begin
  if ListBox1.ItemIndex < 0 then Exit;
  // Extraer nombre del texto "[v1.0.0] nombre-prompt — descripción"
  LNombre := ListBox1.Items[ListBox1.ItemIndex];
  LNombre := Copy(LNombre, Pos('] ', LNombre) + 2, MaxInt);
  LNombre := Copy(LNombre, 1, Pos(' — ', LNombre) - 1);

  LItem := AiPrompts1.LoadFromPPM(LNombre);
  if Assigned(LItem) then
    ShowMessage('Prompt "' + LItem.Nombre + '" importado correctamente.')
  else
    ShowMessage('Error al importar el prompt.');
end;
```

---

### Ejemplo 3 — Descubrir e integrar una herramienta MCP

```pascal
procedure TForm1.BtnIntegrarMCPClick(Sender: TObject);
var
  LResult: TJSONObject;
  LTools: TJSONArray;
  LNombre: String;
  LItem: TMCPClientItem;
begin
  // Buscar calculadoras disponibles
  LResult := AiFunctions1.SearchPPMMCP('calculator');
  if not Assigned(LResult) then Exit;
  try
    LTools := LResult.GetValue<TJSONArray>('tools');
    if LTools.Count = 0 then
    begin
      ShowMessage('No se encontraron herramientas MCP de tipo "calculator".');
      Exit;
    end;
    LNombre := (LTools.Items[0] as TJSONObject).GetValue<String>('name');
  finally
    LResult.Free;
  end;

  // Importar la primera encontrada
  LItem := AiFunctions1.ImportMCPFromPPM(LNombre);
  if not Assigned(LItem) then Exit;

  // Completar configuración con la URL del servidor local
  LItem.Params.Values['URL'] := 'http://localhost:4000/mcp';
  LItem.Enabled := True;
  LItem.UpdateClientProperties;

  if Assigned(LItem.MCPClient) then
    LItem.MCPClient.Initialize;

  ShowMessage(Format('Herramienta "%s" integrada y lista para usar.', [LNombre]));
end;
```

---

## 5. Referencia de la API REST de PPM

Los métodos de MakerAI llaman internamente a estos endpoints. Se documentan aquí para referencia o para implementar llamadas directas.

| Endpoint | Método | Descripción |
|----------|--------|-------------|
| `/v1/search?q=...&type=prompt` | GET | Buscar paquetes |
| `/v1/packages/:name` | GET | Info de un paquete (versiones, autor, etc.) |
| `/v1/packages/:name/:version/raw` | GET | Texto plano del prompt (sin sustitución) |
| `/v1/packages/:name/:version/render?var=val` | GET | Prompt con variables sustituidas (server-side) |
| `/v1/mcp/discover?q=...` | GET | Buscar herramientas MCP con schemas inline |
| `/v1/packages/:name/:version/schema` | GET | JSON Schema de una herramienta MCP |

**Notas:**
- Todos los endpoints de consulta son **públicos** (no requieren autenticación).
- La autenticación (`Authorization: Bearer ppm_...`) solo es necesaria para publicar paquetes.
- La versión `latest` no existe como path literal — `LoadFromPPM` y `ImportMCPFromPPM` la resuelven consultando el endpoint de info del paquete y seleccionando la primera versión no anulada (*yanked*).

---

## 6. Archivos fuente

| Archivo | Contenido relevante |
|---------|---------------------|
| `Source/Core/uMakerAi.Prompts.pas` | `TAiPrompts`, `TAiPromptItem`, métodos PPM para prompts |
| `Source/Tools/uMakerAi.Tools.Functions.pas` | `TAiFunctions`, `TMCPClientItems`, métodos PPM para MCP |

### Constantes y defaults

```pascal
// uMakerAi.Prompts.pas
const
  PPM_DEFAULT_REGISTRY = 'https://registry.pascalai.org';

// uMakerAi.Tools.Functions.pas
// La URL por defecto se pasa como valor por defecto del parámetro ARegistryUrl:
//   const ARegistryUrl: String = 'https://registry.pascalai.org'
```

### Dependencias añadidas

Los dos archivos usan:
- `System.Net.HttpClient` — cliente HTTP (`THTTPClient`, `IHTTPResponse`)
- `System.NetEncoding` — codificación de parámetros URL (`TNetEncoding.URL.Encode`)
- `System.RegularExpressions` — conversión de placeholders `{{var}}` → `<#var>` (solo en Prompts)

Estas unidades forman parte del RTL estándar de Delphi y están disponibles desde Delphi 10.4 Sydney.
