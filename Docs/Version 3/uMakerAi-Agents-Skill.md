# TAiSkill — Skills reutilizables para agentes LLM

## Propósito

`TAiSkill` es un contenedor de configuración reutilizable para nodos LLM en el sistema de agentes de MakerAI. Encapsula en un único objeto la personalidad de un agente: su system prompt, el driver y modelo sugeridos, la API key, y la lista de herramientas que debe tener disponibles.

La idea central es separar **qué hace** un nodo (su propósito, su personalidad) de **dónde vive** (en qué grafo, con qué modelo económico, con qué herramientas adicionales). Un skill se define una vez —localmente o en el registry PPM— y se reutiliza en múltiples nodos o proyectos.

---

## Archivos involucrados

| Archivo | Rol |
|---|---|
| `Source/Agents/uMakerAi.Agents.Skill.pas` | Definición de `TAiSkill` |
| `Source/Agents/uMakerAi.Agents.Node.LLM.pas` | `TLLMNode` con propiedad `Skill` integrada |

---

## Clase TAiSkill

### Declaración

```pascal
TAiSkill = class
public
  constructor Create;
  destructor Destroy; override;

  procedure Clear;
  procedure LoadFromJSON(const AJsonStr: String);
  procedure LoadFromFile(const APath: String);
  procedure LoadFromPPM(const AName: String; const ABaseUrl: String = '');

  class function FromJSON(const AJsonStr: String): TAiSkill;
  class function FromFile(const APath: String): TAiSkill;
  class function FromPPM(const AName: String; const ABaseUrl: String = ''): TAiSkill;

  property Name        : String      read/write;
  property Description : String      read/write;
  property DriverName  : String      read/write;
  property Model       : String      read/write;
  property ApiKey      : String      read/write;
  property SystemPrompt: String      read/write;
  property ExtraTools  : TStringList read-only;
end;
```

### Propiedades

| Propiedad | Tipo | Descripción |
|---|---|---|
| `Name` | `String` | Identificador del skill. Coincide con el nombre en el registry PPM. |
| `Description` | `String` | Descripción legible del skill. No se envía al LLM. |
| `DriverName` | `String` | Driver LLM sugerido: `'OpenAI'`, `'Claude'`, `'Gemini'`, etc. Vacío = no especifica driver. |
| `Model` | `String` | Nombre del modelo sugerido. Vacío = usa el default del driver. |
| `ApiKey` | `String` | API key. Soporta la sintaxis `@ENV_VAR` (p.ej. `@CLAUDE_API_KEY`) para resolución en runtime. |
| `SystemPrompt` | `String` | Instrucción de sistema del skill. Es el principal aporte reutilizable. |
| `ExtraTools` | `TStringList` | Nombres de herramientas del `TAiToolRegistry` a activar cuando `UseAllTools=False`. |

---

## Formato JSON del skill

Un skill se describe con un objeto JSON con los siguientes campos, todos opcionales salvo `name`:

```json
{
  "name":         "code-reviewer",
  "description":  "Revisa código en busca de errores, mejoras y malas prácticas",
  "driverName":   "Claude",
  "model":        "claude-sonnet-4-6",
  "apiKey":       "@CLAUDE_API_KEY",
  "systemPrompt": "Eres un revisor de código experto. Analiza el código que te proporcionen e identifica: errores lógicos, problemas de rendimiento, violaciones de buenas prácticas y posibles bugs. Responde siempre en español con una lista estructurada de hallazgos.",
  "extraTools":   ["filesystem", "git"]
}
```

### Descripción de campos JSON

| Campo JSON | Propiedad Delphi | Obligatorio | Notas |
|---|---|---|---|
| `name` | `Name` | Sí | Identificador único del skill |
| `description` | `Description` | No | Solo para documentación |
| `driverName` | `DriverName` | No | Si omitido, el nodo usa su propio DriverName |
| `model` | `Model` | No | Si omitido, el nodo usa su propio Model o el default del driver |
| `apiKey` | `ApiKey` | No | Si omitido, el nodo usa su propio ApiKey |
| `systemPrompt` | `SystemPrompt` | No | Si omitido, el nodo usa su propio SystemPrompt |
| `extraTools` | `ExtraTools` | No | Array de strings con nombres de tools del TAiToolRegistry |

---

## Carga de skills

### Desde string JSON (inline)

```pascal
var Skill: TAiSkill;
Skill := TAiSkill.FromJSON('{"name":"translator","systemPrompt":"Traduce al inglés."}');
```

Útil para skills definidos directamente en código sin archivo externo.

### Desde archivo local

```pascal
var Skill: TAiSkill;
Skill := TAiSkill.FromFile('C:\MiApp\Skills\code-reviewer.json');
```

El archivo debe ser UTF-8. La ruta puede ser absoluta o resuelta por la aplicación.

### Desde el registry PPM

```pascal
var Skill: TAiSkill;
Skill := TAiSkill.FromPPM('code-reviewer');
```

Descarga el skill desde `https://ppm.pascalai.org/api/v1/packages/code-reviewer/skill.json`. Se puede sobreescribir la URL base:

```pascal
Skill := TAiSkill.FromPPM('code-reviewer', 'https://mi-registry.local/packages/');
```

### Carga condicional (sin excepción)

Las tres variantes de carga lanzan excepción si fallan. Para carga segura:

```pascal
var Skill: TAiSkill;
Skill := nil;
try
  Skill := TAiSkill.FromPPM('code-reviewer');
except
  on E: Exception do
    // skill no disponible, el nodo usará su configuración propia
end;
Node.Skill := Skill;  // nil es válido — el nodo funciona sin skill
```

---

## Integración con TLLMNode

### Propiedad Skill

`TLLMNode` expone la propiedad `Skill: TAiSkill` en su sección `public`:

```pascal
property Skill: TAiSkill read FSkill write SetSkill;
```

**Gestión de memoria:** `TLLMNode` toma ownership del skill. Al asignar un nuevo skill, el anterior se libera automáticamente. El skill activo se libera en el destructor del nodo.

```pascal
// Correcto: el nodo toma ownership
Node.Skill := TAiSkill.FromFile('skills/translator.json');

// Correcto: reemplaza el skill anterior (el anterior se libera)
Node.Skill := TAiSkill.FromPPM('code-reviewer');

// Correcto: elimina el skill (el nodo funciona solo con sus propias props)
Node.Skill := nil;

// INCORRECTO: no liberar manualmente un skill asignado al nodo
// Skill.Free;  ← provocaría double-free
```

---

## Reglas de merge (prioridad)

Cuando un nodo tiene un skill asignado, la configuración final se calcula en `DoExecute` siguiendo estas reglas:

### Regla general
**El skill es la BASE. Las propiedades del nodo son el OVERRIDE.**

Si una propiedad del nodo tiene valor no-vacío, sobreescribe la del skill. Si está vacía, se usa la del skill.

### Tabla de prioridades

| Campo | Skill (`TAiSkill`) | Nodo (`TLLMNode`) | Resultado |
|---|---|---|---|
| `DriverName` | `'Claude'` | `''` (vacío) | `'Claude'` (del skill) |
| `DriverName` | `'Claude'` | `'OpenAI'` | `'OpenAI'` (del nodo) |
| `Model` | `'claude-sonnet-4-6'` | `''` | `'claude-sonnet-4-6'` (del skill) |
| `Model` | `'claude-sonnet-4-6'` | `'claude-haiku-4-5'` | `'claude-haiku-4-5'` (del nodo) |
| `SystemPrompt` | `'Eres un revisor...'` | `''` | `'Eres un revisor...'` (del skill) |
| `SystemPrompt` | `'Eres un revisor...'` | `'Contexto extra...'` | `'Contexto extra...'` (del nodo) |
| `ApiKey` | `'@CLAUDE_API_KEY'` | `''` | `'@CLAUDE_API_KEY'` (del skill) |

### Regla para herramientas (ExtraTools)

Las herramientas son **aditivas**, no exclusivas. El comportamiento depende de `UseAllTools`:

| `UseAllTools` | `Skill.ExtraTools` | Resultado |
|---|---|---|
| `True` | cualquiera | Se carga todo el `TAiToolRegistry`. ExtraTools queda implícito. |
| `False` | vacío | No se cargan herramientas. |
| `False` | `['filesystem','git']` | Se cargan solo `filesystem` y `git` desde el registry. |

---

## Ejemplos de uso

### Nodo con skill completo desde PPM

```pascal
var
  Manager: TAIAgentManager;
  Node: TLLMNode;
begin
  Manager := TAIAgentManager.Create(nil);
  Node := TLLMNode.Create(Manager);

  // El skill define todo: driver, modelo, system prompt y tools
  Node.Skill := TAiSkill.FromPPM('code-reviewer');

  // El nodo puede especializar el skill sin perderlo:
  Node.Model := 'claude-haiku-4-5';  // modelo más barato
  Node.UseAllTools := False;          // solo usa los ExtraTools del skill

  Manager.StartNode := Node;
  Manager.Run('Revisa este código: ' + MiCodigo);
end;
```

### Nodo con skill parcial (solo system prompt)

```pascal
// El skill aporta el system prompt; driver y modelo se configuran en el nodo
Node.Skill := TAiSkill.FromJSON(
  '{"name":"sql-expert","systemPrompt":"Eres un experto en SQL..."}'
);
Node.DriverName := 'OpenAI';
Node.Model      := 'gpt-4.1';
Node.ApiKey     := '@OPENAI_API_KEY';
```

### Múltiples nodos, múltiples skills

```pascal
// Grafo de debate: dos nodos con personalidades distintas
NodeA.Skill := TAiSkill.FromFile('skills/optimista.json');
NodeB.Skill := TAiSkill.FromFile('skills/critico.json');

// Ambos usan el mismo driver (configurado en el nodo), distinto system prompt
NodeA.DriverName := 'Claude';
NodeA.Model      := 'claude-sonnet-4-6';
NodeB.DriverName := 'Claude';
NodeB.Model      := 'claude-sonnet-4-6';
```

### Skill con herramientas específicas

```pascal
// skill.json:
// { "name":"file-analyst", "systemPrompt":"Analiza archivos...",
//   "extraTools":["filesystem","search"] }

Node.Skill       := TAiSkill.FromFile('skills/file-analyst.json');
Node.UseAllTools := False;  // carga solo filesystem y search
```

---

## Organización recomendada de archivos de skills

```
MiApp/
  Skills/
    code-reviewer.json
    sql-expert.json
    translator.json
    file-analyst.json
```

Los archivos de skill son portables: el mismo `.json` funciona en cualquier proyecto MakerAI.

---

## Integración con PPM (Pascal Package Manager)

Los skills se pueden publicar y distribuir como paquetes de tipo `skill` en el registry PPM (`https://ppm.pascalai.org`).

### Estructura del paquete PPM tipo skill

Un paquete skill en PPM expone el archivo `skill.json` en la ruta:

```
https://ppm.pascalai.org/api/v1/packages/{nombre}/skill.json
```

### Publicar un skill en PPM

El proceso de publicación sigue el mismo flujo que los paquetes `prompt` y `mcp` del registry PPM. Consultar la documentación de PPM para el proceso completo.

### Descarga automática con `FromPPM`

`TAiSkill.FromPPM` hace una petición HTTP GET síncrona. Si se requiere descarga asíncrona (para no bloquear el hilo principal), cargar el skill en un hilo separado antes de asignarlo al nodo:

```pascal
TTask.Run(procedure
begin
  var Skill := TAiSkill.FromPPM('code-reviewer');
  TThread.Queue(nil, procedure
  begin
    Node.Skill := Skill;
  end);
end);
```

---

## Relación con otros componentes de MakerAI

### TAiSkill vs TAiPrompts

| Aspecto | `TAiSkill` | `TAiPrompts` |
|---|---|---|
| Propósito | Configuración completa del agente | Gestión de prompts con templates y variables |
| Contenido | Driver + Model + ApiKey + SystemPrompt + Tools | Prompts con placeholders `<#Variable>` |
| Uso típico | Definir la personalidad de un nodo LLM | Construir mensajes dinámicos con datos variables |
| Carga desde PPM | Sí (`skill.json`) | Sí (paquete tipo `prompt`) |
| Se asigna a | `TLLMNode.Skill` | Consulta directa: `Prompts.GetTemplate(...)` |

Los dos se complementan: el skill define el system prompt estático del agente; `TAiPrompts` construye los mensajes dinámicos que ese agente procesa.

### TAiSkill vs configuración directa en TLLMNode

Usar un skill es equivalente a configurar el nodo directamente, con la ventaja de la reutilización:

```pascal
// Sin skill (configuración directa):
Node.DriverName   := 'Claude';
Node.Model        := 'claude-sonnet-4-6';
Node.ApiKey       := '@CLAUDE_API_KEY';
Node.SystemPrompt := 'Eres un revisor de código experto...';

// Con skill (equivalente, pero reutilizable):
Node.Skill := TAiSkill.FromFile('skills/code-reviewer.json');
```

---

## Consideraciones de thread safety

`TAiSkill` es un objeto de datos simples sin sincronización. Se asume que:

- El skill se configura **antes** de que el grafo de agentes empiece a ejecutar.
- Durante la ejecución del grafo, el skill no se modifica.
- Si se requiere cambiar el skill en runtime entre ejecuciones del grafo, hacerlo fuera de cualquier ejecución activa.

`TLLMNode.DoExecute` lee las propiedades del skill de forma inmutable durante la ejecución del nodo. No hay acceso concurrente al mismo skill desde múltiples hilos a menos que se comparta el mismo skill entre múltiples nodos (lo cual es seguro en lectura).

---

## Notas de implementación

- `TAiSkill` no hereda de `TPersistent` ni de `TComponent`. No se puede serializar a DFM/FMX. Es un objeto de runtime puro.
- La propiedad `Skill` de `TLLMNode` es `public` (no `published`) por este motivo.
- `TAiSkill.FromPPM` usa `THTTPClient` (Delphi nativo), sin dependencia de librerías externas.
- Los campos `DriverName`, `Model`, `ApiKey` en el skill son **sugerencias**. El nodo siempre tiene la última palabra si sus propias propiedades están configuradas.
- `ExtraTools` usa `dupIgnore` y `CaseSensitive=False`, por lo que duplicados y variaciones de mayúsculas se normalizan automáticamente.
