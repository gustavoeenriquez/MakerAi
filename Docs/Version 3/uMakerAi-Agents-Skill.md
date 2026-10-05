# TAiSkill — Skills reutilizables para agentes LLM

## Propósito

`TAiSkill` es un contenedor de configuración reutilizable para nodos LLM (`TLLMNode`) en el sistema de agentes de MakerAI. Reúne en un único objeto la personalidad de un agente: sus instrucciones (system prompt), el driver y el modelo sugeridos, la API key y la lista de herramientas que debe tener disponibles.

La idea central es separar **qué hace** un nodo (su propósito, su personalidad) de **dónde vive** (en qué grafo, con qué modelo, con qué herramientas adicionales). Un skill se define una vez —en disco o en el registry PPM— y se reutiliza en varios nodos o proyectos.

> **v3.8:** `TAiSkill` entiende el formato **SKILL.md** (el de las Agent Skills y el que publica PPM) además de su JSON propio, `FromPPM` descarga del registry real y la combinación con el nodo se reescribió (ver [Reglas de combinación](#reglas-de-combinación)). Antes de v3.8 `FromPPM` no funcionaba y el `DriverName` del skill nunca se aplicaba.

---

## Archivos involucrados

| Archivo | Rol |
|---|---|
| `Source/Agents/uMakerAi.Agents.Skill.pas` | `TAiSkill` |
| `Source/Agents/uMakerAi.Agents.Node.LLM.pas` | `TLLMNode` con la propiedad `Skill`, `ResolveConfig` y `ConfigureChat` |
| `Source/Core/uMakerAi.Skills.Format.pas` | Parser de SKILL.md (`TAiSkillDoc`) y cliente del registry PPM (`TAiPPMClient`), compartido con `TAiPrompts` |

---

## Formatos

### 1. SKILL.md (recomendado)

Un archivo Markdown con un bloque de *frontmatter* YAML al inicio. Es el formato de las Agent Skills de Claude y el de los paquetes `skill` del registry PPM, así que un mismo archivo sirve en MakerAI, en Claude Code y en PPM.

```markdown
---
name: revisor-delphi
description: Revisa código Delphi en busca de errores y malas prácticas.
model: claude-sonnet-5
allowed-tools:
  - filesystem
  - git
driver: Claude          # extensión de MakerAI (opcional)
---

Eres un revisor de código Delphi experto. Analiza el código que te den e
identifica errores lógicos, fugas de memoria y violaciones de buenas prácticas.
Responde en español con una lista de hallazgos por severidad.
```

| Frontmatter | Propiedad | Notas |
|---|---|---|
| `name` | `Name` | Si falta, se usa el nombre de la carpeta (o del archivo) |
| `description` | `Description` | Para documentar; no se envía al LLM |
| `model` | `Model` | Ver [Reglas de combinación](#reglas-de-combinación) |
| `allowed-tools` | `ExtraTools` | Lista `- x` o en línea `a, b` / `[a, b]` |
| `driver` | `DriverName` | Extensión de MakerAI; las Agent Skills no la tienen |
| *cuerpo* | `SystemPrompt` | Todo lo que va después del segundo `---` |

**Un SKILL.md nunca aporta `ApiKey`**, aunque traiga una clave `apikey:`. Es contenido que puede venir de terceros (el registry, una carpeta compartida): si pudiera fijar la clave, un skill malicioso pediría `@CUALQUIER_VARIABLE` y el nodo enviaría ese secreto al proveedor.

Los nombres de `allowed-tools` que no existen en el `TAiToolRegistry` se ignoran al cargar las herramientas. Es lo normal con skills escritos para Claude Code, que declaran `Read`, `Grep` o `Bash`.

### 2. JSON propio (archivos del desarrollador)

El formato original de `TAiSkill`. Se mantiene para archivos locales y es el **único** que acepta `apiKey`, porque es un archivo del propio desarrollador.

```json
{
  "name":         "sql-expert",
  "description":  "Experto en SQL",
  "driverName":   "OpenAI",
  "model":        "gpt-5.6",
  "apiKey":       "@OPENAI_API_KEY",
  "systemPrompt": "Eres un experto en SQL...",
  "extraTools":   ["database"]
}
```

---

## Carga de skills

| Método | Formato | Origen |
|---|---|---|
| `TAiSkill.FromPPM('skill-code-review')` | SKILL.md | Registry PPM |
| `TAiSkill.FromFolder('Skills\revisor')` | SKILL.md | Carpeta con `SKILL.md` |
| `TAiSkill.FromSkillFile('Skills\revisor\SKILL.md')` | SKILL.md | Archivo |
| `TAiSkill.FromFile(ruta)` | según la ruta | Carpeta o `.md` → SKILL.md; otro archivo → JSON |
| `TAiSkill.FromJSON(texto)` | JSON | Texto en el código |
| `Skill.LoadFromSkillText(texto)` | SKILL.md | Texto en el código |

Todas lanzan `EAiSkillError` si fallan, con un mensaje que dice qué pasó (archivo inexistente, paquete que no existe, paquete que no es un skill, etc.).

### Desde el registry PPM

```pascal
Node.Skill := TAiSkill.FromPPM('skill-code-review');
Node.Skill := TAiSkill.FromPPM('code-review');   // también: se prueba 'skill-code-review'
```

- Registry por defecto: `https://registry.cimamaker.com`. Para otro, el segundo parámetro: `FromPPM('mi-skill', 'https://mi-registry.local')`.
- Sin versión se usa la **mayor versión no retirada** (por semver). Para fijar una: `FromPPM('skill-code-review', '', '1.0.0')`.
- `Skill.Version` y `Skill.SourcePath` dicen qué se descargó y de dónde.
- Para ver los skills disponibles: `ppm search --type skill`, o `TAiPrompts.SearchPPM('', 'skill')`.

> Ojo con los nombres: en PPM hay paquetes de tipo `prompt` y de tipo `skill`. `code-reviewer` es un **prompt**, así que `FromPPM('code-reviewer')` falla con *"es de tipo prompt, no un skill"*; el skill equivalente es `skill-code-review`.

`FromPPM` hace una petición HTTP síncrona. Para no bloquear la interfaz, cargar el skill en un hilo y asignarlo después:

```pascal
TTask.Run(procedure
begin
  var Skill := TAiSkill.FromPPM('skill-code-review');
  TThread.Queue(nil, procedure
  begin
    Node.Skill := Skill;
  end);
end);
```

### Desde una carpeta de skills

```text
MiApp/
  Skills/
    revisor-delphi/
      SKILL.md
    sql-expert/
      SKILL.md
    traductor.json        ← JSON propio, también válido
```

```pascal
NodeA.Skill := TAiSkill.FromFolder('Skills\revisor-delphi');
NodeB.Skill := TAiSkill.FromFile('Skills\traductor.json');
```

---

## Integración con TLLMNode

`TLLMNode` expone `Skill: TAiSkill` en su sección `public` (no se guarda en el DFM).

**Gestión de memoria:** el nodo toma ownership del skill. Al asignar uno nuevo, el anterior se libera; el activo se libera en el destructor del nodo. No compartir la misma instancia entre dos nodos (sería un double-free): cargar una por nodo.

```pascal
Node.Skill := TAiSkill.FromFolder('Skills\revisor');   // el nodo es el dueño
Node.Skill := TAiSkill.FromPPM('skill-code-review');    // libera el anterior
Node.Skill := nil;                                      // sin skill
// Skill.Free;  ← NUNCA con un skill asignado a un nodo
```

---

## Reglas de combinación

`TLLMNode.ResolveConfig` combina las propiedades del nodo con las del skill. `DoExecute` la usa (a través de `ConfigureChat`) para configurar el chat de cada ejecución, y se puede llamar directamente para ver qué configuración va a salir.

| Campo | Regla |
|---|---|
| `DriverName` | El del nodo. Si está vacío, el del skill. Si tampoco hay, `'Claude'`. |
| `Model` | El del nodo. Si está vacío, el del skill **solo si es del driver efectivo**: mismo `DriverName`, o ninguno de los dos fijó driver. Si no, el default del driver. |
| `ApiKey` | La del nodo. Si está vacía, la del skill (solo JSON). |
| `SystemPrompt` | **Se concatenan**: primero el del skill (instrucciones base), después el del nodo (ajustes para ese nodo), separados por una línea en blanco. |
| `ServiceURL`, `MaxTokens` | Solo del nodo. |

> **Cambio de comportamiento en v3.8:** `TLLMNode.DriverName` es `''` por defecto (antes `'Claude'`). Sin skill no cambia nada, porque el resultado sigue siendo `'Claude'`. Con skill, ahora sí se usa el driver del skill; antes el `'Claude'` fijo del constructor lo pisaba siempre. Los DFM existentes guardan `DriverName = 'Claude'` de forma explícita y se comportan como antes.

### Ejemplos

| Skill | Nodo | Resultado |
|---|---|---|
| `OpenAI` / `gpt-5.6` / `@K` | vacío | `OpenAI` / `gpt-5.6` / `@K` |
| `OpenAI` / `gpt-5.6` / `@K` | `DriverName := 'Claude'` | `Claude` / default de Claude / `@K` — el `gpt-5.6` no se hereda |
| `OpenAI` / `gpt-5.6` | `Model := 'gpt-5.4'` | `OpenAI` / `gpt-5.4` |
| SKILL.md sin driver, `model: claude-opus-4-6` | vacío | `Claude` / `claude-opus-4-6` |
| SKILL.md sin driver, `model: claude-opus-4-6` | `DriverName := 'OpenAI'` | `OpenAI` / default de OpenAI |
| `systemPrompt: "Eres un revisor..."` | `SystemPrompt := 'Revisa solo seguridad.'` | `"Eres un revisor...\n\nRevisa solo seguridad."` |

### Herramientas (ExtraTools)

| `UseAllTools` | `Skill.ExtraTools` | Resultado |
|---|---|---|
| `True` (default) | cualquiera | Se carga todo el `TAiToolRegistry` |
| `False` | vacío | No se cargan herramientas |
| `False` | `['filesystem','git']` | Solo `filesystem` y `git` (los que existan en el registry) |

---

## Ejemplos de uso

### Nodo con un skill del registry

```pascal
var
  Manager: TAIAgentManager;
  Node: TLLMNode;
begin
  Manager := TAIAgentManager.Create(nil);
  Node := TLLMNode.Create(Manager);

  Node.Skill := TAiSkill.FromPPM('skill-code-review');   // instrucciones + modelo
  Node.ApiKey := '@CLAUDE_API_KEY';                      // la clave la pone el nodo
  Node.SystemPrompt := 'Responde en español.';           // se agrega al del skill

  Manager.StartNode := Node;
  Manager.Run('Revisa este código: ' + MiCodigo);
end;
```

### Mismo skill, otro proveedor

```pascal
Node.Skill      := TAiSkill.FromFolder('Skills\revisor-delphi');
Node.DriverName := 'OpenAI';       // el modelo de Claude del skill no se hereda
Node.Model      := 'gpt-5.6';
Node.ApiKey     := '@OPENAI_API_KEY';
```

### Grafo con varias personalidades

```pascal
NodeA.Skill := TAiSkill.FromFolder('Skills\optimista');
NodeB.Skill := TAiSkill.FromFolder('Skills\critico');
// Mismo driver para los dos (configurado en cada nodo), distintas instrucciones
NodeA.DriverName := 'Claude';
NodeB.DriverName := 'Claude';
```

### Ver la configuración antes de ejecutar

```pascal
var Cfg := Node.ResolveConfig;
Writeln(Cfg.DriverName, ' / ', Cfg.Model);
```

---

## Relación con otros componentes

| Componente | Para qué |
|---|---|
| `TAiSkill` + `TLLMNode` | La personalidad **fija** de un nodo de agente |
| `TAiPrompts.LoadSkillFromPPM` | Traer el texto de un skill como plantilla, para usarlo a mano |
| Skills de la Agent Card A2A (`TAiA2AServer.Skills`) | Describir hacia afuera lo que sabe hacer un agente; no ejecutan nada |

---

## Thread safety

`TAiSkill` es un objeto de datos sin sincronización. Se configura **antes** de ejecutar el grafo y no se modifica durante la ejecución. `TLLMNode` lo lee sin modificarlo en cada `DoExecute`.

---

## Notas de implementación

- `TAiSkill` no hereda de `TPersistent` ni de `TComponent`: es un objeto de runtime puro, y por eso `TLLMNode.Skill` es `public`.
- El parser de SKILL.md vive en `uMakerAi.Skills.Format` y es el mismo que usan `TAiPrompts` y el resto del framework. Entiende del YAML lo que usan los skills reales: escalares (con comillas o con comentario `#`), listas y bloques `|` / `>`.
- `ExtraTools` usa `dupIgnore` y `CaseSensitive=False`.
- Las pruebas de regresión están en `Tests/RegressionSuite` (casos `skills.*`, sin red: el registry es un servidor falso).
