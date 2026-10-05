# Skills en MakerAI

*English version: [uMakerAi-Skills.EN.md](uMakerAi-Skills.EN.md)*

Un **skill** es un conjunto de instrucciones reutilizables para un modelo: cómo revisar código, cómo redactar una factura, qué tono usar con un cliente. Se escribe una vez y se usa en muchos chats, agentes o proyectos.

MakerAI usa el formato **SKILL.md**, el mismo de las Agent Skills de Claude y de los paquetes `skill` del registry PPM. Un mismo archivo sirve en MakerAI, en Claude Code y en PPM.

---

## ¿Qué componente uso?

| Quiero... | Usar | Ver |
|---|---|---|
| Que un chat tenga **muchos skills disponibles** y el modelo cargue **solo el que necesita** | `TAiSkills` | [TAiSkills](#taiskills-skills-bajo-demanda) |
| Que un **nodo de agente** tenga una personalidad fija | `TAiSkill` + `TLLMNode.Skill` | [uMakerAi-Agents-Skill.md](uMakerAi-Agents-Skill.md) |
| Poner las instrucciones de un skill en el `SystemPrompt` **a mano** | `TAiPrompts.ApplySkill` | [TAiPrompts](#taiprompts-el-skill-en-el-systemprompt) |

La diferencia entre el primero y el tercero es el costo: con `ApplySkill` las instrucciones viajan **en cada turno**; con `TAiSkills` viaja solo un catálogo de una línea por skill, y las instrucciones únicamente cuando el modelo las pide.

> Las "skills" de la Agent Card de A2A (`TAiA2AServer.Skills`) son otra cosa: describen hacia afuera lo que sabe hacer un agente y no ejecutan nada.

Demo: [`Demos/091-Skills`](../../Demos/091-Skills) usa las tres formas.

---

## El formato SKILL.md

```markdown
---
name: facturas
description: Úsalo cuando el usuario pida redactar una factura o una cuenta de cobro.
model: claude-sonnet-5
allowed-tools:
  - database
---

Redacta la factura siguiendo la plantilla del archivo `plantilla.md`
(léelo con read_skill_file antes de escribir).
- Calcula subtotal, IVA (19 %) y total.
- Si falta un dato obligatorio, pregúntalo en vez de inventarlo.
```

| Campo | Para qué |
|---|---|
| `name` | Identificador. Si falta, se usa el nombre de la carpeta |
| `description` | **Cuándo usar el skill.** Es lo que el modelo lee para decidir: escribirla pensando en él ("Úsalo cuando el usuario pida...") |
| `model` | Modelo sugerido (lo usa `TLLMNode`; `TAiSkills` no cambia el modelo del chat) |
| `allowed-tools` | Herramientas que el skill necesita (lo usa `TLLMNode`; los nombres que no existen se ignoran) |
| *cuerpo* | Las instrucciones, en Markdown |

**Organización recomendada:** una carpeta por skill, con el `SKILL.md` y sus archivos de apoyo.

```text
Skills/
  facturas/
    SKILL.md
    plantilla.md
  revisor-delphi/
    SKILL.md
```

**Consejos para escribir un skill**
- La `description` decide si el modelo lo carga. Concreta y en términos de lo que pide el usuario.
- Decir qué hacer cuando falta información. En las pruebas del demo, una plantilla que decía "fecha de hoy" hizo que el modelo inventara una fecha.
- Si el skill tiene archivos de apoyo, decir en las instrucciones cuándo leerlos.

---

## TAiSkills: skills bajo demanda

`TAiSkills` (`Source/Tools/uMakerAi.Tools.Skills.pas`) se conecta a un `TAiFunctions` y registra dos funciones que el modelo puede llamar:

| Función | Qué hace |
|---|---|
| `use_skill(name)` | Devuelve las instrucciones del skill. Su descripción contiene el **catálogo** (una línea `- nombre: descripción` por skill) y `name` es un `enum` con los nombres, así que el modelo no puede inventar uno |
| `read_skill_file(name, path)` | Devuelve un archivo de apoyo de un skill de carpeta. Solo se registra si algún skill viene de una carpeta |

Funciona con cualquier driver que soporte function calling. Probado con OpenAI (`gpt-5.6-luna`), Claude (`claude-haiku-4-5`) y Groq (`gpt-oss-120b`).

### Uso

```pascal
uses uMakerAi.Tools.Functions, uMakerAi.Tools.Skills;

Skills := TAiSkills.Create(Self);
Skills.Functions := AiFunctions1;             // registra use_skill
Skills.LoadFromFolder('Skills');              // Skills\<nombre>\SKILL.md
AiConnection1.AiFunctions := AiFunctions1;
// Algunos drivers (Claude, Gemini) traen el function calling apagado:
AiConnection1.Params.Values['Tool_Active'] := 'True';

Resp := AiConnection1.AddMessageAndRun('Hazme la factura para ACME por 3 horas...', 'user', []);
// El modelo llama use_skill('facturas'), luego read_skill_file('facturas', 'plantilla.md'),
// y responde siguiendo las instrucciones.
```

En el IDE: poner un `TAiSkills` en el formulario, asignar `Functions` y `Folder`, o escribir los skills en la colección `Skills` (nombre, descripción e instrucciones). Las funciones se registran en tiempo de ejecución.

### Cargar skills

| Método | Origen |
|---|---|
| `AddSkill(nombre, descripción, instrucciones)` | Código |
| `LoadFromFile(ruta)` | Un `SKILL.md` o la carpeta que lo contiene |
| `LoadFromFolder(dir)` | Todos los `dir\<nombre>\SKILL.md` (devuelve cuántos) |
| `LoadFromPPM('skill-code-review')` | Registry PPM (`https://registry.cimamaker.com`) |
| Propiedad `Folder` | Se carga al iniciar la aplicación |
| Colección `Skills` | Editada en el IDE |

Un skill con un nombre que ya existe reemplaza al anterior. `Enabled := False` en un item lo saca del catálogo sin borrarlo.

### Qué recibe el modelo

```text
<skill name="facturas" source="file">
Redacta la factura siguiendo la plantilla...

Supporting files (read them with read_skill_file when the instructions refer to them): plantilla.md
</skill>
```

`source` es `inline`, `file` o `ppm`; los de PPM llevan además la marca *Third-party skill from the PPM registry*. `Catalog`, `ExecuteUseSkill` y `ExecuteReadFile` son públicos y devuelven exactamente lo que ve el modelo, sin pasar por él.

### Propiedades y eventos

| Miembro | Descripción |
|---|---|
| `Functions` | `TAiFunctions` donde se registran las funciones |
| `Skills` | Colección de skills |
| `Folder` | Carpeta que se carga al iniciar (solo en runtime) |
| `UseFunctionName` / `ReadFunctionName` | Nombres de las funciones (`use_skill` / `read_skill_file`) |
| `AllowFileAccess` | Registrar `read_skill_file` (default `True`) |
| `MaxFileSize` | Tamaño máximo de un archivo de apoyo (default 256 KB) |
| `OnBeforeUseSkill(Sender, Skill, var Allow)` | Vetar un skill (por usuario, por sesión...) |
| `OnBeforeReadFile(Sender, Skill, Path, var Allow)` | Vetar un archivo; `Path` ya está validado |
| `OnSkillLoaded(Sender, Skill)` | El modelo cargó un skill (auditoría, UI) |

Las llamadas pasan por `TAiFunctions.DoCallFunction`, así que un `TAiGuardrails` asignado también las filtra, y cada carga queda en la telemetría como span `skill.load`.

---

## TAiPrompts: el skill en el SystemPrompt

Para una instrucción que se quiere **siempre** (no bajo demanda):

```pascal
Prompts.LoadSkillsFromFolder('Skills');
Prompts.ApplySkill('tono-soporte', AiConnection1.SystemPrompt);          // reemplaza
Prompts.ApplySkill('facturas', AiConnection1.SystemPrompt, True);        // agrega al final
```

`ApplySkill` recibe un `TStrings`, así que sirve para `TAiChatConnection`, cualquier `TAiChat` o un memo. También: `LoadSkillFromFile`, `LoadSkillFromPPM` y, en cada item, `SkillDescription`, `SkillModel` y `SkillAllowedTools`.

---

## TAiSkill y TLLMNode: el skill como base de un agente

```pascal
Node.Skill := TAiSkill.FromFolder('Skills\revisor-delphi');   // o FromPPM / FromJSON
Node.SystemPrompt := 'Responde en español.';                 // se agrega al del skill
```

El nodo combina sus propiedades con las del skill: driver, modelo y clave del nodo si los tiene y, si no, los del skill. El `SystemPrompt` del skill va seguido del del nodo. Detalle completo en [uMakerAi-Agents-Skill.md](uMakerAi-Agents-Skill.md).

---

## Skills del registry PPM

```pascal
Skills.LoadFromPPM('skill-code-review');   // TAiSkills
TAiSkill.FromPPM('skill-code-review');     // nodo de agente
Prompts.LoadSkillFromPPM('skill-code-review');
```

- Sin versión se usa la mayor versión no retirada; `LoadFromPPM(nombre, registry, '1.0.0')` fija una.
- `'code-review'` también funciona: si no existe, se prueba `'skill-code-review'`.
- Para ver qué hay: `ppm search --type skill` o `TAiPrompts.SearchPPM('', 'skill')`.
- En PPM hay paquetes `prompt` y `skill`: `code-reviewer` es un prompt, no un skill, y cargarlo como skill falla con un mensaje que lo dice.
- Muchos skills de PPM están escritos para Claude Code y mencionan herramientas como `Read` o `Bash`. En MakerAI esas herramientas no existen salvo que se registren: conviene revisar un skill antes de usarlo.

---

## Seguridad

Un skill es texto que el modelo va a **obedecer**. Un skill de terceros (del registry, de una carpeta compartida) es por eso una vía de *prompt injection*:

- Cargar skills solo de fuentes confiables y revisarlos antes.
- `OnBeforeUseSkill` permite limitar qué skills puede usar cada usuario o sesión.
- Un SKILL.md **nunca aporta credenciales**: aunque traiga `apikey:`, se ignora.
- `read_skill_file` está confinado a la carpeta del skill. Rechaza rutas absolutas, `..`, enlaces simbólicos, binarios y archivos más grandes que `MaxFileSize`, y **nunca ejecuta nada**.

---

## Problemas frecuentes

| Síntoma | Causa |
|---|---|
| El modelo no llama `use_skill` | La `description` no describe bien cuándo usarlo; o `Tool_Active` está en `False` en la conexión; o `AiFunctions` no está asignado |
| `use_skill` no aparece entre las funciones | No hay skills habilitados (la función se apaga sin skills), o se está mirando en el IDE (se registra en runtime) |
| `read_skill_file` no aparece | Ningún skill viene de una carpeta (los de `AddSkill` y PPM no tienen archivos de apoyo), o `AllowFileAccess = False` |
| `EAiSkillError: ... es de tipo "prompt", no un skill` | El paquete de PPM es un prompt: usar `TAiPrompts.LoadFromPPM` o buscar el skill equivalente |
| `EAiSkillError: La URL devolvió una página HTML...` | El registry configurado no es el de PPM (`https://registry.cimamaker.com`) |
