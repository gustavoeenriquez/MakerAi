# Skills in MakerAI

*Versión en español: [uMakerAi-Skills.md](uMakerAi-Skills.md)*

A **skill** is a reusable set of instructions for a model: how to review code, how to write an invoice, which tone to use with a customer. You write it once and use it in many chats, agents or projects.

MakerAI uses the **SKILL.md** format — the same as Claude's Agent Skills and the `skill` packages of the PPM registry. The same file works in MakerAI, in Claude Code and in PPM.

---

## Which component should I use?

| I want... | Use | See |
|---|---|---|
| A chat with **many skills available**, where the model loads **only the one it needs** | `TAiSkills` | [TAiSkills](#taiskills-on-demand-skills) |
| An **agent node** with a fixed personality | `TAiSkill` + `TLLMNode.Skill` | [uMakerAi-Agents-Skill.md](uMakerAi-Agents-Skill.md) (Spanish) |
| To put a skill's instructions into the `SystemPrompt` **manually** | `TAiPrompts.ApplySkill` | [TAiPrompts](#taiprompts-the-skill-in-the-systemprompt) |

The difference between the first and the third is cost: with `ApplySkill` the instructions are sent **on every turn**; with `TAiSkills` only a one-line-per-skill catalog is sent, and the instructions only when the model asks for them.

> The "skills" of an A2A Agent Card (`TAiA2AServer.Skills`) are a different thing: they describe what an agent can do to the outside world and do not execute anything.

Demo: [`Demos/091-Skills`](../../Demos/091-Skills) shows all three.

---

## The SKILL.md format

```markdown
---
name: invoices
description: Use it when the user asks to write an invoice or a billing note.
model: claude-sonnet-5
allowed-tools:
  - database
---

Write the invoice following the template in `template.md`
(read it with read_skill_file before writing).
- Compute subtotal, VAT (19 %) and total.
- If a required field is missing, ask for it instead of making it up.
```

| Field | Purpose |
|---|---|
| `name` | Identifier. If missing, the folder name is used |
| `description` | **When to use the skill.** This is what the model reads to decide, so write it for the model ("Use it when the user asks...") |
| `model` | Suggested model (used by `TLLMNode`; `TAiSkills` does not change the chat model) |
| `allowed-tools` | Tools the skill needs (used by `TLLMNode`; names that don't exist are ignored) |
| *body* | The instructions, in Markdown |

**Recommended layout:** one folder per skill, with its `SKILL.md` and supporting files.

```text
Skills/
  invoices/
    SKILL.md
    template.md
  delphi-reviewer/
    SKILL.md
```

**Writing tips**
- The `description` decides whether the model loads the skill. Make it concrete and phrased in terms of what the user asks.
- Say what to do when information is missing. In the demo tests, a template that said "today's date" made the model invent a date.
- If the skill has supporting files, say in the instructions when to read them.

---

## TAiSkills: on-demand skills

`TAiSkills` (`Source/Tools/uMakerAi.Tools.Skills.pas`) plugs into a `TAiFunctions` and registers two functions the model can call:

| Function | What it does |
|---|---|
| `use_skill(name)` | Returns the skill's instructions. Its description holds the **catalog** (one `- name: description` line per skill) and `name` is an `enum` of the skill names, so the model cannot invent one |
| `read_skill_file(name, path)` | Returns a supporting file of a folder-based skill. Only registered when at least one skill comes from a folder |

It works with any driver that supports function calling. Tested with OpenAI (`gpt-5.6-luna`), Claude (`claude-haiku-4-5`) and Groq (`gpt-oss-120b`).

### Usage

```pascal
uses uMakerAi.Tools.Functions, uMakerAi.Tools.Skills;

Skills := TAiSkills.Create(Self);
Skills.Functions := AiFunctions1;             // registers use_skill
Skills.LoadFromFolder('Skills');              // Skills\<name>\SKILL.md
AiConnection1.AiFunctions := AiFunctions1;
// Some drivers (Claude, Gemini) have function calling off by default:
AiConnection1.Params.Values['Tool_Active'] := 'True';

Resp := AiConnection1.AddMessageAndRun('Write the invoice for ACME for 3 hours...', 'user', []);
// The model calls use_skill('invoices'), then read_skill_file('invoices', 'template.md'),
// and answers following the instructions.
```

In the IDE: drop a `TAiSkills` on the form and set `Functions` and `Folder`, or write skills directly in the `Skills` collection (name, description and instructions). The functions are registered at run time.

### Loading skills

| Method | Source |
|---|---|
| `AddSkill(name, description, instructions)` | Code |
| `LoadFromFile(path)` | A `SKILL.md` or the folder containing it |
| `LoadFromFolder(dir)` | Every `dir\<name>\SKILL.md` (returns how many) |
| `LoadFromPPM('skill-code-review')` | PPM registry (`https://registry.cimamaker.com`) |
| `Folder` property | Loaded when the application starts |
| `Skills` collection | Edited in the IDE |

A skill whose name already exists replaces the previous one. `Enabled := False` on an item removes it from the catalog without deleting it.

### What the model receives

```text
<skill name="invoices" source="file">
Write the invoice following the template...

Supporting files (read them with read_skill_file when the instructions refer to them): template.md
</skill>
```

`source` is `inline`, `file` or `ppm`; PPM skills are also marked *Third-party skill from the PPM registry*. `Catalog`, `ExecuteUseSkill` and `ExecuteReadFile` are public and return exactly what the model sees, without calling it.

### Properties and events

| Member | Description |
|---|---|
| `Functions` | `TAiFunctions` where the functions are registered |
| `Skills` | Skill collection |
| `Folder` | Folder loaded at startup (run time only) |
| `UseFunctionName` / `ReadFunctionName` | Function names (`use_skill` / `read_skill_file`) |
| `AllowFileAccess` | Register `read_skill_file` (default `True`) |
| `MaxFileSize` | Maximum size of a supporting file (default 256 KB) |
| `OnBeforeUseSkill(Sender, Skill, var Allow)` | Veto a skill (per user, per session...) |
| `OnBeforeReadFile(Sender, Skill, Path, var Allow)` | Veto a file; `Path` is already validated |
| `OnSkillLoaded(Sender, Skill)` | The model loaded a skill (auditing, UI) |

Calls go through `TAiFunctions.DoCallFunction`, so an assigned `TAiGuardrails` also filters them, and every load is traced as a `skill.load` span.

---

## TAiPrompts: the skill in the SystemPrompt

For instructions you want **always** (not on demand):

```pascal
Prompts.LoadSkillsFromFolder('Skills');
Prompts.ApplySkill('support-tone', AiConnection1.SystemPrompt);          // replace
Prompts.ApplySkill('invoices', AiConnection1.SystemPrompt, True);        // append
```

`ApplySkill` takes a `TStrings`, so it works with `TAiChatConnection`, any `TAiChat` or a memo. Also available: `LoadSkillFromFile`, `LoadSkillFromPPM` and, on each item, `SkillDescription`, `SkillModel` and `SkillAllowedTools`.

---

## TAiSkill and TLLMNode: the skill as the base of an agent

```pascal
Node.Skill := TAiSkill.FromFolder('Skills\delphi-reviewer');   // or FromPPM / FromJSON
Node.SystemPrompt := 'Answer in English.';                     // appended to the skill's
```

The node combines its properties with the skill's: driver, model and key come from the node if set, otherwise from the skill. The skill's `SystemPrompt` comes first, followed by the node's. Full details in [uMakerAi-Agents-Skill.md](uMakerAi-Agents-Skill.md) (Spanish).

---

## Skills from the PPM registry

```pascal
Skills.LoadFromPPM('skill-code-review');   // TAiSkills
TAiSkill.FromPPM('skill-code-review');     // agent node
Prompts.LoadSkillFromPPM('skill-code-review');
```

- Without a version, the highest non-yanked version is used; `LoadFromPPM(name, registry, '1.0.0')` pins one.
- `'code-review'` also works: if it doesn't exist, `'skill-code-review'` is tried.
- To see what's available: `ppm search --type skill` or `TAiPrompts.SearchPPM('', 'skill')`.
- PPM has both `prompt` and `skill` packages: `code-reviewer` is a prompt, not a skill, and loading it as a skill fails with a message saying so.
- Many PPM skills are written for Claude Code and mention tools such as `Read` or `Bash`. Those tools don't exist in MakerAI unless you register them, so review a skill before using it.

---

## Security

A skill is text the model will **obey**. A third-party skill (from the registry, from a shared folder) is therefore a *prompt injection* vector:

- Only load skills from trusted sources, and review them first.
- `OnBeforeUseSkill` lets you restrict which skills each user or session can use.
- A SKILL.md **never provides credentials**: an `apikey:` field is ignored.
- `read_skill_file` is confined to the skill's folder. It rejects absolute paths, `..`, symbolic links, binary files and files larger than `MaxFileSize`, and it **never executes anything**.

---

## Troubleshooting

| Symptom | Cause |
|---|---|
| The model never calls `use_skill` | The `description` doesn't say clearly when to use it; or `Tool_Active` is `False` on the connection; or `AiFunctions` is not assigned |
| `use_skill` is not among the functions | No enabled skills (the function is switched off when there are none), or you're looking at design time (it's registered at run time) |
| `read_skill_file` is not there | No skill comes from a folder (skills from `AddSkill` or PPM have no supporting files), or `AllowFileAccess = False` |
| `EAiSkillError: ... es de tipo "prompt", no un skill` | The PPM package is a prompt: use `TAiPrompts.LoadFromPPM` or look for the equivalent skill |
| `EAiSkillError: La URL devolvió una página HTML...` | The configured registry is not PPM's (`https://registry.cimamaker.com`) |

> Error messages from the framework are in Spanish.
