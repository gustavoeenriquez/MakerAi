# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 091 — **Skills**: instrucciones reutilizables en formato SKILL.md (el de las Agent Skills y del registry PPM) usadas de tres formas. Consola. **Requiere la API key del driver elegido** (`OPENAI_API_KEY` por defecto).

La carpeta `skills\` del demo trae tres skills:

| Skill | Qué tiene |
|-------|-----------|
| `facturas` | Reglas + un archivo de apoyo (`plantilla.md`) que el modelo lee con `read_skill_file` |
| `revisor-delphi` | Orden de revisión y formato de respuesta; declara `allowed-tools: Read` (se ignora: no existe en el registry) |
| `tono-soporte` | Tono para responder a clientes |

| Bloque | Qué muestra |
|--------|-------------|
| **A. `TAiSkills` (bajo demanda)** | El modelo ve solo el catálogo (una línea por skill) y carga el que necesita con `use_skill`. Cuatro peticiones: factura (carga `facturas` y encadena `read_skill_file` para la plantilla), revisión de código, respuesta a un cliente, y una cuenta que **no** carga ningún skill |
| **B. `TLLMNode` + `TAiSkill`** | El skill como personalidad fija de un nodo de agente. `ResolveConfig` muestra la combinación: el `SystemPrompt` del skill seguido del del nodo |
| **C. `TAiPrompts.ApplySkill`** | El skill copiado a mano al `SystemPrompt` de una `TAiChatConnection` |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `SkillsDemo.dproj`, build Win64.

```bash
SkillsDemo.exe                                              # OpenAI gpt-5.6-luna
SkillsDemo.exe --driver Claude --model claude-haiku-4-5-20251001
SkillsDemo.exe --driver Groq --model openai/gpt-oss-120b
SkillsDemo.exe --ppm        # agrega skill-code-review del registry PPM al catálogo
```

La key se toma de `<DRIVER>_API_KEY`. El `.exe` busca la carpeta `skills\` subiendo desde su propia carpeta (queda en `Win64\Release`). Exit code: 0 ok, 2 si hay una excepción.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiSkills` (`use_skill`, `read_skill_file`) | `Source/Tools/uMakerAi.Tools.Skills.pas` |
| `TAiSkill`, `TLLMNode.ResolveConfig` | `Source/Agents/uMakerAi.Agents.Skill.pas`, `Source/Agents/uMakerAi.Agents.Node.LLM.pas` |
| `TAiPrompts.LoadSkillsFromFolder` / `ApplySkill` | `Source/Core/uMakerAi.Prompts.pas` |
| Parser de SKILL.md y cliente PPM | `Source/Core/uMakerAi.Skills.Format.pas` |

## Notas

- **Verificado (sep 29/2026)** con `gpt-5.6-luna` y `claude-haiku-4-5`: cada petición cargó el skill que correspondía y la cuenta no cargó ninguno; la factura salió con la plantilla exacta (prueba de que leyó `plantilla.md`), y B y C respetaron las reglas del skill.
- La primera versión de la plantilla decía "fecha de hoy" y el modelo inventó una fecha: las instrucciones deben decir qué hacer cuando falta el dato.
- `--ppm` no se usa en las peticiones a propósito: `skill-code-review` está escrito para Claude Code ("usa Read/Grep") y compite con `revisor-delphi` por la misma petición. Sirve para ver cómo entra un skill de terceros al catálogo.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
