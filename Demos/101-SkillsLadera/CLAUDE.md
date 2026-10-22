# CLAUDE.md

Demo 101: skills de Café La Ladera en formato SKILL.md, bajo demanda con `TAiSkills`. Es la base del episodio 11 de la serie; el demo 091 muestra además `TLLMNode` + `TAiSkill` y `TAiPrompts.ApplySkill`.

## Los skills (`skills\`)

| Skill | Qué tiene |
|---|---|
| `cotizacion-mayorista` | Precios por kilo, descuentos (10 % desde 10 kg, 15 % desde 25 kg) y `plantilla.md`, que el modelo lee con `read_skill_file` |
| `respuesta-reclamo` | Cómo le contesta Lucía a un cliente molesto. Lo usa también el agente del demo 102 |
| `ficha-de-cata` | El formato de cinco líneas de la ficha de un café |

El modelo ve solo el catálogo (una línea por skill; el demo imprime su tamaño frente al de los skills completos) y carga el que necesita con `use_skill`. La cuarta petición (una traducción) no carga ninguno. El demo envuelve `read_skill_file` para mostrar cada lectura y la delega en `TAiSkills.ExecuteReadFile`.

## Uso

```bash
SkillsLadera.exe    # requiere OPENAI_API_KEY
```

## Probado (oct 22/2026)

Con `gpt-6-sol`: cada petición cargó el skill que correspondía. La cotización siguió la plantilla exacta (15 kg de Sierra Nevada: 1.242.000 COP con el 10 %), y la traducción no cargó ninguno.

## Notas

- La primera versión de `respuesta-reclamo` ofrecía siempre la política de devoluciones y pedía el número de pedido. En el agente del demo 102, que ya tenía el pedido y el cambio programado, la respuesta quedaba rara. Las reglas ahora dicen qué hacer en cada caso.
- Empresa, productos y precios ficticios.
