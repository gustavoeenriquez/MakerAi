# CLAUDE.md

Demo 097: memoria persistente en el chat. Marta vuelve otro día, con un chat nuevo y sin historial, y Lucía la recuerda. Es la base del episodio 9 de la serie de Café La Ladera; el demo 076 muestra `TAiMemory` suelto y este lo conecta a un chat.

## Qué muestra

| Pieza | Qué hace |
|---|---|
| `Messages` | Historial de una conversación. Se pierde al cerrar el chat. |
| `TAiMemory` | Memorias en SQLite (`memoria_ladera.db`), con `Namespace` por cliente. Con `Embedder` la búsqueda es híbrida: palabras más significado. |
| `PersistentMemory` + `MemoryTokenBudget` | En cada turno el chat busca en la memoria lo relevante para la pregunta y lo inyecta antes del prompt. El bloque no se queda en el historial. |
| `Analyzer` | Un modelo barato (`gpt-6-luna`) lee la conversación y decide qué recordar. El demo llama `AnalyzeNow` al cerrar la sesión; con `AnalysisInterval = N` lo hace solo cada N turnos. |

El flujo:
1. **Sesión 1, tres turnos.** Marta cuenta que usa prensa francesa, que lo toma sin azúcar, que le gustan los frutales y que el Nariño le pareció muy ácido.
2. **Cierre.** El analizador guarda las memorias y el demo las lista.
3. **Sesión 2, chat nuevo con 0 mensajes.** El demo muestra primero el bloque que inyecta la memoria. Luego pregunta qué café le conviene y cómo prepararlo.

## Uso

```bash
ChatMemoryLadera.exe            # sesión 1 + sesión 2, con una base nueva
ChatMemoryLadera.exe --sesion2  # solo la sesión 2, con lo que ya quedó guardado
```

Requiere `OPENAI_API_KEY` (chat, analizador y embeddings).

## Probado (oct 22/2026)

Con `gpt-6-sol` para el chat y `gpt-6-luna` para el analizador:
- **Memorias guardadas.** Fueron 2 preferencias, con importancia 8 y 7: frutales, el Nariño demasiado ácido, prensa francesa y sin azúcar.
- **Sesión 2.** Recomendó Huila Reserva (frutal y menos ácido que el Nariño) con la receta para prensa francesa: 20 g por 300 ml, 94 °C, 4 minutos y sin azúcar.

La prueba encontró un bug del framework, ya corregido. `TAiMemory` nunca limpiaba el historial del `Analyzer`, así que cada análisis reenviaba los anteriores. Ahora hace `NewChat` antes de cada análisis (caso de la suite `memory.analyzer.stateless`).

## Notas

- El `Analyzer` tiene que ser síncrono. En asíncrono, `AddMessageAndRun` devuelve vacío y no se guarda nada.
- En consola hay que enlazar FireDAC a mano, igual que en el demo 076.
- Empresa, clientes, productos y precios ficticios.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
