# CLAUDE.md

Demo 104: guardrails y evals para Lucía. Es la base del episodio 15 de la serie; el demo 073 muestra las mismas piezas sin LLM.

## Qué muestra

| Bloque | Qué hace |
|---|---|
| **A. Guardrails** | `TAiGuardrails` en el `TAiFunctions` del chat. `BlockedTools` = `anular_*` (anular es cosa de personas) y `OnCheckToolCall` veta un descuento mayor al 15 % leyendo el JSON crudo de los argumentos. Si bloquea, la herramienta no se ejecuta y el modelo recibe el motivo. `OnBlocked` lo muestra en consola |
| **B. Evals** | `TAiEvalRunner` sobre las respuestas reales de Lucía: `ExpectContains`, `ExpectNotContains`, `ExpectMaxLength` y `ExpectJudge` con un `TAiOpenChat` (`gpt-6-luna`) como juez. Imprime el reporte y lo que respondió en los casos que fallan. Exit code 0 si todos pasan |

## Uso

```bash
GuardrailsLadera.exe                # guardrails + evals
GuardrailsLadera.exe --solo-evals   # solo los evals
```

Requiere `OPENAI_API_KEY`.

## Probado (oct 22/2026)

Con `gpt-6-sol`:
- **Guardrails.** El 10 % se aplicó; el 30 % y la anulación se bloquearon (2 llamadas) y Lucía explicó el límite.
- **Evals.** 4/4 en dos corridas seguidas.

La prueba encontró un bug del framework, ya corregido. El juez de `ExpectJudge` no recibía la pregunta del caso y rechazó "No, no tenemos café de Etiopía. Solo vendemos...". Además, su historial acumulaba los casos anteriores. Ahora recibe la pregunta y empieza cada juicio en un chat nuevo (caso de la suite `evals.judge.input-stateless`).

## Notas

- `aplicar_descuento` FIJA el descuento del pedido (reemplaza el anterior). Con la versión que lo sumaba al total rebajado, el modelo no llegaba a pedir el 30 %: explicaba que la herramienta no servía para eso, y el guardrail nunca se veía.
- `AArguments` del guardrail es el JSON crudo que mandó el modelo: se parsea, no se compara como texto.
- Empresa, clientes, pedidos y precios ficticios.
