# CLAUDE.md

Demo 098: un pedido de Café La Ladera recorre un grafo de agentes armado con la API fluida de `TAIAgentManager`. Cubre fanout, unión, expresiones con camino por defecto, condicional, blackboard y un `TLLMNode` al final. Es la base del episodio 13 de la serie (orquestación).

## El grafo

```
Recibir --+--> Inventario --+
          +--> Cotizar -----+--> Consolidar (jmAll)
          +--> Envio -------+        |
                                     | AddExpressionEdge 'faltantes > 0' --> Avisar
                                     | AddFallbackEdge                  --> Despachar
                                                                              |
                                          AddConditionalEdge (next_route): local --> Mensajero
                                                                           nacional --> Transportadora
Avisar / Mensajero / Transportadora --> Redactor (TLLMNode, jmAny) = fin
```

| Pieza | Cómo se arma |
|---|---|
| Fanout | Tres `AddEdge` desde `Recibir`: un solo link con tres destinos que corren en paralelo |
| Join | `SetJoinMode('Consolidar', jmAll)`: espera las tres ramas |
| Blackboard | Las ramas dejan `faltantes`, `subtotal`, `envio`; `Consolidar` calcula `total` |
| Expresión | `AddExpressionEdge('Consolidar', 'Avisar', 'faltantes > 0')` se evalúa contra el blackboard; `AddFallbackEdge` (NextNo) es el camino si no se cumple |
| Condicional | `Despachar` escribe `next_route` (`ConditionalKey` por defecto) y `AddConditionalEdge` elige `Mensajero` o `Transportadora` |
| TLLMNode | `Grafo.AddNode(Redactor)` con un nodo ya creado. Redacta el mensaje de WhatsApp a partir del estado del pedido |
| Corrida limpia | `Grafo.Run(Texto, nil)` reinicia blackboard y estado en cada pedido |

Tres pedidos de ejemplo, uno por cada camino:
- **Pitalito:** local, va por mensajero.
- **Bogotá:** nacional, va por transportadora.
- **Pedido con Sierra Nevada:** está agotado, así que pasa a `Avisar`.

## Uso

```bash
AgentGraphLadera.exe            # Redactor = TLLMNode con OpenAI (@OPENAI_API_KEY)
AgentGraphLadera.exe --sin-llm  # Redactor en código; el grafo es el mismo
```

## Probado (oct 22/2026)

- **Sin LLM.** Los tres pedidos tomaron su camino: 123.000 COP por mensajero, 117.000 COP por transportadora y el pedido en espera por Sierra Nevada. En el tercero se ve que las ramas terminan en otro orden: corren en paralelo.
- **Con `gpt-6-sol` como Redactor.** Escribió tres mensajes de 2 a 3 líneas con los datos del pedido. Al pedido en espera le ofreció ajustar la compra.

## Notas

- Las expresiones admiten una comparación (`>`, `<`, `>=`, `<=`, `=`, `<>`) contra una clave del blackboard, sin `AND`. Para dos condiciones se encadenan nodos.
- Las ramas corren en hilos del pool, por eso la consola va con candado y `Esperar` bombea `CheckSynchronize`.
- Este grafo no se podía armar antes de la corrección de `AddEdge` de oct 22: cada `AddEdge` pisaba el link anterior.
- Empresa, productos y precios ficticios.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
