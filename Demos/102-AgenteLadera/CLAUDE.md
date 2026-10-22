# CLAUDE.md

Demo 102: el primer agente. Es la base del episodio 12 de la serie. Un `TLLMNode` atiende el reclamo de una clienta con un skill y dos herramientas, y un segundo `TLLMNode` revisa la respuesta.

## Qué muestra

| Pieza | Qué hace |
|---|---|
| `TLLMNode` "Atencion" | Skill `respuesta-reclamo` del demo 101 (`TAiSkill.FromFolder`; el nodo lo libera) + `SystemPrompt` propio. Consulta el pedido, programa el cambio y redacta la respuesta |
| `TAiToolRegistry` | Propio del demo (`Registry`), llenado con `RegisterFromTAiFunctions`: las mismas funciones de un chat (`consultar_pedido`, `programar_cambio`) |
| `TLLMNode` "Revisor" | Sin herramientas (`UseAllTools := False`). Revisa las reglas y responde APROBADA o CORREGIDA |
| `TAIAgentManager` | API fluida: `AddNode(nodo)`, `AddEdge`, `SetEntryPoint`, `SetFinishPoint`. La salida de un nodo es la entrada del siguiente. `OnEnterNode` y `OnExitNode` muestran el recorrido y la salida de cada nodo al terminar |

## Uso

```bash
AgenteLadera.exe    # requiere OPENAI_API_KEY
```

El `.exe` busca `101-SkillsLadera\skills\respuesta-reclamo` subiendo desde su carpeta.

## Probado (oct 22/2026)

Con `gpt-6-sol`: Atencion llamó `consultar_pedido` y `programar_cambio` y respondió con la guía LAD-55821 y la entrega del viernes, antes del sábado que pedía la clienta. El revisor la aprobó.

## Notas

- El grafo corre asíncrono y el programa espera con `CheckSynchronize` (como el 098): `OnEnterNode`/`OnExitNode` se sincronizan con el hilo principal.
- Empresa, clientes, pedidos y precios ficticios.
