# CLAUDE.md

Demo 103: el humano en el ciclo, con checkpoints. Un pedido grande no se despacha sin que lo apruebe el encargado; el grafo se detiene, guarda su estado en disco y el programa termina. Es la base del episodio 14 de la serie; el demo 081 muestra lo mismo dentro de un chat FMX y sin LLM.

## El grafo

```
Cotizar (código) -> Aprobar (TAiWaitApprovalTool) -> Decidir (código) -> Redactar (TLLMNode)
```

| Pieza | Qué hace |
|---|---|
| `TAiWaitApprovalTool` | En el nodo `Aprobar`: suspende el grafo y dispara `OnSuspend` con la pregunta (`SuspendReason`) y la cotización del blackboard (`ContextKey`). Al reanudar, la respuesta humana pasa como `Output` |
| `TAiFileCheckpointer` | Guarda el hilo suspendido en `checkpoints\` junto al `.exe`. Al terminar el grafo, el checkpoint se borra |
| `GetActiveThreads` + `ResumeThread` | La segunda corrida busca el hilo pendiente y lo reanuda con la respuesta del encargado |
| `TLLMNode` "Redactar" | Escribe el mensaje al cliente con la cotización y la respuesta del encargado |

## Uso

```bash
AprobacionLadera.exe                                    # cotiza y queda esperando aprobación
AprobacionLadera.exe --responder "Sí, aprobado. Que salga el lunes."   # el grafo sigue
```

Requiere `OPENAI_API_KEY` (el Redactor). El grafo se arma igual en las dos corridas: el checkpoint guarda el estado, no el código.

## Probado (oct 22/2026)

Con `gpt-6-sol`: 25 kg de Sierra Nevada dan 1.955.000 COP con el 15 %, más que el tope de 1.500.000; el grafo se suspendió y el programa terminó con el checkpoint en disco. La segunda corrida recuperó el hilo, el Redactor confirmó el pedido con el total y la salida del lunes, y no quedó ningún hilo pendiente.

## Notas

- Empresa, clientes y precios ficticios.
