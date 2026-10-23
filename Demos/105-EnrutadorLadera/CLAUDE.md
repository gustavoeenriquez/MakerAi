# CLAUDE.md

Demo 105: enrutar cada mensaje de un cliente al agente indicado con Jev (TypeSafe AI), antes de gastar un modelo de lenguaje. Es la base del episodio 16 de la serie; el demo 084 muestra lo mismo con dominios contables y legales, más los atajos `Choose`/`Noul`.

## El grafo

```
Recepcion (TAiJevRouterTool) --(lmConditional)--> Pedidos | Reclamos | Mayorista | Carta | Persona
                             --(NextNo)--------> Persona  (confianza < MinConfidence)
```

| Pieza | Qué hace |
|---|---|
| `TAiJevRouterTool` | Una llamada a Jev por mensaje: elige la ruta (`AddRoute`, una por agente, descrita como para una recepcionista) y responde el flag `urgente` (`AddFlag`). Deja `Recepcion.jev.choice/confidence/top/urgente` en el blackboard |
| `MinConfidence` | 0,6: por debajo, el link condicional cae a `NextNo` (Persona) |
| Ruta `Persona` | Para los temas que no son de ningún agente. Sin ella, Jev elige la ruta "menos mala" con confianza alta (la alianza para exportar iba a Mayorista con 0,97) |
| `Usage` | Peticiones, tokens y costo de Jev |

Los agentes son nodos simples para ver solo la decisión; en la tienda cada uno sería un `TLLMNode` como el del demo 102.

## Uso

```bash
EnrutadorLadera.exe    # requiere TYPESAFE_API_KEY (no usa OpenAI)
```

## Probado (oct 23/2026)

Los cinco mensajes fueron a su agente con confianza de 0,97 a 1,00, entre 150 y 280 ms cada uno. Jev marcó como urgente el pedido para el sábado (0,80). En total fueron 5 llamadas y 2.306 tokens, por US$ 0,000097.

## Notas

- Empresa y clientes ficticios.
