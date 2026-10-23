# CLAUDE.md

Demo 099, la cima de la serie: una clienta pide café hablando y la voz delega en un grafo federado. Es la base del episodio 18 de Café La Ladera.

## La cadena

```
voz (TAiOpenAiLiveChat, GPT-Live)
  -> DelegateChat: TAiChatConnection con la herramienta 'pedidos_ladera'
       -> TAiA2AAgentTool --A2A--> AGENTE DE PEDIDOS (TAiA2AServer :8302)
            Recibir -> [Inventario || Cotizar] -> Consolidar (jmAll)
                         |
                         +-- nodo federado: TAiA2ARemoteAgentTool --A2A--> AGENTE DE BODEGA (:8301)
            Consolidar -> 'faltantes > 0' ? Avisar : Confirmar -> Responder
  <- la respuesta vuelve por la misma cadena y la voz la dice
```

| Pieza | Qué hace |
|---|---|
| `TAiOpenAiLiveChat` + `DelegateChat` | La voz conversa y, para un pedido, delega en un chat normal. |
| `TAiA2AAgentTool` | El chat ve al agente de Pedidos como una herramienta. La descripción le pide el formato `cantidad producto, ... \| ciudad`. |
| `TAiA2AServer` (dos) | Publican los grafos de Pedidos y de Bodega como agentes A2A 1.0. |
| `TAiA2ARemoteAgentTool` | El nodo `Inventario` de Pedidos lo ejecuta otro agente: la federación. |
| Grafo de Pedidos | Fanout, `jmAll`, expresión con camino por defecto: lo mismo que el demo 098. |

En el demo todo corre en un proceso. En la vida real, Bodega y Pedidos estarían en otros servidores y solo cambiaría la URL.

## Uso

```bash
VozPedidoLadera.exe                      # micrófono y parlantes (usar audífonos); Enter para salir
VozPedidoLadera.exe --audio pedido.pcm   # voz desde un archivo PCM16 24 kHz mono (o .wav con cabecera de 44 bytes)
VozPedidoLadera.exe --texto "Quiero 2 bolsas de Huila Reserva para Pitalito"   # sin voz
```

El modo `--audio` alimenta `SendAudioChunk` a ritmo real, igual que el micrófono, y no reproduce audio. Con `--salida llamada.pcm` guarda las dos voces (clienta y Lucía) en un PCM16 24 kHz mono, alineadas con el reloj de la corrida. Cada voz se suma a la mezcla con su propia posición: la clienta suena seguida desde su primer trozo y Lucía, cuya voz llega más rápido que tiempo real, al llegar o al terminar lo anterior, como en un parlante (episodio 18 de la serie). Con una sola posición para las dos, los trozos de silencio que GPT-Live manda mientras escucha partían la voz de la clienta en pedazos de 100 ms. `--texto` llama directo al chat delegado, para ver la cadena A2A sin voz.

Requiere `OPENAI_API_KEY`. Los dos agentes A2A usan los puertos 8301 y 8302.

## Probado (oct 22/2026)

Con OpenAI (`gpt-live-1` para la voz y `gpt-6-sol` para el chat delegado):
- **`--texto`, pedido normal.** 2 Huila Reserva y 1 Nariño Altura para Pitalito: la Bodega respondió DISPONIBLE, el total fue 123.000 COP con entrega hoy y Lucía lo confirmó.
- **`--texto`, producto agotado.** Sierra Nevada para Medellín: la Bodega respondió AGOTADO y Lucía ofreció las alternativas.
- **`--audio` con los mismos dos pedidos.** Las voces se generaron con `gpt-4o-mini-tts`. GPT-Live transcribió y delegó; la cadena completa tardó cerca de 1 s desde la delegación hasta la respuesta del grafo. Lucía dijo "un segundito, reviso" y luego el resultado. Ninguna respuesta se cortó.

**Sin probar:** el modo micrófono con parlantes. Es el mismo camino que el demo 092, que sí se probó en vivo con audífonos.

## Notas

- En consola, la voz, la captura y el reproductor entregan sus eventos con `TThread.Queue`, así que el hilo principal bombea `CheckSynchronize` todo el tiempo. Por eso Enter se lee en otro hilo.
- `Consolidar` lee la respuesta de la Bodega en `FindNode('Inventario').Output`. El nodo federado no escribe en el blackboard del grafo local.
- Empresa, productos y precios ficticios.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
