# Reporte a MKAIServer — cambios en MakerAi, 2026-09-16

Escrito desde el lado de MakerAi (`E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker`)
para el broker (`E:\Copilot\MKAIServer`). Es la contraparte del
`REPORTE_MKAISERVER_2026-08-29.md`.

Todo lo que sigue está **verificado contra los APIs reales** (sondas de
validación + llamadas de generación), no deducido de documentación.

Commits nuevos en `dev` y `master` (mismo punto, fast-forward):

```
fca6aba fix(deepseek): el nombre canonico es deepseek-flash y el gate de thinking no lo cubria
ca175d0 feat(openai): familia gpt-image-2.5 (flare/sunburst)
dc555a3 (tag: v3.7.0)
```

La versión **sigue en 3.7.0 / API_LEVEL 37**: no hubo bump, solo contenido
aditivo sobre el tag.

Orden por dinero en riesgo, no por tamaño.

---

## 1. URGENTE — `git pull` + recompilar, o DeepSeek factura razonamiento no pedido

`ResApiServer.dpr:87` enlaza `uMakerAi.Chat.DeepSeek.pas`, así que esto llega a
producción del broker en cuanto alguien use el nombre nuevo del modelo.

**Qué pasaba:** el bloque `thinking` se enviaba solo si el modelo empezaba por
`deepseek-v4`:

```pascal
if StartsText('deepseek-v4', LModel) then   // deepseek-flash NO entra
```

DeepSeek renombró el modelo (ver punto 2). Con el nombre canónico
`deepseek-flash` ese gate da False, el driver **no envía `thinking`**, y el API
aplica su default, que es **razonar**. Medido con llamada cruda sin el
parámetro: `completion_tokens_details.reasoning_tokens: 157`.

Consecuencia: un request con `ModelCaps=[]` (modo rápido/económico, que es lo
que el broker quiere para el tier barato) paga tokens de razonamiento.

**Ya está arreglado** en `fca6aba`: la decisión vive en
`ModelSupportsThinking()` y cubre `deepseek-v4*`, `deepseek-flash*` y los alias
en gracia (`deepseek-chat`, `deepseek-reasoner`, que también enrutan a flash).

**Acción del broker:** `git pull` en el checkout compartido y **recompilar el
servidor**. Sin recompilar, el arreglo no existe para MKAIServer.

Probado runtime 4/4 tras el fix:

| Caso | reasoning_content | thinking tokens |
|---|---|---|
| `deepseek-flash` sin `cap_Reasoning` | 0 chars | **0** |
| `deepseek-flash` con `cap_Reasoning` (tlLow) | 974 chars | 216 |
| `deepseek-v4-flash` (alias legacy) sin cap | 0 chars | 0 |
| `deepseek-v4-pro` con `cap_Reasoning` | 1.437 chars | 336 |

---

## 2. DeepSeek renombró el modelo barato: `deepseek-flash`

`GET /models` hoy devuelve **solo dos** modelos:

```json
["deepseek-flash", "deepseek-v4-pro"]
```

Docs oficiales, textual:

> *"Use `deepseek-flash` as the model name. The legacy names
> `deepseek-v4-flash` and `deepseek-v4-flash-vision-exp` are still accepted,
> but the corresponding models have been retired."*

Verificado: `deepseek-v4-flash`, `deepseek-chat` y `deepseek-reasoner` siguen
respondiendo, y en los tres casos el campo `model` de la respuesta devuelve
`deepseek-flash`.

**Lado MakerAi (ya hecho):** el default del driver y del registry es
`deepseek-flash`; `deepseek-v4-flash` queda registrado como alias legacy para
no romper DFMs existentes.

**Acción del broker:**
- Su mapa `mk-deepseek-v4-flash` → upstream debe apuntar a `deepseek-flash`.
- Exponer `mk-deepseek-flash` como nombre público nuevo y **conservar
  `mk-deepseek-v4-flash` como alias**, para no romper clientes desplegados.
- No usar `deepseek-chat` / `deepseek-reasoner` para rutas nuevas: están
  retirados desde el 24 jul 2026 y solo siguen por periodo de gracia.

---

## 3. Precios DeepSeek: subieron ~2x y ahora hay tarifa pico/valle

Esto es lo que más impacta la contabilidad del broker. Si la tabla de precios
está estática con los valores de agosto, **se está cobrando de menos**.

Por 1M de tokens, **hora pico**:

| Modelo | Input (miss) | Input (cache hit) | Output |
|---|---|---|---|
| `deepseek-flash` | $0.30 | $0.003 | $1.20 |
| `deepseek-v4-pro` | $1.32 | $0.022 | $3.96 |

- **Fuera de pico se paga exactamente la mitad.**
- Horas pico: **01:00–04:00 y 06:00–10:00 UTC, lunes a viernes.**
- Lo que teníamos anotado antes (`$0.14/$0.28` flash, `$0.435/$0.87` pro, y
  "pico 2x próximamente 9-12 y 14-18 UTC+8") está **obsoleto**: las horas son
  otras y la tarifa ya está vigente.

**Acción del broker:** decidir si el margen absorbe el valle o si se repercute
al cliente, y en cualquier caso mover las ventanas a UTC. Ojo con el uso de
hora local: la ventana es UTC y L-V, así que el fin de semana es todo valle.

**Lo que el correo de DeepSeek sí dice y no obliga a nada:** `deepseek-v4-pro`
**continúa** después del 14 sep 2026, con la misma facturación. Era un retiro
anunciado que se canceló; no hay que migrar nada.

---

## 4. OpenAI: familia `gpt-image-2.5` (flare / sunburst)

Dos modelos nuevos, endpoints `/v1/images/generations` y `/v1/images/edits`
(no están en `/responses`). IDs canónicos y snapshots, los cuatro verificados:

```
gpt-image-2.5-flare      gpt-image-2.5-flare-2026-09-08
gpt-image-2.5-sunburst   gpt-image-2.5-sunburst-2026-09-08
```

`flare` = rápido para generación cotidiana. `sunburst` = premium, control fino
en edición.

### Reglas de parámetros que el proxy debe respetar (todas verificadas)

| Parámetro | Regla |
|---|---|
| `quality` | 2.5 acepta `low/medium/high/xhigh/max/auto`. **`xhigh` y `max` solo en 2.5**; mandarlos a gpt-image-1/2 es riesgo de 400. Curiosidad: el mensaje de error del schema sigue listando solo `low/medium/high/auto`, pero `xhigh`/`max` pasan y la respuesta los devuelve en el eco |
| `background` | `transparent` funciona con **alpha real** (novedad frente a gpt-image-2, que no lo soportaba). Exige `output_format` `png` o `webp` — con `jpeg` hay que corregirlo, no reenviarlo |
| `input_fidelity` | **RECHAZADO**: *"The model 'gpt-image-2.5-sunburst' does not support the 'input_fidelity' parameter"*. Hay que hacer strip si el cliente lo manda |
| `response_format` | **Unknown parameter** en toda la familia gpt-image. Strip |
| `n` | El schema acepta hasta 10, pero en gpt-image-2 el límite real era 8. MakerAi clampa a 8 por precaución; recomendación: hacer lo mismo en el broker |
| `size` | Ancho y alto divisibles por 16, aspecto 1:3..3:1, máximo `3840x2160`, y hay un **mínimo de pixel budget**: `512x512` responde *"below the current minimum pixel budget"*. `1024x1024` en adelante funciona |
| `stream` + `partial_images` | Aceptados (gpt-image-2 no tenía streaming) |

### Contabilidad de imagen — el punto importante

El precio por token es **idéntico** al de gpt-image-2: $5/M texto in,
$1.25/M texto cacheado, $8/M imagen in, $2/M imagen cacheada, **$30/M imagen
out**.

Por eso **el coste por imagen NO es plano por modelo, depende del tier de
calidad**. Medido, mismo tamaño 1024x1024:

| Petición | output_tokens | Coste |
|---|---|---|
| `flare` + `quality=xhigh` | 3.122 | ~$0.094 |
| `sunburst` + `quality=max` | 7.024 | ~$0.211 |

**Acción del broker:** facturar desde `usage.output_tokens` de la respuesta, no
con una tarifa fija por imagen. Una tarifa plana por modelo se equivoca en más
del 100% entre `xhigh` y `max`.

### Cambio de forma en la respuesta (riesgo de facturar a cero)

En `/v1/images/generations` los campos `background`, `output_format`, `size`,
`quality` y **`usage`** vienen en la **RAÍZ** del JSON, no dentro de `data[]`.
Cada item de `data[]` trae solo `b64_json` y un nuevo **`generation_id`**; ya
no hay `revised_prompt`.

```json
{
  "created": 1789562281,
  "background": "transparent",
  "output_format": "png",
  "quality": "xhigh",
  "size": "1024x1024",
  "usage": { "input_tokens": 27, "output_tokens": 3122, "total_tokens": 3149,
             "output_tokens_details": { "image_tokens": 3122 } },
  "data": [ { "b64_json": "...", "generation_id": "67793cd9-..." } ]
}
```

Si el parser del broker busca `usage` dentro de `data[]`, **factura cero** —
es el mismo patrón de fuga del punto 1 del reporte de agosto. En MakerAi esto
se arregló en `ca175d0` con `TAiDalleImage.ParseRootMeta`, que completa los
campos vacíos desde la raíz (afectaba a toda la familia gpt-image, no solo a
2.5).

Al reenviar la respuesta, conviene propagar `generation_id`: es lo que permite
la edición multi-turno referenciando la imagen anterior.

---

## 5. Si el broker va a exponer `mk-gpt-image-2.5-*`

Hay que registrar los alias en MakerAi. Hoy **no están**: el bloque del driver
`MakerAi` en `Source/Chat/uMakerAi.Chat.Initializations.pas` (~línea 1897) solo
lista `mk-gpt-image-1`, `mk-gpt-image-1.5`, `mk-gpt-image-1-mini` y
`mk-gpt-image-2`.

Sin esa entrada, el cliente MakerAi calcula gap vacío, manda la petición por
`/responses` y falla con *"model was not found"*. El registro necesario es:

```pascal
for Model in ['mk-gpt-image-2.5-flare', 'mk-gpt-image-2.5-sunburst'] do
begin
  TAiChatFactory.Instance.RegisterUserParam('MakerAi', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('MakerAi', Model, 'SessionCaps', '[cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('MakerAi', Model, 'Tool_Active', 'False');
end;
```

**Acción:** decidir los nombres públicos definitivos y avisar a MakerAi para
añadirlos en el mismo commit que la ruta del servidor. No se registraron ahora
porque dependen de que el broker realmente los enrute.

---

## 6. Resumen de acciones para MKAIServer

1. `git pull` + **recompilar** (arregla la fuga de thinking en DeepSeek).
2. Mapear `mk-deepseek-v4-flash` → `deepseek-flash` y exponer
   `mk-deepseek-flash` conservando el alias viejo.
3. Actualizar la tabla de precios de DeepSeek (2x) e implementar pico/valle en
   UTC, L-V.
4. Si se ofrecen los modelos de imagen 2.5: aplicar las reglas de parámetros
   (strip `input_fidelity`/`response_format`, `xhigh`/`max` solo en 2.5,
   transparente exige png/webp, mínimo 1024x1024, n≤8).
5. Leer `usage` desde la **raíz** de la respuesta de imágenes y facturar por
   `output_tokens`, no por imagen.
6. Avisar los nombres `mk-gpt-image-2.5-*` para registrarlos en MakerAi.
