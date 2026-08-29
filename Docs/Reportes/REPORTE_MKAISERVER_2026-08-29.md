# Reporte a MakerAi — hallazgos desde MKAIServer, 2026-08-29

Escrito desde el lado del broker (`E:\Copilot\MKAIServer`) tras un día de
diagnóstico en producción. Todo lo que sigue está **medido**, no deducido; cada
punto lleva su evidencia y cómo reproducirlo.

Orden por urgencia, no por tamaño.

---

## 0. URGENTE — hay un arreglo sin commitear y está en peligro

```
$ git status --short
 M Source/Chat/uMakerAi.Chat.MakerAi.pas
$ git log --oneline dev..origin/dev
(vacío)
```

El arreglo del usage a cero en el driver `MakerAi` **existe solo como cambio
local en el checkout compartido**. No está commiteado ni pusheado.

Esto no es teórico: hoy mismo, revirtiendo un intento fallido mío, ejecuté
`git checkout -- Source/Core/uMakerAi.Chat.pas` en este repo. Nombré el fichero
uno a uno y por eso ese cambio se salvó. Un `git checkout .` o un
`git reset --hard` lo habría borrado sin aviso.

**Acción: commitear y pushear ya.** Es lo único de este reporte que corre riesgo
de perderse.

---

## 1. `uMakerAi.Chat.DeepSeek.pas` tiene el MISMO bug del usage a cero

Es el hermano exacto del que se acaba de arreglar en el driver MakerAi, y este
sí llega a producción de MKAIServer: `ResApiServer.dpr:87` enlaza el driver
DeepSeek.

**Dónde** — `uMakerAi.Chat.DeepSeek.pas:336-338`, en el cierre sintético del
stream:

```pascal
FakeUsage.AddPair('prompt_tokens',     TJSONNumber.Create(0));
FakeUsage.AddPair('completion_tokens', TJSONNumber.Create(0));
FakeUsage.AddPair('total_tokens',      TJSONNumber.Create(0));
```

**Consecuencia medida hoy en producción**, llamada real por el broker con
`stream:true` a `mk-deepseek-v4-flash`:

```
46 chunks de salida real
usage en el SSE:  prompt_tokens 0 · completion_tokens 0 · total_tokens 0
facturado:        input 0 · output 0 · cobro $0.000000
```

**Es una fuga de dinero**: todo el tráfico DeepSeek en streaming se factura a
cero. Sin streaming el mismo modelo contabiliza bien (medido: 18 in / 37 out),
así que el agujero es exclusivo de esa ruta.

**Cómo arreglarlo** — la clase base YA lo hace bien; solo hay que dejar de
duplicar el cierre. En `uMakerAi.Chat.pas`:

- `:2705-2706` captura el usage de los chunks SSE en `FStreamPromptTokens` /
  `FStreamCompletionTokens`.
- `:2464-2466` los usa en el cierre sintético.
- `:2468-2469` los reinicia, para que un turno sin usage no herede el anterior.

Es exactamente el patrón de tres piezas que se aplicó al driver MakerAi. Aquí
basta con replicarlo o, mejor, con que el driver reutilice el cierre de la base
en vez de fabricar el suyo.

**Criterio de aceptación:** una llamada con `stream:true` a un modelo DeepSeek
devuelve en el último chunk un `usage` con los tokens reales, y coincide con lo
que reporta la misma llamada sin streaming.

---

## 2. Inventario: qué drivers duplican el cierre sintético

```
uMakerAi.Chat.DeepSeek.pas           <- activo, con el bug (punto 1)
uMakerAi.Chat.MakerAi.pas            <- arreglado, pendiente de commit (punto 0)
uMakerAi.Chat.OpenAi_Deprecated.pas  <- deprecado, probablemente da igual
```

Los demás drivers usan el cierre de la clase base y por eso no sufren esto. La
raíz del problema no es un despiste puntual: es que **el cierre sintético se
copió en vez de reutilizarse**, así que cada mejora en la base (usage,
`finish_reason`, `reasoning_content`) hay que acordarse de replicarla en tres
sitios. Si se puede unificar, se cierra la familia entera de bugs de golpe.

---

## 3. `finish_reason` fijo en `'stop'` en el cierre sintético

Mismo patrón, otro campo. En los tres sitios del inventario, y también en la
clase base:

```
uMakerAi.Chat.pas:~2484        FakeChoice.AddPair('finish_reason', 'stop');
uMakerAi.Chat.DeepSeek.pas:351 FakeChoice.AddPair('finish_reason', 'stop');
```

Consecuencia: una respuesta **cortada por `max_tokens`** se le presenta al
consumidor como un final limpio. No hay forma de distinguir "terminó de hablar"
de "se quedó a medias", que es justo lo que un cliente necesita para decidir si
reintenta con más presupuesto o avisa al usuario.

**Aviso honesto de lo que NO funcionó**, para que no se repita el camino: lo
intenté por aquí y fallé. Añadí la captura del `finish_reason` real de los
deltas SSE, lo expuse como propiedad, lo leí en `ParseChat` y lo usé en el
cierre. Compilaba, desplegaba y **no cambiaba nada**: el consumidor seguía
viendo `stop`. Reverti todo.

Lo que sí funcionó fue arreglarlo **fuera de la librería**, en el consumidor:
MKAIServer lee `choices[0].finish_reason` del JSON crudo del proveedor en su
propio bridge de streaming. Verificado con `completion_tokens` como testigo:

```
max_tokens=40 -> finish=length  completion_tokens=40
max_tokens=60 -> finish=length  completion_tokens=60
```

O sea: el dato llega bien por el cable, pero **algo entre el cierre sintético y
quien consume la librería lo sigue pisando**. No encontré qué. Si se ataca desde
dentro de AiMaker, ese es el hilo, y conviene verificar con un consumidor real
antes de darlo por bueno — a mí me engañó tres veces.

---

## 4. `Thinking_tokens` solo lo rellenan dos drivers

```
uMakerAi.Chat.Gemini.pas   (6 asignaciones)
uMakerAi.Chat.OpenAi.pas   (2 asignaciones)
```

Claude, DeepSeek, GLM, Kimi y los demás nunca lo tocan. Lo arreglé para el
dialecto OpenAI en la clase base (commit `1f87db9`, ver punto 5), pero **Claude
sigue sin reportarlo**: parsea su propio SSE y no pasa por ahí.

Ojo con un detalle contable al tocarlo: en el dialecto OpenAI los tokens de
razonamiento **ya vienen dentro de `completion_tokens`**, así que sumarlos
aparte los duplica. En Gemini es al revés: `thoughtsTokenCount` va FUERA de
`candidatesTokenCount`. Esa asimetría nos costó un bug de totales inflados en el
broker; está documentada en `MKAIServer/database/migrations/migration_025`.

---

## 5. Lo que YA cambié en este repo hoy — no rehacer

Cuatro commits en `dev`, todos desplegados y verificados en producción:

| Commit | Qué |
|---|---|
| `1f87db9` | `ParseChat` captura `reasoning_tokens` del usage (dialecto OpenAI). Antes `Thinking_tokens` era siempre 0 para GLM, DeepSeek, Kimi y gpt-oss |
| `89e9bc9` | `DoCallFunction` ya no secuestra las tools cuyo nombre contiene `_99_`. Bifurcaba solo por `Pos(MCP_TOOL_SEP, Name)`, así que una tool legítima llamada `alfa_99_beta` se buscaba como servidor MCP, no se encontraba y quedaba muda |
| `3ff9631` | Driver Claude: no declarar `code_execution` cuando `web_search_20260209` está presente, porque Anthropic lo auto-inyecta y rechaza la petición entera por nombres duplicados |
| `9914654` | `ParseChat` deja de usar el razonamiento como respuesta cuando hay `tool_calls` o `finish_reason='length'` |

El último merece una nota, porque es de la misma familia que lo demás: el
fallback "si `content` viene vacío, usa `reasoning`" tiene sentido para modelos
que responden solo en reasoning, pero mandaba el monólogo interno del modelo
—en inglés— como si fuera la contestación al usuario. Reproducido de forma
estable con Kimi (`content` de 184 chars idéntico al `reasoning`), y **no era un
problema de GLM** aunque así se reportara.

---

## 6. Nota de método

Tres de los bugs de hoy solo aparecieron **verificando después de desplegar**, no
antes. Y dos veces di por bueno un arreglo que compilaba y no hacía nada. Con
esta librería, la única prueba que vale es medir el efecto desde un consumidor
real; el compilador no dice nada útil sobre estos fallos.

---

*Generado desde la sesión de MKAIServer del 2026-08-29. Las evidencias de
producción son del SaaS (`api.cimamaker.com`); calera1 quedó a propósito con el
binario anterior porque estaba en uso.*
