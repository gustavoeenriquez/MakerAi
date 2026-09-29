# Reporte a MKAIServer — Computer Use ya se puede delegar, 2026-09-20

Escrito desde el lado de MakerAi (`E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker`)
para el broker (`E:\Copilot\MKAIServer`). Continúa el
`REPORTE_PARA_MKAISERVER_2026-09-16.md`.

Esto sale de un análisis que llegó desde vuestro lado y que era **correcto en
los dos puntos que señalaba**:

> 1. No hay ejecutor Linux.
> 2. El pass-through del broker es Claude-only en la práctica: astra y Gemini
>    ejecutan igual en local aunque el interceptor ponga `Response`.

Los dos están resueltos. Todo lo que sigue está **verificado en runtime contra
los APIs reales**, no deducido de la documentación.

Commits en `dev` y `master` (ambas en el mismo punto, `0f1fbf1`):

```
83a1656  fix(gemini)         el driver no ejecutaba NINGUNA funcion de usuario
38eeacf  feat(computer-use)  OpenAI y Gemini respetan la intercepcion, como Claude
4d1f81a  fix(computer-use)   Claude manda coordenadas como array y como cadena
2e3e3be  feat(computer-use)  executor de Linux + Computer Use sobre X11
b99e269  fix(websocket)      OpenSSL no validaba el certificado del servidor
0a88e43  fix(shell)          TAiShell: Windows no ingles, stderr y timeout
```

La versión **sigue en 3.7.0 / API_LEVEL 37**: no hubo bump, es contenido
aditivo sobre el tag.

Orden por lo que os bloquea, no por tamaño.

---

## 0. Primero: `git pull` **y recompilar el paquete**, no solo la app

`ResApiServer.dpr` enlaza los drivers por ruta absoluta a nuestro `Source\Chat`
(líneas 89–101), así que esos llegan solos al recompilar. **Pero la clase base
`TAiChat` no**: esa se resuelve por el paquete instalado, y también cambió (el
contrato de intercepción vive en `TAiChat.DoCallFunction`).

```
1. git pull            en E:\Delphi\Delphi13\Compo\FMXCompo\AiMaker
2. recompilar e instalar Source\Packages\MakerAI.dpk
3. rebuild de ResApiServer
```

Si solo hacéis el paso 3, os quedáis con la base vieja y nada de esto funciona.

> **Trampa que encontramos de paso:** `api-server\chat\` tiene copias de
> `UMakerAi.Chat.pas`, `uMakerAi.Core.pas` y `uMakerAi.Chat.Messages.pas` con
> fecha **3 de julio** (la nuestra es de septiembre y pesa 75 KB más). Hoy **no
> se compilan** — esa carpeta no está en el search path del `.dproj` ni la
> enlaza el `.dpr` — pero si alguien añade ese directorio al path, el servidor
> pasará a compilar una base de hace tres meses sin avisar. Conviene borrarlas.

---

## 1. La delegación ya funciona en los tres drivers

El contrato era y sigue siendo el mismo: **si el handler de `OnCallToolFunction`
deja `ToolCall.Response` lleno, el driver no ejecuta la acción en local**. Lo
cumplían la clase base y Claude. Ahora también OpenAI y Gemini.

Qué pasaba antes:

| Driver | Antes | Ahora |
|--------|-------|-------|
| Claude | Respetaba `Response` | igual |
| OpenAI (`gpt-6-astra`) | Disparaba el evento pero **descartaba `Response`** y ejecutaba igual | respeta `Response` |
| Gemini | **Ni siquiera disparaba el evento** | dispara y respeta `Response` |

### Cableado en el broker

```pascal
// 1. Hay que asignar un TAiComputerUseTool AUNQUE NO EJECUTE NADA.
//    En OpenAI el único punto de intercepción vive DENTRO del recorrido del
//    array 'actions', y ese bloque está condicionado a que el tool exista.
//    Sin él no se llama a OnCallToolFunction y no hay nada que delegar.
FCU := TAiComputerUseTool.Create(nil);
FConn.ChatTools.ComputerUseTool := FCU;

// 2. NO asignéis OnExecuteAction ni OnRequestScreenshot: el broker no tiene
//    pantalla. Dejarlos sin asignar es lo correcto.

// 3. El interceptor reclama la llamada para el cliente remoto.
FConn.OnCallToolFunction := ReclamarParaElCliente;

procedure TMiBroker.ReclamarParaElCliente(Sender: TObject;
  AiToolCall: TAiToolsFunction);
begin
  // Con poner cualquier cosa no vacía basta: el driver ya no ejecuta.
  AiToolCall.Response := '{"output":"delegated"}';
  // ...y aquí encoláis/emitís la tool_call hacia vuestro cliente.
end;
```

### Qué hace el driver cuando delegáis

- **No emite** `computer_call_output` y **no recurre**.
- El `computer_call` **crudo queda en el historial** como mensaje `assistant`
  con su `ToolCallId`. **Eso es lo que tenéis que reenviar al cliente.**
- El turno se corta ahí, que es lo que queréis: la respuesta la traerá el
  cliente en la siguiente vuelta.

### La delegación de OpenAI es ATÓMICA sobre el lote

`gpt-6-astra` no manda una acción: manda un `computer_call` con un **array
`actions`** (p.ej. `keypress[WIN,r]` + `type 'notepad'` + `keypress[ENTER]`) y
el API admite **un único** `computer_call_output` por llamada.

Por eso, **en cuanto la primera acción vuelve con `Response` lleno se da por
delegado el lote entero** y no se recorre el resto. Repartir las acciones entre
un ejecutor local y uno remoto dejaría el screenshot final sin dueño.

Consecuencia para vosotros: reenviad el **item completo**, no acción por acción.

### Si os falta el tool, ahora os enteráis

Antes, sin `ComputerUseTool` asignado, el camino síncrono emitía igual un
`computer_call_output` **sin `image_url`** — y el API lo rechaza con 400 porque
exige exactamente uno de `image_url` o `file_id`. El de streaming se lo tragaba
en silencio. Ahora los dos cortan el turno y lo reportan por `LastError`:

```
computer_call call_xxx: cap_ComputerUse esta activo pero ChatTools.ComputerUseTool
no esta asignado. El turno se corta sin responder al modelo.
```

---

## 2. El lado del cliente (el que sí tiene pantalla)

No tiene que interpretar el JSON crudo de cada proveedor. Los traductores son
**públicos justo para esto**:

```pascal
// El cliente recibe la tool_call cruda que le reenvió el broker
LCU := TAiToolsFunction.Create;
LCU.Id        := <call_id>;
LCU.Name      := <tipo de accion>;   // 'click', 'type', 'left_click'...
LCU.Arguments := <json de la accion>;

// Traduce al formato canónico de TAiComputerUseTool
MiTool.TranslateOpenAIToolCall(LCU);   // o TranslateClaudeToolCall(LCU)

// Y ejecuta en local, con sus propios OnExecuteAction / OnRequestScreenshot
LRespuesta := MiTool.ProcessToolCall(LCU, LScreenshot);
```

Para OpenAI, recordad: el `computer_call` trae un array `actions` y el cliente
debe recorrerlo en orden, quedándose **solo con el screenshot de la última**.

### Trampa de Claude que os afecta si reenviáis JSON crudo

Encontramos que **Claude manda las coordenadas unas veces como array y otras
como cadena con el array dentro, dentro del mismo turno**:

```
✅ left_click args={ "coordinate": [299, 282] }     <- las primeras llamadas
❌ left_click args={ "coordinate": "[299, 400]" }   <- a partir de cierto punto
```

Nuestro `TryGetValue<TJSONArray>` no casaba con la segunda forma: la coordenada
se perdía, la acción caía en **(0, 0)** —un clic en la esquina— y el modelo se
desorientaba y reintentaba hasta agotar el turno. Afectaba a `coordinate`,
`start_coordinate`, `region` (zoom) y a numéricos (`"duration": "1"`), es decir
a clic, doble/triple clic, arrastre y zoom.

Arreglado en `4d1f81a`: los traductores aceptan las dos formas. **Si vuestro
cliente parsea el JSON por su cuenta en vez de usar el traductor, tiene que
tolerar ambas.**

---

## 3. Alternativa: que ejecute el propio broker, en Linux

Si en algún escenario preferís no depender de un cliente con pantalla, ahora hay
ejecutor Linux.

`TAiLinuxExecutor` (`Source\Tools\uMakerAi.Tools.ComputerUse.Linux.pas`) cubre
las 19 acciones sobre X11 con **xdotool** y captura con **scrot**. Misma interfaz
pública que los de Windows y macOS, así que es intercambiable.

El framework no necesitó cambios: `TAiComputerUseTool` solo usa la RTL y delega
todo en sus dos eventos, así que cruzó a Linux64 tal cual.

Montaje en un VPS headless (probado de cero en Ubuntu 24.04 y 26.04):

```bash
apt-get install -y xvfb xdotool scrot x11-apps
Xvfb :99 -screen 0 1280x800x24 &
export DISPLAY=:99
# y, si hace falta navegador:
wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
apt-get install -y ./google-chrome-stable_current_amd64.deb
google-chrome --no-sandbox --disable-gpu --window-size=1280,800 <url> &
```

Probado en runtime con `gpt-6-astra` y `claude-opus-4-8`: rellenó un formulario
web completo (clic en campo, escribir, clic en botón) y leyó de vuelta el
resultado exacto de la página. Demo: `083-ComputerUseLinux`.

**Dos cosas que aprendimos y os ahorran tiempo:**

- **No declaréis dimensiones y no reescaléis si no hace falta.** Si la pantalla X
  es de 1280 de ancho y declaráis `ScreenWidth = AreaWidth = 1280`, los clics
  caen donde el modelo cree. El demo 066 de Windows sí reescala (monitor de 3440
  a 1280 declarados) y por eso allí la conversión importa.
- **`catNavigate` usa `xdg-open`** y en un Xvfb pelado no hay manejador
  registrado. Es más fiable lanzar el navegador ya apuntando a la URL y dejar
  que el modelo solo conduzca: la navegación inicial la pone la infraestructura.

Para automatizar **solo navegador**, sigue siendo mejor idea **CDP** (Chrome
DevTools Protocol): se salta Xvfb, xdotool y scrot enteros y deja la golden image
mucho más ligera. No está implementado; sería otro ejecutor, y como son dos
handlers no tocaría nada más del framework.

---

## 4. Dos cambios que pueden romperos algo al recompilar

### ⚠️ TLS: el transporte POSIX ahora valida el certificado

`TOpenSSLTransport` corría con `SSL_VERIFY_NONE`: aceptaba **cualquier**
certificado, de cualquiera. Es el transporte que usa el módulo Realtime en Linux
y macOS. Ahora valida cadena contra el almacén de CAs del sistema **y hostname**.

**Si apuntáis Realtime a un endpoint con certificado propio** (un LM Studio
interno, un proxy vuestro), dejará de conectar hasta que pongáis:

```pascal
LTransport.InsecureSkipVerify := True;
```

En Linux hace falta el paquete `ca-certificates` instalado.

### ⚠️ Gemini: las funciones de usuario ahora se ejecutan

`TAiGeminiChat.DoCallFunction` tenía el `inherited` comentado y respondía
`'Command <nombre> not found'` a **toda** tool call que no fuera Computer Use —
y es el único punto de despacho del driver. Ni `AiFunctions` ni
`OnCallToolFunction` llegaban a dispararse.

Si algo de vuestro lado dependía de ese "not found" (poco probable, pero
conviene mirarlo), ahora se comporta distinto: las funciones se ejecutan.

---

## 5. Lo que NO está verificado

- **Computer Use en Gemini**: el gate y la intercepción están implementados
  igual que en Claude, pero **sin prueba de runtime**. La `GEMINI_API_KEY` de
  aquí no es válida. Si lo usáis, avisadnos de cómo se comporta.
- **El end-to-end del demo 066** con ratón y teclado reales no se re-corrió tras
  estos cambios; el camino local sí está cubierto por el 082 con ejecutor
  simulado.

---

## 6. Resumen de lo que os toca

1. `git pull` en MakerAi, **recompilar e instalar `MakerAI.dpk`**, rebuild de
   ResApiServer.
2. Asignar un `TAiComputerUseTool` al `ChatTools` aunque no ejecute nada.
3. **No** asignar `OnExecuteAction` ni `OnRequestScreenshot`.
4. En `OnCallToolFunction`, rellenar `Response` y encolar la tool_call.
5. Reenviar al cliente el `computer_call` **completo** (el mensaje `assistant`
   crudo del historial), no acción por acción.
6. En el cliente, usar `TranslateOpenAIToolCall` / `TranslateClaudeToolCall`
   antes de `ProcessToolCall`, o tolerar que Claude mande las coordenadas como
   cadena.
7. Revisar si os afecta el cambio de TLS.
8. Borrar `api-server\chat\*.pas`, que son copias de julio.
