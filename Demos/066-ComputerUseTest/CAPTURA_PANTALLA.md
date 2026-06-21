# Computer Use — Cómo manejar la captura de pantalla (066-ComputerUseTest)

Guía de referencia para entender y depurar la captura de pantalla en el demo de
Computer Use de MakerAI. La mayoría de los "problemas de captura" en realidad NO
son que la imagen salga mal, sino que **la imagen y las coordenadas no hablan el
mismo idioma**: el modelo ve un screenshot de un tamaño y el ejecutor hace click
en píxeles de otro tamaño. Este documento explica el contrato que debe cumplirse.

---

## 1. El modelo mental: tres espacios de coordenadas

Hay **tres** resoluciones en juego y deben mantenerse coherentes:

| Espacio | Qué es | De dónde sale en el demo |
|--------|--------|--------------------------|
| **Área física** (`AreaLeft/Top/Width/Height`) | La región real del escritorio que se captura, en **píxeles físicos**. Es donde el ratón hace click de verdad. | `FCU.AreaWidth := GetSystemMetrics(SM_CXSCREEN)` etc. |
| **Resolución declarada** (`ScreenWidth/Height`) | El tamaño que le DECIMOS a la IA que tiene la pantalla. La imagen se envía reducida a este tamaño. | `FCU.ScreenWidth := 1280; FCU.ScreenHeight := Round(AreaHeight*1280/AreaWidth)` |
| **Coordenadas del modelo** | Lo que la IA devuelve en cada acción (`x`, `y`). | Gemini: 0–999 normalizado. Claude: píxeles sobre la resolución declarada. |

**Regla de oro:** la imagen JPG que envías debe medir EXACTAMENTE `ScreenWidth × ScreenHeight`,
y `AreaWidth/Height` debe ser el tamaño físico real capturado. Si rompes cualquiera
de las dos, los clicks se desvían y *parece* que la captura está mal.

Flujo completo (una iteración):

```
1. OnRequestScreenshot  -> capturas AreaWidth×AreaHeight px físicos
                           -> reescalas a ScreenWidth×ScreenHeight
                           -> envías JPG a la IA
2. La IA responde:        click x=N y=M
3. DenormalizeCoordinate  -> convierte (N,M) a píxel físico dentro del Área
4. SendInput / SetCursorPos -> click físico real
```

---

## 2. Las dos rutinas de captura (¡no confundirlas!)

En el repositorio hay **DOS** unidades `TScreenCapture` distintas. El demo usa la
de `Utils`. La otra (`ChatUI` / `AIChat.ScreenCapture`) es la del cliente ClientAI.

| Unidad | Clase | Recorta a | Usada por |
|--------|-------|-----------|-----------|
| `uMakerAi.Utils.ScreenCapture` | `TScreenCapture` | **Virtual screen** (multi-monitor, orígenes negativos) | Este demo + `TAiWindowsFMXExecutor` |
| `AIChat.ScreenCapture` | `TScreenCapture` | Virtual screen, con coords físicas por `GetCursorPos` | ClientAI |

Ambas en Windows hacen lo mismo a bajo nivel: `GetDC(0)` → `CreateCompatibleBitmap`
→ `BitBlt` → `GetDIBits` (32bpp top-down, alpha forzado a 255) → `TBitmapSurface`
(BGRA) → `TBitmap.Assign`. La técnica es correcta y robusta para multi-monitor.

> El demo `CaptureScaled` (`uMainComputerUseTest.pas`) llama a
> `TScreenCapture.CaptureArea(Area)` y luego **reescala él mismo** a la resolución
> declarada y dibuja el cursor. El executor (`TAiWindowsFMXExecutor.CaptureScreen`)
> hace una variante que NO reescala (envía a resolución nativa). En el demo se usa
> la primera; ten claro cuál estás invocando.

---

## 3. Causa #1 de problemas: DPI Awareness

Este es, de lejos, el motivo más común de capturas "raras" (borrosas, recortadas,
o con clicks desviados) en Windows.

`GetSystemMetrics(SM_CXSCREEN)` y `BitBlt(GetDC(0))` devuelven cosas distintas
según el **nivel de DPI awareness del proceso**:

- **App DPI-aware (Per-Monitor-V2)** — *lo que declara el manifiesto FMX por defecto
  en Delphi 11+*:
  - `SM_CXSCREEN` = ancho **físico** del monitor primario (ej. 1920 con escala 150%).
  - `BitBlt` captura píxeles **físicos**. Imagen nítida.
  - Área física y captura coinciden. ✅ Correcto.

- **App NO DPI-aware (`dpiAware=false`)**:
  - Windows **virtualiza**: `SM_CXSCREEN` devuelve la resolución **lógica escalada**
    (ej. 1280 a 150%), y `BitBlt` entrega un framebuffer estirado y **borroso**.
  - Internamente es coherente, pero la imagen pierde nitidez y la IA lee peor el texto.

**Cómo verificarlo:** mira el manifiesto del `.dproj` (Application → Manifest) o
llama en runtime a `GetProcessDpiAwareness`. Para Computer Use **siempre conviene
Per-Monitor-V2**: capturas nítidas en píxeles físicos.

### Trampa multi-monitor con DPI mixto
Si tienes dos monitores a escalas distintas (ej. 100% y 150%), `SM_CXSCREEN` solo
describe el primario. Capturar `Area = (0,0,SM_CXSCREEN,SM_CYSCREEN)` deja fuera
el resto y los clicks en el monitor secundario se desvían. **En este demo,
mantén la app objetivo en el monitor primario** o define el Área explícitamente
con las coordenadas físicas de ese monitor (ver §6).

---

## 4. Causa #2: la captura FMX debe correr en el hilo principal

`TScreenCapture.CaptureArea` termina en `TBitmap.Assign(Surface)`, y el reescalado
y el cursor usan `Canvas.BeginScene/DrawBitmap/EndScene`. **Todo eso toca el
contexto gráfico de FMX y NO es thread-safe.**

Computer Use dispara `OnRequestScreenshot` desde el **hilo de la conexión HTTP**
(background). Si capturas ahí directamente obtienes **imagen negra, vacía o un AV**.

El demo lo hace bien:

```pascal
// uMainComputerUseTest.pas — DoRequestScreenshot
TThread.Synchronize(nil,
  procedure
  begin
    MF := CaptureScaled(Area, OutW, OutH);  // FMX.Canvas -> hilo principal
  end);
```

> Si llamas a `TAiWindowsFMXExecutor.CaptureScreen` por tu cuenta, **envuélvelo
> también en `TThread.Synchronize`** — esa rutina NO sincroniza internamente,
> confía en que el llamador lo haga.

---

## 5. Coordenadas — Gemini vs Claude (cómo se concilian)

`TAiComputerUseTool.DenormalizeCoordinate` aplica la fórmula de **Gemini** (espacio
normalizado 0–999):

```pascal
Result := Round((Coord / 1000) * MaxPixels) + Offset;   // 0..999  ->  px físico
```

- **Gemini** (`gemini-2.5-computer-use`): devuelve siempre **0–999** normalizado,
  independiente de la resolución declarada. La fórmula `/1000 * AreaWidth` es correcta. ✅
- **Claude** (`computer_20251124`): NO normaliza; devuelve **píxeles reales sobre la
  resolución declarada** (`ScreenWidth × ScreenHeight`). **Pero el driver de Claude lo
  concilia aguas arriba**: `TAiComputerUseTool.TranslateClaudeToolCall` (lo invoca el
  driver Claude antes de `ProcessToolCall`) **normaliza los píxeles de Claude a 0–999
  dividiendo por `ScreenWidth`** antes de que corra `DenormalizeCoordinate`:

  ```text
  Claude px (espacio ScreenWidth) --Translate--> norm = px/ScreenWidth*1000 (0..999)
  norm                           --Denormalize-> px_fis = norm/1000*AreaWidth + Offset
  ```

  El resultado neto es **`px_fisico = px · AreaWidth / ScreenWidth + Offset`**, que es
  exactamente la conversión correcta para Claude. **Por tanto los clicks de Claude NO se
  desvían por este motivo** en el flujo actual. ✅

> El componente `TAiComputerUseTool` es Gemini-nativo (su `DenormalizeCoordinate` solo
> implementa `/1000`), pero el driver de Claude adapta las coordenadas con
> `TranslateClaudeToolCall`, así que ambos proveedores quedan alineados sin tocar
> `DenormalizeCoordinate`.
>
> ⚠️ La causa real de clicks desviados que se observó en runtime **no** fue la fórmula,
> sino enviar la imagen a una resolución mayor que el límite del servidor (downscale
> silencioso): por eso el demo envía el screenshot ya reducido a `ScreenWidth × ScreenHeight`
> (≤1280 px). Ver §1 (regla de oro) y §8.4.

---

## 6. Definición correcta del Área

```pascal
// Monitor primario, píxeles físicos (lo que usa el demo):
FCU.AreaLeft   := 0;
FCU.AreaTop    := 0;
FCU.AreaWidth  := GetSystemMetrics(SM_CXSCREEN);
FCU.AreaHeight := GetSystemMetrics(SM_CYSCREEN);
```

- Para capturar **un monitor concreto** en multi-monitor: usa `MonitorFromWindow` /
  `GetMonitorInfo` y pon `AreaLeft/Top` con el origen físico de ese monitor (puede
  ser negativo). La captura por `BitBlt` ya soporta orígenes negativos del virtual screen.
- Para capturar **una ventana concreta**: `GetWindowRect(HWnd)` → usa ese rect como Área.
- **No** mezcles un Área del monitor secundario con `ScreenWidth` calculado sobre el
  primario: recalcula `ScreenHeight := Round(AreaHeight * ScreenWidth / AreaWidth)`
  con el Área que de verdad vas a capturar.

---

## 7. El cursor en la imagen

`BitBlt` **no** incluye el puntero del ratón. Por eso tanto el demo (`CaptureScaled`)
como el executor (`DrawCursorOnBitmap`) lo dibujan a mano: un círculo rojo
semitransparente con borde blanco en la posición de `GetCursorPos`, escalado al
tamaño de salida. Es vital para que la IA "vea" dónde está el ratón antes de actuar.
Si cambias el reescalado, recuerda escalar también la posición del cursor:

```pascal
cx := (Cur.X - Area.Left) * OutW / Max(1, Area.Width);
cy := (Cur.Y - Area.Top)  * OutH / Max(1, Area.Height);
```

---

## 8. Checklist de depuración

Cuando "la captura falle", revisa en este orden:

1. **¿Imagen negra/vacía?** → casi seguro capturaste fuera del hilo principal.
   Envuelve en `TThread.Synchronize`. (§4)
2. **¿Imagen borrosa / texto ilegible?** → proceso no DPI-aware. Activa
   Per-Monitor-V2 en el manifiesto. (§3)
3. **¿Imagen recortada o de otro monitor?** → Área mal definida en multi-monitor.
   Usa el monitor primario o define el Área física explícita. (§3, §6)
4. **¿La imagen está bien pero los clicks caen desviados?** → no es la captura, es
   la coordenada. Verifica:
   - que el JPG enviado mida exactamente `ScreenWidth × ScreenHeight`;
   - que `AreaWidth/Height` sea el tamaño físico real;
   - el divisor de `DenormalizeCoordinate` según proveedor (Gemini 1000 vs Claude
     resolución declarada). (§1, §5)
5. **¿Click desviado?** → revisa la coherencia de tamaños (§1) y que la imagen no
   exceda el límite del servidor (downscale silencioso). Las coordenadas Claude ya
   se concilian en `TranslateClaudeToolCall` (§5), así que no suelen ser la causa.
6. **Para aislar:** guarda el screenshot a disco antes de enviarlo
   (`TScreenCapture.SaveToFile` o `Bmp.SaveToFile`) y ábrelo: confirmarás de un
   vistazo si el problema es la imagen (1–3) o las coordenadas (4–5).

---

## 9. Resumen ejecutable

- Captura **siempre en el hilo principal** (FMX Canvas no es thread-safe).
- Proceso **Per-Monitor-V2 DPI-aware** → píxeles físicos, imagen nítida.
- La imagen enviada debe medir `ScreenWidth × ScreenHeight`; el Área debe ser
  físico real; ambos coherentes.
- App objetivo en el **monitor primario** (o Área física explícita en multi-monitor).
- Dibuja el cursor sobre el bitmap (BitBlt no lo trae).
- Coordenadas: Gemini = 0–999 (`/1000`, nativo del componente); Claude = píxeles sobre
  la resolución declarada, **conciliados por `TranslateClaudeToolCall`** (normaliza
  `/ScreenWidth` → 0–999) antes de `DenormalizeCoordinate`. Net: `px·AreaWidth/ScreenWidth`.
  Ambos proveedores quedan alineados.
