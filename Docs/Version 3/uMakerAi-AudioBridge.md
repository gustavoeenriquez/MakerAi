# uMakerAi — Puente de Audio para Traducción en Tiempo Real

**Unidades:** `Source/Utils/uMakerAi.Utils.AudioCapture.pas` · `Source/Utils/uMakerAi.Utils.AudioPlayback.pas`
**Componentes:** `TAiAudioCapture` · `TAiAudioPlayer`
**Demos:** `Demos/061-LoopbackAudioCapture` · `Demos/062-BidirectionalTranslator` · `Demos/063-VoiceBridgeTranslator`
**Plataforma:** Windows (WASAPI, Vista+). Sin drivers ni DLLs adicionales (el cable virtual es opcional, ver §4).

---

## 1. Visión general

Este módulo permite construir un **traductor/intérprete simultáneo de videollamadas**: captura el audio que el sistema reproduce (lo que dice el otro lado de la reunión) y el micrófono local, los transcribe y traduce, y reproduce la voz sintetizada (TTS) en el dispositivo adecuado — incluyendo un **cable virtual** para que la aplicación de reuniones (Zoom, Meet, Teams) reciba la traducción como si fuera el micrófono del usuario.

```text
                 ┌──────────────────────── EQUIPO DEL USUARIO ────────────────────────┐
 Reunión ──────▶ │ auriculares ──(loopback)──▶ TAiAudioCapture ─▶ STT(en) ─▶ Traducir │
 (audio remoto)  │                                                            │ (es)  │
                 │ auriculares ◀──────────── TAiAudioPlayer ◀─────── TTS ◀────┘       │
                 │                                                                    │
                 │ micrófono ───────────────▶ TAiAudioCapture ─▶ STT(es) ─▶ Traducir  │
                 │                                                            │ (en)  │
 Reunión ◀────── │ CABLE Input ◀──────────── TAiAudioPlayer ◀─────── TTS ◀────┘       │
 (mic = CABLE    └────────────────────────────────────────────────────────────────────┘
  Output)
```

Las piezas de IA son intercambiables: el STT es cualquier `TAiRealtimeBase` (driver OpenAI Realtime incluido), la traducción es un `TAiChatConnection` (OpenAI, Claude, Gemini, Ollama local, etc.) y el TTS es `TAiOpenAiAudio` (o ElevenLabs/Gemini Speech).

---

## 2. TAiAudioCapture — captura WASAPI (loopback y micrófono)

Captura el audio que el sistema **reproduce** (modo loopback, sin Stereo Mix ni cables) o un micrófono, y entrega chunks **PCM16 listos para STT** (remuestreo lineal con continuidad entre paquetes y mezcla a mono integrados).

| Propiedad | Default | Descripción |
|-----------|---------|-------------|
| `Source` | `asLoopback` | `asLoopback` = audio de reproducción; `asMicrophone` = micrófono |
| `DeviceId` | `''` | ID WASAPI del endpoint (ver `GetAudioDevices`). `''` = predeterminado |
| `OutputSampleRate` | `16000` | Rate de salida. `0` = nativo del dispositivo (típicamente 48000) |
| `OutputChannels` | `1` | `0` = nativo (máx 2), `1` = mono, `2` = estéreo |
| `ChunkDurationMs` | `100` | Duración de cada chunk entregado en `OnData` |
| `InjectSilence` | `True` | En loopback WASAPI no entrega paquetes si no suena nada; con `True` se inyectan chunks de silencio para mantener el flujo continuo (necesario para VAD/STT en streaming) |
| `Muted` | `False` | Sustituye los chunks por silencio **sin cortar el flujo**. Cambiable con la captura activa. Es la pieza anti-realimentación (§5) |
| `RealtimeSTT` | — | Si se asigna un `TAiRealtimeBase`, cada chunk se reenvía (en mono) al STT, sincronizando `InputSampleRate` automáticamente |

Eventos: `OnData` (chunk PCM16, copia segura), `OnFormat` (formato nativo y efectivo), `OnUpdate` (nivel medio), `OnError`.

```pascal
// Enumerar dispositivos (para loopback se listan los de REPRODUCCIÓN)
for D in TAiAudioCapture.GetAudioDevices(asLoopback) do
  Memo1.Lines.Add(D.DeviceName + IfThen(D.IsDefault, ' (predeterminado)', ''));

Capture := TAiAudioCapture.Create(Self);
Capture.Source := asLoopback;
Capture.OutputSampleRate := 16000;   // óptimo para Whisper / Realtime STT
Capture.OutputChannels := 1;
Capture.RealtimeSTT := MiRealtime;   // o usar OnData manualmente
Capture.Active := True;
```

Notas técnicas:
- El formato nativo de mezcla suele ser float32 48 kHz estéreo; la conversión a PCM16 es interna.
- Los eventos se despachan con `TThread.Queue`; en aplicaciones de consola hay que llamar `CheckSynchronize`.
- Con `InjectSilence` y silencio absoluto, los chunks se emiten a ~2× `ChunkDurationMs` (cadencia reducida, suficiente para mantener vivo el VAD).

---

## 3. TAiAudioPlayer — reproducción WASAPI a dispositivo seleccionable

Pieza simétrica de la captura: reproduce PCM16 en **cualquier endpoint de salida**, no solo el predeterminado. Esto es lo que permite enviar el TTS al cable virtual.

| Propiedad / método | Descripción |
|--------------------|-------------|
| `DeviceId` | ID WASAPI del endpoint de salida. `''` = predeterminado |
| `Active` | Arranca/detiene el hilo de reproducción |
| `PlayPCM16(aData, aRate, aChannels)` | Encola una frase; las frases suenan en orden, sin solaparse |
| `ClearQueue` | Vacía la cola (no corta la frase en curso) |
| `IsPlaying` | True mientras hay audio sonando |
| `OnStateChange(IsPlaying)` | Se dispara al empezar/terminar de sonar la cola — conectar aquí el anti-realimentación (§5) |
| `GetPlaybackDevices` (class) | Enumera los endpoints de salida |

El componente convierte automáticamente al formato del dispositivo (remuestreo lineal + mapeo de canales). El TTS de OpenAI con `TTSResponseFormat := trfPcm` devuelve **PCM16 24 kHz mono**, que se reproduce directo:

```pascal
Player := TAiAudioPlayer.Create(Self);
Player.DeviceId := CableInputEndpointId;  // '' = predeterminado
Player.Active := True;

PcmStream := TTS.Speech('Good morning, the report is ready');  // TAiOpenAiAudio, trfPcm
Player.PlayPCM16(StreamABytes(PcmStream), 24000, 1);
```

---

## 4. El cable virtual (VB-CABLE) — IMPRESCINDIBLE LEER PARA DISTRIBUCIÓN

### 4.1 Por qué se necesita

Windows **no permite inyectar audio en un micrófono físico**. Para que la aplicación de reuniones "escuche" el TTS, hace falta un dispositivo virtual con dos extremos:

- **CABLE Input** — endpoint de *reproducción*: nuestra app reproduce ahí el TTS.
- **CABLE Output** — endpoint de *grabación*: la app de reuniones lo selecciona como micrófono.

Todo lo que entra por `CABLE Input` sale por `CABLE Output`.

### 4.2 Instalación y configuración correcta

1. Descargar de la web oficial: <https://vb-audio.com/Cable/> (VBCABLE_Driver_Pack). Ejecutar el instalador **como administrador** y reiniciar si lo pide.
2. **⚠️ TRAMPA CONOCIDA (verificada):** el instalador puede dejar `CABLE Input` como **dispositivo de salida predeterminado de Windows**. Si se queda así, TODO el audio del sistema (incluida la reunión y nuestro TTS local) se va al cable y el usuario **no oye nada**. Tras instalar:
   - Configuración de Windows → Sistema → Sonido → **Salida = auriculares del usuario**.
3. En la aplicación de reuniones: **Micrófono = `CABLE Output (VB-Audio Virtual Cable)`**, Altavoz = auriculares.
4. **Auriculares obligatorios**: con altavoces, el micrófono físico captura el audio remoto y el TTS, duplicando transcripciones.

Asignación de dispositivos en la app traductora:

| Flujo | Dispositivo |
|-------|-------------|
| Captura loopback (lo que dice la reunión) | El dispositivo donde el usuario ESCUCHA la reunión (auriculares) |
| TTS para el usuario (traducción local) | Auriculares |
| TTS para la reunión (voz traducida) | `CABLE Input` |
| Captura micrófono (voz del usuario) | Micrófono físico |

### 4.3 Detección desde código

```pascal
function FindCableDevice(out aId, aName: string): Boolean;
var
  D: TAiAudioDeviceInfo;
begin
  Result := False;
  for D in TAiAudioPlayer.GetPlaybackDevices do
    if Pos('cable input', LowerCase(D.DeviceName)) > 0 then
    begin
      aId := D.EndpointId;
      aName := D.DeviceName;
      Exit(True);
    end;
end;
```

La app de distribución debe verificar el cable al arrancar y, si falta, guiar al usuario a instalarlo (o degradarse a modo "solo subtítulos" sin TTS hacia la reunión).

### 4.4 Licencia y redistribución — IMPORTANTE

- **VB-CABLE es donationware de VB-Audio Software.** La licencia estándar permite al usuario final instalarlo gratuitamente, pero **NO permite redistribuir el driver dentro del instalador de una aplicación comercial sin autorización**.
- Opciones para la app de distribución:
  1. **Recomendada:** el instalador/la app detecta la ausencia del cable y abre la página oficial de descarga (o lanza el instalador descargado por el usuario). Cero riesgo legal.
  2. **Licencia de distribución:** contactar a VB-Audio (<https://vb-audio.com/Services/licensing.htm>) para obtener autorización de bundling/OEM.
  3. **Alternativas:** "Virtual Audio Cable" de E. Muzychenko (licencia comercial de pago, redistribuible bajo acuerdo) o desarrollar un driver propio (APO/AVStream — coste alto).
- El nombre del dispositivo (`CABLE Input/Output`) puede variar con otros cables; hacer el patrón de búsqueda configurable.

---

## 5. Anti-realimentación: la propiedad `Muted`

Cuando el TTS local (la traducción que escucha el usuario) suena por los auriculares, el **loopback lo captura** — y el sistema retraduciría su propia voz sintética en bucle.

Solución integrada: silenciar la captura **sin cortar el flujo** mientras suena el propio TTS.

```pascal
// Antes de reproducir nuestro TTS en un dispositivo que el loopback captura:
CaptureLoopback.Muted := True;
PlayerLocal.PlayPCM16(Pcm, 24000, 1);

// En PlayerLocal.OnStateChange:
procedure TForm1.PlayerStateChange(Sender: TObject; aIsPlaying: Boolean);
begin
  if not aIsPlaying then
    CaptureLoopback.Muted := False; // la cola terminó de sonar
end;
```

**Compromiso de diseño:** mientras está silenciado, si el interlocutor remoto sigue hablando, ese fragmento no se traduce (igual que un intérprete simultáneo que está hablando no puede escuchar). El TTS hacia el cable (`CABLE Input`) NO necesita mute, porque el loopback no captura ese dispositivo.

---

## 6. Checklist para la app de distribución

- [ ] **Selección explícita de dispositivos en la UI** (loopback, mic, TTS local, TTS reunión) con persistencia. No confiar en el dispositivo predeterminado: el instalador del cable puede cambiarlo (§4.2).
- [ ] Verificación del cable al arrancar + asistente de instalación (enlace oficial, §4.4).
- [ ] Aviso de auriculares (o detección de eco: si el mic transcribe lo mismo que el loopback, alertar).
- [ ] Modelo Realtime STT: usar `'gpt-realtime'` (el default del driver, `gpt-4o-realtime-preview`, fue retirado por OpenAI; ver `uMakerAi.Realtime.OpenAI.GetDefaultModel`).
- [ ] API keys con la convención `@VAR_DE_ENTORNO` del framework (no hardcodear).
- [ ] Manejo de errores `OnError` de los 4 componentes de audio + reconexión del STT.
- [ ] Si el usuario cambia el dispositivo predeterminado en caliente, reiniciar la captura/reproducción afectada (`Active := False/True`).
- [ ] Latencia esperada del ciclo completo (fin de frase → VAD → STT → traducción → TTS → audio): 2–4 s con OpenAI. Mostrar subtítulos del texto traducido mientras llega el TTS mejora mucho la percepción.
- [ ] Costes: el STT Realtime + TTS facturan por minuto de audio; `InjectSilence` mantiene el stream activo (el VAD del servidor descarta silencio, pero el audio se envía — para llamadas largas considerar pausar el envío con `Muted`/desconexión cuando no hay actividad).

---

## 7. Diarización: identificar quién habla en la reunión

El Realtime STT no diariza. Para etiquetar a los participantes remotos se usa el endpoint REST con **`gpt-4o-transcribe-diarize`** (`TAiOpenAiAudio`, ver `Source/Tools/uMakerAi.OpenAI.Audio.pas`):

- `TranscriptionModel := tmGpt4oDiarize` + `TranscriptionResponseFormat := trfDiarizedJson` → `TTranscriptionResult.Segments` (Speaker/Text/StartTime/EndTime) y `DiarizedText`.
- **Hablantes conocidos**: `AddKnownSpeaker(nombre, muestraDeVoz)` (máx. 4; la API exige muestras de **1.2 a 10.0 s** — usar 2–9 s para evitar rechazos en el borde) hace que los segmentos usen nombres reales.
- **Etiquetas consistentes entre peticiones**: las letras A/B/C solo son estables dentro de UNA petición. El patrón validado (demo 064) es el *auto-registro*: la primera vez que aparece una voz, se recorta su tramo más largo del segmento (2–9 s) y se registra como "Hablante N"; las peticiones siguientes devuelven ese nombre de forma estable.
- Arquitectura del canal remoto diarizado: VAD local por nivel (segmentos de habla con pre-buffer) → `Transcribe` diarizado → traducir por hablante → TTS con una voz distinta por hablante.
- Resiliencia: si la API rechaza las muestras registradas (`known_speaker_references`), limpiar con `ClearKnownSpeakers` y reintentar sin ellas (se re-registran en los siguientes segmentos).
- Latencia: el canal diarizado es REST por segmento (~2–4 s tras el fin de frase). Para subtítulo instantáneo + atribución se puede combinar Realtime (texto inmediato) con diarización en paralelo (etiqueta).

## 8. Demos de referencia

| Demo | Qué muestra |
|------|-------------|
| `061-LoopbackAudioCapture` | Captura loopback básica → WAV 16 kHz mono |
| `062-BidirectionalTranslator` | Traductor bidireccional con salida de TEXTO (loopback + mic → STT → traducción) |
| `063-VoiceBridgeTranslator` | Puente de voz completo: TTS hacia auriculares y hacia `CABLE Input`, anti-realimentación con `Muted`, detección del cable y modo prueba sin cable |
| `064-VoiceBridgeDiarized` | Puente de voz con diarización: VAD local + `gpt-4o-transcribe-diarize`, auto-registro de hablantes (etiquetas estables) y una voz TTS distinta por participante |

Requisitos de los demos 062/063/064: variable de entorno `OPENAI_API_KEY`.

---

*MakerAI Suite — Gustavo Enríquez · documento generado junio 2026*
