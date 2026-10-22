# CLAUDE.md

Demo 095: chat FMX con `TAIChatView` y `TAIChatInput`, con imágenes, PDF y voz. La barista es Lucía, de Café La Ladera. Es la base de los episodios 4 (interfaz) y 5 (imágenes, PDF y voz) de la serie.

## Qué muestra

- **Streaming en burbujas.** `BeginAssistantMessage`, luego `AppendToken` y al final `FinishMessage`. Los eventos llegan desde el hilo HTTP, así que la UI se toca con `TThread.Queue`.
- **Pasos y pie de cada burbuja.** Mientras el modelo razona, `OnReceiveThinking` muestra el paso "Pensando" con `AddStep`/`FinishStep`. Al terminar, `SetMessageFooter` pone tokens y segundos.
- **Imágenes y PDF.** Para OpenAI, `ModelCaps := [cap_Image, cap_Pdf, cap_Reasoning]`, porque `gpt-6-sol` lee los dos por la API Responses.
- **Voz.** `TAIVoiceMonitor` graba y `TAIChatInput` entrega el WAV en `OnSendEvent`. El modelo no escucha audio, así que la sesión pide `cap_Audio` (`SessionCaps := ModelCaps + [cap_Audio]`). El framework lo transcribe antes de llamar al modelo con `ChatTools.SpeechTool`, un `TAiOpenAiSpeechTool` con `gpt-transcribe`. La burbuja del usuario muestra lo que se entendió (`GetMediaTranscription`).
- **Proveedor.** El combo cambia entre OpenAI, Claude, Gemini y Ollama. Cambiar `DriverName` recrea el chat, así que el historial empieza de cero.
- **Guardar y abrir.** Son dos cosas distintas: lo que se ve (`ChatView1.SaveToStream`) y lo que el modelo recuerda (`Messages.SaveToFile`). Se guardan en `Documentos\CafeLaLadera`.
- **Cerrar a mitad de un turno.** `FormCloseQuery` desconecta los eventos, aborta y espera la tarea (`FTarea.Wait` + `CheckSynchronize`). Sin eso, cerrar mientras se transcribía la nota colgaba la app, porque la tarea seguía usando la conexión ya destruida.

## Detalles que muerden

- **Orden del `uses`.** `AIChat.Types` también declara un `TAIChatMessage`, y Delphi no distingue mayúsculas. Va antes de `uMakerAi.Chat.Messages` para que gane el del framework.
- **Adjuntos.** `TAIChatInput` libera `aMediaFiles` y `aAudioStream` al volver de `OnSendEvent`. `ToMediaFileArray` devuelve copias, y el audio se copia en un `TAiMediaFile`.
- **Hilo aparte.** `AddMessageAndRun` corre en un `TTask` porque el puente de audio es síncrono, y sin el hilo la ventana se congela mientras transcribe.

## Prueba automática

```bash
ChatViewLadera.exe --prueba "pregunta" "@C:\ruta\bolsa.png|¿Qué café es este?" "@C:\ruta\nota.wav|" --captura salida.png
```

Envía cada pregunta como si la escribiera el usuario (`@ruta|texto` adjunta un archivo). Al final guarda una captura del chat con `MakeScreenshot` y cierra. No mueve el mouse ni el teclado del escritorio.

## Probado (oct 22/2026)

Con OpenAI `gpt-6-sol`:
- **Texto, en streaming y con Markdown.** Recomendó Sierra Nevada Orgánico y luego calculó precios con el historial.
- **Foto de la bolsa.** Leyó la etiqueta.
- **Factura en PDF.** Sacó el total y el vencimiento.
- **Nota de voz en WAV.** Se transcribió con `gpt-transcribe`, se mostró en la burbuja del usuario y la respuesta recomendó el Descafeinado Suave.
- **Cierre a mitad de turno.** Probado a los 2,5, 3,5 y 6 s; cerró limpio.

La nota de voz se probó como adjunto. Es el mismo camino que el micrófono desde `OnSendEvent`, pero la captura con `TAIVoiceMonitor` no se probó aquí.

Esta prueba encontró un bug del framework, ya corregido en `TAiChat.RunNew` (caso `chat.bridge.speechtool-dataend`). El SpeechTool del puente de audio disparaba `OnReceiveDataEnd` con la transcripción cruda. La UI cerraba el turno antes de que respondiera el modelo, y la respuesta real se perdía.

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
