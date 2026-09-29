# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Demo 089 — **Qwen (Alibaba Model Studio) en MakerAI**: un recorrido por todo lo que el framework hace con Qwen usando una sola API key. Consola. **Requiere `DASHSCOPE_API_KEY`** (región internacional, Singapur: con claves de otra región el API responde 401).

| Sección | Qué muestra | Componentes |
|---------|-------------|-------------|
| `chat` | síncrono, streaming, razonamiento con presupuesto (`cap_Reasoning` + `ThinkingLevel`), herramientas (`TAiFunctions`) | `TAiChatConnection` driver `Qwen` |
| `vision` | generar una imagen (`qwen-image-2.0`) → editarla (`qwen-image-edit-plus`, la imagen adjunta es la entrada) → describirla (`qwen3.8-flash`) | mismo conector, gap `cap_GenImage` |
| `voz` | TTS (`qwen3-tts-flash`) → transcripción en `cmTranscription` con el prompt como contexto → audio entendido por `qwen3.8-omni-flash` | `TtsParams`, `ChatMode` |
| `traduccion` | `qwen-mt-plus` con `TranslateTo`, `TranslateDomain` y glosario `TranslateTerms`; inglés y japonés | `TAiQwenChat` |
| `rag` | embeddings `text-embedding-v4` con ranking por similitud coseno + `qwen3-rerank` con `Instruct` | `TAiEmbeddingConnection`, `TAiQwenRAGReranker` |
| `realtime` | LLM → voz: los fragmentos del chat en streaming van directo al TTS en tiempo real; traducción simultánea por el conector universal con `DriverParams` | `TAiQwenRealtimeTTS`, `TAiRealtimeConnection` driver `QwenTranslate` |
| `--video` | texto a video con `wan2.6-t2v`, 2 s a 720p con audio | gap `cap_GenVideo`, `VideoParams` |
| `--voces` | clonar la voz de la sección `voz`, hablar con ella y borrarla de la cuenta | `TAiQwenVoices` |

## Build & Run

**IDE:** RAD Studio (Delphi 11 Alexandria a 13 Florence). Abrir `QwenShowcase.dproj`, build Win64.

```bash
QwenShowcase.exe                     # todas menos video y voces
QwenShowcase.exe vision voz          # solo esas
QwenShowcase.exe --video --voces     # agrega las opcionales (video cuesta mas)
```

Los archivos quedan en `salida/` (ignorada por git): `faro.png`, `faro_verde.png`, `voz.wav`, `llm_voz.wav`, `traduccion_en.wav`, `traduccion_ja.txt` y, con las opcionales, `video.mp4` y `voz_clonada.wav`. Exit code: 0 ok, 2 si falta la API key o hay excepción.

## Notas

- **Resultado verificado (sep 28/2026)**, todas las secciones en una corrida: chat (la herramienta se llamó con `{"moneda": "USD"}`; el razonamiento con `tlLow` dio 100000 con ~260 caracteres de razonamiento), la imagen editada solo cambió el color del faro, transcripción exacta ("MakerAI" y "Delphi" gracias al contexto; omni oyó "Make AI" / "Delfi"), traducción "petty cash" / "deductible VAT", embeddings 0.70 para el pasaje correcto contra 0.21 el irrelevante y reranker 0.97, LLM → voz con primer audio a ~1.8 s de la pregunta, traducción simultánea español → inglés con voz, video de 2 s y voz clonada.
- **Consola y eventos:** todo evento llega por `TThread.Queue`; el demo drena la cola con `CheckSynchronize` en `Esperar`. En una app VCL/FMX no hace falta.
- **`Model` se asigna después de `DriverName`** (cambiar de driver lo vacía). Sin `Model` se usa el modelo por defecto del driver con sus parámetros del catálogo; cada sección asigna el suyo.
- La traducción simultánea recibe el WAV de 24 kHz como si fuera un micrófono (bloques de 100 ms); el conector remuestrea a los 16 kHz del modelo. Este modelo cierra el turno tras ~2.5 s de silencio, por eso se envían 3 s al final.
- Clonar voces solo con permiso de su dueño. Aquí la muestra es la voz sintética del TTS.

## Key Source

| Componente | Unit |
|------------|------|
| `TAiQwenChat` (chat, imagen, video, TTS, ASR, traducción) | `Source/Chat/uMakerAi.Chat.Qwen.pas` |
| `TAiQwenEmbeddings` | `Source/Embeddings/uMakerAi.Embeddings.Qwen.pas` |
| `TAiQwenRAGReranker` | `Source/Tools/uMakerAi.Qwen.Rerank.pas` |
| `TAiQwenVoices` | `Source/Tools/uMakerAi.Qwen.Voices.pas` |
| `TAiQwenRealtimeChat` / `STT` / `Translate` | `Source/Realtime/uMakerAi.Realtime.Qwen.pas` |
| `TAiQwenRealtimeTTS` | `Source/Realtime/uMakerAi.Realtime.QwenTTS.pas` |

## Navigation

> See [../CLAUDE.md](../CLAUDE.md) for demos overview and [../../CLAUDE.md](../../CLAUDE.md) for project overview.
