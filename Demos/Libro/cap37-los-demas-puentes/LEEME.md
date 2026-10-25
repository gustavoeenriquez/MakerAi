# Capítulo 37 — Los demás puentes

Consola `PuentesLadera` (`--voz`, `--vision`, `--web`, `--embeddings`; sin argumentos, los cuatro).
El mismo chat (`deepseek-flash`, que no oye, no ve y no busca) cubre cada hueco con otra pieza:
OpenAI, ElevenLabs y Whisper para la voz; Ollama (`gemma4:e4b`, `deepseek-ocr`) para la visión y el OCR;
la búsqueda web de OpenAI; y embeddings de OpenAI, Mistral, Cohere y Ollama (`nomic-embed-text`).

Requiere `DEEPSEEK_API_KEY`, `OPENAI_API_KEY`, `ELEVENLABS_API_KEY`, `MISTRAL_API_KEY`, `COHERE_API_KEY` y un Ollama con
`gemma4:e4b`, `deepseek-ocr` y `nomic-embed-text`: el de `localhost:11434` o el que diga `OLLAMA_URL` (en el libro, un
DGX Spark de la red local: `OLLAMA_URL=http://192.168.3.121:11434/`). Gemini (voz, grounding, Veo) queda marcado
como pendiente. `media\`: la nota de voz y la foto del cap 9 y la pantalla de Andina Despachos del cap 35; `salida\` se crea.
Café La Ladera: empresa, productos y precios ficticios.
