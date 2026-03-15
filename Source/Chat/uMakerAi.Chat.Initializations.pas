// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/



unit uMakerAi.Chat.Initializations;

interface

uses
  System.SysUtils, uMakerAi.ParamsRegistry, uMakerAi.Embeddings;

implementation

Procedure InitChatModels;
Var
  Model: String;
Begin


  // ===================================================================
  // CONFIGURACION GLOBAL DE OLLAMA
  // https://ollama.com/library
  // Ultima actualizacion: Feb 2026
  // ===================================================================
  // Por defecto: texto puro, sin tools nativos (conservador)
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens',   '8000');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Asynchronous', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'SessionCaps',  '[]');

  // ------- Llama 3.3 (texto + tools, 128K ctx) ------
  Model := 'llama3.3:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'True');

  // ------- Qwen 2.5 (texto + tools, 128K ctx) ------
  Model := 'qwen2.5:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'True');

  // ------- Qwen3 (texto + tools + reasoning, 128K ctx) ------
  Model := 'qwen3:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active',   'True');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ThinkingLevel', 'tlMedium');

  // ------- DeepSeek R1 (reasoning via <think>, sin tools) ------
  Model := 'deepseek-r1:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Llama 3.2 Vision (vision, 11B, 128K ctx) ------
  Model := 'llama3.2-vision:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps', '[cap_Image]');

  // ------- Qwen 2.5 VL (vision avanzada + tools, 128K ctx) ------
  Model := 'qwen2.5vl:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'True');

  // ------- Gemma 3 (vision + tools, tamanios: 1b/4b/12b/27b) ------
  // Todos los tamanios comparten las mismas capacidades
  Model := 'gemma3:1b';  // = gemma3:4b
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'True');

  Model := 'gemma3:4b';  // = gemma3:4b
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'True');

  Model := 'gemma3:12b';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'True');

  Model := 'gemma3:27b';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'True');


  // ------------------------- OPENAI ----------------------------------
  // https://platform.openai.com/docs/models
  // Ultima actualizacion: Feb 2026
  // ------------------------- OPENAI ----------------------------------

  // --- Valores globales por defecto para todos los modelos OpenAI ---
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Temperature', '1');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'SessionCaps', '[cap_Image]');

  // ------- GPT-4.1 (Apr 2025) -- 1M ctx, 32K output, vision + tools ------
  // https://platform.openai.com/docs/models/gpt-4.1
  Model := 'gpt-4.1';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',   '32768');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Image]');

  Model := 'gpt-4.1-mini';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',   '32768');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Image]');

  Model := 'gpt-4.1-nano';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',   '32768');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Image]');

  // ------- GPT-4o -- 128K ctx, 16K output, vision + tools ------
  // https://platform.openai.com/docs/models/gpt-4o
  Model := 'gpt-4o';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',   '16384');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Image]');

  Model := 'gpt-4o-mini';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',   '16384');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Image]');

  // ------- Razonamiento: o3 / o4-mini -- 200K ctx, 100K output ------
  // https://platform.openai.com/docs/models/o3
  Model := 'o3';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',    '100000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',    '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps',  '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  Model := 'o3-pro';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',    '100000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',    '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps',  '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlHigh');

  // https://platform.openai.com/docs/models/o4-mini
  Model := 'o4-mini';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',    '100000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',    '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps',  '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Deep Research -- tiempo de respuesta extendido ------
  Model := 'o3-deep-research';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',    '100000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',    '[cap_Reasoning, cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps',  '[cap_Reasoning, cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ResponseTimeOut', '120000');

  Model := 'o4-mini-deep-research';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',    '100000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',    '[cap_Reasoning, cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps',  '[cap_Reasoning, cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ResponseTimeOut', '120000');

  // ------- Busqueda web nativa via Responses API ------
  // Nota: gpt-4o-search-preview solo funciona via Chat Completions API (no Responses API)
  // Para Responses API usar gpt-4.1 + tool web_search (recomendado)
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_gpt-4.1-web-search', 'gpt-4.1');
  Model := 'aa_gpt-4.1-web-search';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Max_Tokens',   '32768');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Image, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Image, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // Legacy Chat Completions search models (pueden no funcionar via Responses API)
  Model := 'gpt-4o-search-preview';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  Model := 'gpt-4o-mini-search-preview';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- Generacion de imagenes ------
  // https://platform.openai.com/docs/guides/images
  // ModelCaps=[]: usa endpoint dedicado; Gap=[cap_GenImage] activa InternalRunImageGeneration
  Model := 'gpt-image-1';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',      '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps',    '[cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active',    'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ResponseTimeOut', '36000');

  Model := 'dall-e-3';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  Model := 'dall-e-2';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- Audio TTS -- ModelCaps=[]: usa endpoint TTS dedicado ------
  // Gap=[cap_GenAudio] activa InternalRunSpeechGeneration
  // https://platform.openai.com/docs/guides/audio
  Model := 'gpt-4o-mini-tts';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps',  '[cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Voice',        'alloy');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Voice_Format', 'mp3');

  // ------- Transcripcion (STT) -- cap_Audio: procesa audio nativo ------
  // Usar con ChatMode = cmTranscription
  Model := 'gpt-4o-transcribe';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  Model := 'gpt-4o-mini-transcribe';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- Audio multimodal (audio I/O en completions) ------
  // cap_Audio + cap_GenAudio en ModelCaps: procesa y genera audio via completions
  Model := 'gpt-4o-audio-preview';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Audio, cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Audio, cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  Model := 'gpt-4o-mini-audio-preview';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Audio, cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Audio, cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- Perfiles personalizados (aa_*) ------
  // GPT-4.1 con soporte PDF nativo (file API de OpenAI)
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_gpt-4.1-pdf', 'gpt-4.1');
  Model := 'aa_gpt-4.1-pdf';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ModelCaps',   '[cap_Image, cap_Pdf]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'SessionCaps', '[cap_Image, cap_Pdf]');

  // o3 con nivel de razonamiento alto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_o3-high', 'o3');
  Model := 'aa_o3-high';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlHigh');

  // o4-mini -- niveles de razonamiento alto y bajo
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_o4-mini-high', 'o4-mini');
  Model := 'aa_o4-mini-high';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlHigh');

  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_o4-mini-low', 'o4-mini');
  Model := 'aa_o4-mini-low';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlLow');


  // ------------------------- GEMINI ----------------------------------
  // https://ai.google.dev/gemini-api/docs/models
  // Ultima actualizacion: Feb 2026
  // ------------------------- GEMINI ----------------------------------

  // --- Valores globales por defecto para todos los modelos Gemini ---
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Max_Tokens',  '32768');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'SessionCaps', '[cap_Image]');

  // ------- Gemini 2.5 Flash -- 1M ctx, 65K output, multimodal completo ------
  // https://ai.google.dev/gemini-api/docs/models/gemini-2.5-flash
  // Entrada: Audio, Video, PDF, Imagenes; WebSearch+CodeInterpreter+Thinking nativos
  Model := 'gemini-2.5-flash';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Max_Tokens',   '65536');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active', 'True');

  // ------- Gemini 2.5 Flash Lite -- budget/rapido, multimodal ------
  Model := 'gemini-2.5-flash-lite';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Max_Tokens',   '32768');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf]');

  // ------- Gemini 2.5 Pro -- 1M ctx, alta capacidad ------
  // https://ai.google.dev/gemini-api/docs/models/gemini-2.5-pro
  Model := 'gemini-2.5-pro';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Max_Tokens',   '65536');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active', 'True');

  // ------- Gemini 3 Pro Preview -- 1M ctx, razonamiento avanzado ------
  // https://ai.google.dev/gemini-api/docs/models/gemini-3-pro-preview
  Model := 'gemini-3-pro-preview';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Max_Tokens',   '65536');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ThinkingLevel', 'tlHigh');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active',   'True');

  // ------- Gemini 3.1 Pro Preview -- Feb 2026, mas avanzado ------
  // https://ai.google.dev/gemini-api/docs/models/gemini-3.1-pro-preview
  Model := 'gemini-3.1-pro-preview';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Max_Tokens',   '65536');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch, cap_Reasoning, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ThinkingLevel', 'tlHigh');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active',   'True');

  // ------- TTS -- ModelCaps=[]: usa endpoint TTS dedicado ------
  // Gap=[cap_GenAudio] activa InternalRunSpeechGeneration
  // https://ai.google.dev/gemini-api/docs/speech-generation
  Model := 'gemini-2.5-flash-preview-tts';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',       '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',     '[cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active',     'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Voice',           'Sol=Kore,Gustavo=Puck');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ResponseTimeOut', '36000');

  Model := 'gemini-2.5-pro-preview-tts';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',       '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',     '[cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active',     'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Voice',           'Sol=Kore,Gustavo=Puck');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ResponseTimeOut', '36000');

  // ------- Generacion de imagenes via completions ------
  // cap_GenImage en ModelCaps y SessionCaps: InternalRunCompletions + responseModalities=[IMAGE]
  // https://ai.google.dev/gemini-api/docs/image-generation
  // Nota: 'gemini-2.5-flash-image-preview' (nombre anterior, devuelve 404 en v1beta)
  Model := 'gemini-2.5-flash-image';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',   '[cap_Image, cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps', '[cap_Image, cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active', 'False');

  // ------- Gemini 3 Pro Image (Nano Banana Pro) -- generacion de imagenes avanzada ------
  // https://ai.google.dev/gemini-api/docs/image-generation
  // Nota: no soporta ThinkingLevel (INVALID_ARGUMENT si se envía)
  Model := 'gemini-3-pro-image-preview';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',   '[cap_Image, cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps', '[cap_Image, cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active', 'False');

  // ------- Generacion de video (Veo) ------
  // ModelCaps=[cap_Image]: acepta imagen de entrada
  // SessionCaps agrega cap_GenVideo: Gap activa InternalRunImageVideoGeneration
  // https://ai.google.dev/gemini-api/docs/video
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_veo-2.0-generate-001', 'veo-2.0-generate-001');
  Model := 'aa_veo-2.0-generate-001';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',    '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',  '[cap_Image, cap_GenVideo]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False');

  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_veo-3.0-generate-preview', 'veo-3.0-generate-preview');
  Model := 'aa_veo-3.0-generate-preview';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',    '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',  '[cap_Image, cap_GenVideo]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False');

  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_veo-3.1-generate-preview', 'veo-3.1-generate-preview');
  Model := 'aa_veo-3.1-generate-preview';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',    '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',  '[cap_Image, cap_GenVideo]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False');

  // ------- Perfiles personalizados (aa_*) ------
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-pro-pdf', 'gemini-2.5-pro');
  Model := 'aa_gemini-2.5-pro-pdf';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',   '[cap_Image, cap_Pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps', '[cap_Image, cap_Pdf]');

  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-flash-pdf', 'gemini-2.5-flash');
  Model := 'aa_gemini-2.5-flash-pdf';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',   '[cap_Image, cap_Pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps', '[cap_Image, cap_Pdf]');

  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-flash-code-interpreter', 'gemini-2.5-flash');
  Model := 'aa_gemini-2.5-flash-code-interpreter';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Pdf, cap_CodeInterpreter, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Pdf, cap_CodeInterpreter, cap_WebSearch]');

  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-flash-web-search', 'gemini-2.5-flash');
  Model := 'aa_gemini-2.5-flash-web-search';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',   '[cap_Image, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps', '[cap_Image, cap_WebSearch]');

  // Gemini 3 Pro: thinking bajo (respuesta rapida, bajo costo de tokens de razonamiento)
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-3-pro-fast', 'gemini-3-pro-preview');
  Model := 'aa_gemini-3-pro-fast';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ThinkingLevel', 'tlLow');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');

  // Gemini 3.1 Pro: thinking bajo (respuesta rapida)
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-3.1-pro-fast', 'gemini-3.1-pro-preview');
  Model := 'aa_gemini-3.1-pro-fast';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ThinkingLevel', 'tlLow');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ModelCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'SessionCaps',
    '[cap_Image, cap_Audio, cap_Video, cap_Pdf, cap_WebSearch]');

  // ------------------------- GROQ ----------------------------------
  // https://console.groq.com/docs/models
  // Ultima actualizacion: Feb 2026
  // ------------------------- GROQ ----------------------------------

  // --- Valores globales por defecto para todos los modelos Groq ---
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Max_Tokens',  '8192');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'SessionCaps', '[]');

  // ------- Modelos de produccion -- texto/chat ------
  // llama-3.1-8b-instant: 131K ctx, 131K output, ultra-rapido
  Model := 'llama-3.1-8b-instant';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens', '32768');

  // llama-3.3-70b-versatile: 131K ctx, 32K output, alta calidad
  Model := 'llama-3.3-70b-versatile';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens', '32768');

  // gpt-oss-20b: 131K ctx, 65K output, 1000 T/seg
  Model := 'openai/gpt-oss-20b';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens', '65536');

  // gpt-oss-120b: 131K ctx, 65K output, 500 T/seg
  Model := 'openai/gpt-oss-120b';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens', '65536');

  // ------- Modelos de razonamiento (preview) ------
  // qwen-3-32b: 131K ctx, razonamiento nativo
  Model := 'qwen/qwen-3-32b';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens',    '32768');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ThinkingLevel', 'tlMedium');

  // deepseek-r1-distill-llama-70b: 128K ctx, CoT reasoning
  Model := 'deepseek-r1-distill-llama-70b';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens',    '32768');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Modelos con vision (preview) ------
  // llama-4-scout: 131K ctx, vision + tools, eficiente
  Model := 'meta-llama/llama-4-scout-17b-16e-instruct';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens',  '8192');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_Image]');

  // llama-4-maverick: 131K ctx, vision + tools, mas potente
  Model := 'meta-llama/llama-4-maverick-17b-128e-instruct';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens',  '8192');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_Image]');

  // kimi-k2: 131K ctx, agentic, alta capacidad de tools
  Model := 'moonshotai/kimi-k2-instruct';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens', '16384');

  // ------- Sistemas agentes con herramientas integradas ------
  // Compound: web search + code execution nativo; Gap=[] -> InternalRunCompletions
  // https://console.groq.com/docs/agentic-tooling/compound-beta
  Model := 'compound-beta';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens',  '8192');

  Model := 'compound-beta-mini';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_WebSearch, cap_CodeInterpreter]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Max_Tokens',  '8192');

  // ------- Audio STT -- cap_Audio: procesa audio nativo ------
  // Usar con ChatMode = cmTranscription
  // https://console.groq.com/docs/speech-text
  Model := 'whisper-large-v3';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');

  Model := 'whisper-large-v3-turbo';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');

  // ------- Audio TTS -- ModelCaps=[]: usa endpoint TTS dedicado ------
  // Gap=[cap_GenAudio] activa InternalRunSpeechGeneration
  // https://console.groq.com/docs/text-to-speech
  // Orpheus TTS (Canopy Labs) -- reemplaza playai-tts (decommissioned dic 2025)
  // https://console.groq.com/docs/text-to-speech/orpheus
  Model := 'canopylabs/orpheus-v1-english';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Voice',       'austin');  // Voces Groq Orpheus: autumn, diana, hannah, austin, daniel, troy

  // Arabic TTS (Orpheus)
  Model := 'canopylabs/orpheus-arabic-saudi';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Voice',       'lulwa');  // Voces Groq Orpheus Arabic: fahad, sultan, lulwa, noura

  // Legacy (decommissioned dic 2025 - mantenidos por compatibilidad)
  Model := 'playai-tts';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Voice',       'Arista-PlayAI');

  Model := 'playai-tts-arabic';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'SessionCaps', '[cap_GenAudio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Voice',       'Ahmad-PlayAI');

  // ===========================================================================
  // CLAUDE — CONFIGURACION GLOBAL
  // Fuente: https://platform.claude.com/docs/en/about-claude/models/overview
  // Todos los modelos actuales soportan de forma nativa:
  //   - Vision (imagenes + PDF)
  //   - Extended Thinking (razonamiento CoT)
  //   - Web Search (via tool nativa de Anthropic)
  //   - Tool calling
  // Contexto estandar: 200K tokens
  // Contexto extendido (1M, beta): Opus 4.6, Sonnet 4.6, Sonnet 4.5, Sonnet 4
  //   Requiere header: context-1m-2025-08-07
  //
  // ModelCaps  = capacidades nativas del modelo (SetModelCaps sincroniza NativeInputFiles + ChatMediaSupports)
  // SessionCaps = capacidades deseadas en la sesion (SetSessionCaps sincroniza NativeOutputFiles + EnabledFeatures)
  //   Gap = SessionCaps - ModelCaps -> determina que bridges/tools se activan en RunNew
  //   Sin gap (SessionCaps = ModelCaps) -> llamada directa a InternalRunCompletions
  // ===========================================================================
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Asynchronous', 'False');

  // ===========================================================================
  // CLAUDE HAIKU 4.5  |  Alias: claude-haiku-4-5
  // El mas rapido, inteligencia cercana a frontera
  // Contexto:     200K tokens
  // Max output:   64K tokens
  // Thinking:     Extended thinking (SIN Adaptive thinking)
  // Precio:       $1 / $5 MTok (input / output)
  // Cutoff datos: Jul 2025 (conocimiento fiable: Feb 2025)
  // ===========================================================================
  Model := 'claude-haiku-4-5-20251001';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // CLAUDE SONNET 4  |  Alias: claude-sonnet-4-0  [Legacy]
  // Primer Sonnet de la familia Claude 4
  // Contexto:     200K tokens (1M tokens beta disponible)
  // Max output:   64K tokens
  // Thinking:     Extended thinking + Adaptive thinking
  // Precio:       $3 / $15 MTok
  // Cutoff datos: Mar 2025 (conocimiento fiable: Ene 2025)
  // ===========================================================================
  Model := 'claude-sonnet-4-20250514';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // CLAUDE OPUS 4  |  Alias: claude-opus-4-0  [Legacy]
  // Primer Opus de la familia Claude 4 — muy capaz en agentes y codigo
  // Contexto:     200K tokens
  // Max output:   32K tokens
  // Thinking:     Extended thinking + Adaptive thinking
  // Precio:       $15 / $75 MTok
  // Cutoff datos: Mar 2025 (conocimiento fiable: Ene 2025)
  // ===========================================================================
  Model := 'claude-opus-4-20250514';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // CLAUDE OPUS 4.1  |  Alias: claude-opus-4-1  [Legacy]
  // Contexto:     200K tokens
  // Max output:   32K tokens
  // Thinking:     Extended thinking + Adaptive thinking
  // Precio:       $15 / $75 MTok  (modelo de alto coste)
  // Cutoff datos: Mar 2025 (conocimiento fiable: Ene 2025)
  // ===========================================================================
  Model := 'claude-opus-4-1-20250805';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // CLAUDE SONNET 4.5  |  Alias: claude-sonnet-4-5  [Legacy]
  // Equilibrio velocidad / inteligencia — generacion 4.5
  // Contexto:     200K tokens (1M tokens beta disponible)
  // Max output:   64K tokens
  // Thinking:     Extended thinking + Adaptive thinking
  // Precio:       $3 / $15 MTok
  // Cutoff datos: Jul 2025 (conocimiento fiable: Ene 2025)
  // ===========================================================================
  Model := 'claude-sonnet-4-5-20250929';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // CLAUDE OPUS 4.5  |  Alias: claude-opus-4-5  [Legacy]
  // Mejor en codificacion y tareas de agente complejas — generacion 4.5
  // Contexto:     200K tokens
  // Max output:   64K tokens
  // Thinking:     Extended thinking + Adaptive thinking
  // Precio:       $5 / $25 MTok
  // Cutoff datos: Ago 2025 (conocimiento fiable: May 2025)
  // ===========================================================================
  Model := 'claude-opus-4-5-20251101';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '16000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // CLAUDE SONNET 4.6  |  Alias: claude-sonnet-4-6  [MODELO ACTUAL]
  // Mejor equilibrio velocidad / inteligencia de la familia 4.6
  // Contexto:     200K tokens (1M tokens beta disponible)
  // Max output:   64K tokens
  // Thinking:     Extended thinking + Adaptive thinking
  // Precio:       $3 / $15 MTok
  // Cutoff datos: Ene 2026 (conocimiento fiable: Ago 2025)  <- cutoff mas reciente
  // ===========================================================================
  Model := 'claude-sonnet-4-6';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '32000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // CLAUDE OPUS 4.6  |  Alias: claude-opus-4-6  [MODELO ACTUAL — MAS INTELIGENTE]
  // El modelo mas inteligente de Anthropic para agentes complejos y codigo
  // Contexto:     200K tokens (1M tokens beta disponible)
  // Max output:   128K tokens  <- unico modelo con salida de 128K
  // Thinking:     Extended thinking + Adaptive thinking
  // Precio:       $5 / $25 MTok
  // Cutoff datos: Ago 2025 (conocimiento fiable: May 2025)
  // ===========================================================================
  Model := 'claude-opus-4-6';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '32000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // ===========================================================================
  // PERFILES PERSONALIZADOS CLAUDE (aa_*)
  // Basados en los modelos actuales de la familia 4.6
  //
  // ModelCaps  = lo que el perfil declara como nativo del modelo
  // SessionCaps = lo que se quiere en la sesion (gap activa bridges automaticos)
  // ===========================================================================
  var BaseName: string;

  // 1. Sonnet 4.6 — Thinking profundo (equilibrio coste/razonamiento)
  //    Gap = [] -> InternalRunCompletions con ThinkingLevel=tlHigh activo
  BaseName := 'aa_claude-sonnet-4-6-thinking';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, 'claude-sonnet-4-6');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ThinkingLevel', 'tlHigh');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Max_Tokens', '32000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // 2. Opus 4.6 — Thinking profundo (maxima capacidad de razonamiento)
  //    Gap = [] -> InternalRunCompletions con ThinkingLevel=tlHigh activo
  BaseName := 'aa_claude-opus-4-6-thinking';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, 'claude-opus-4-6');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ThinkingLevel', 'tlHigh');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Max_Tokens', '64000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');

  // 3. Opus 4.6 — Agentes (tool calling + TextEditor + Shell habilitados)
  //    Gap = [] -> InternalRunCompletions con Tool_Active=True
  //    cap_TextEditor y cap_Shell en ModelCaps habilitan esos tools via ChatMediaSupports
  BaseName := 'aa_claude-opus-4-6-agent';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, 'claude-opus-4-6');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Max_Tokens', '32000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch, cap_TextEditor, cap_Shell]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch, cap_TextEditor, cap_Shell]');

  // 4. Haiku 4.5 — Rapido para clasificacion y tools sencillas (bajo coste)
  //    Gap = [] -> InternalRunCompletions con Tool_Active=True
  BaseName := 'aa_claude-haiku-4-5-tools';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, 'claude-haiku-4-5-20251001');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ModelCaps',  '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'SessionCaps', '[cap_Image, cap_Pdf, cap_Reasoning, cap_WebSearch]');


  // ------------------------- MISTRAL ----------------------------------
  // https://docs.mistral.ai/getting-started/models
  // Ultima actualizacion: Feb 2026
  // ------------------------- MISTRAL ----------------------------------

  // --- Valores globales por defecto para todos los modelos Mistral ---
  // La mayoria soporta vision nativa -> ModelCaps=[cap_Image] por defecto
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Max_Tokens',  '16000');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'SessionCaps', '[cap_Image]');

  // ------- Modelos frontier/generalistas (vision + tools) ------
  // https://docs.mistral.ai/getting-started/models/overview
  // mistral-large-latest -> Mistral Large 3 (v25.12), 256K ctx
  // Hereda defaults: ModelCaps=[cap_Image], Tool_Active=True

  // mistral-medium-latest -> Mistral Medium 3.1 (v25.08), 128K ctx
  // Hereda defaults

  // mistral-small-latest -> Mistral Small 3.2 (v25.06), 128K ctx
  // Hereda defaults

  // ------- Ministral (edge models) (vision + tools) ------
  // ministral-14b-latest -> 256K ctx
  // ministral-8b-latest  -> 128K ctx
  // ministral-3b-latest  -> 128K ctx
  // Todos heredan defaults: ModelCaps=[cap_Image], Tool_Active=True

  // ------- Razonamiento: Magistral (NO vision) ------
  // https://docs.mistral.ai/getting-started/models/reasoning
  Model := 'magistral-medium-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ThinkingLevel', 'tlMedium');

  TAiChatFactory.Instance.RegisterCustomModel('Mistral', 'aa_magistral-medium-high', 'magistral-medium-latest');
  Model := 'aa_magistral-medium-high';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ThinkingLevel', 'tlHigh');

  Model := 'magistral-small-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Codigo: Devstral + Codestral ------
  // devstral-latest -> Devstral 2 (v25.12), 256K ctx, agente de codigo, tools
  Model := 'devstral-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps', '[]');

  // devstral-small-latest -> Devstral Small 2, 24B, 256K ctx
  Model := 'devstral-small-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps', '[]');

  // codestral-latest -> 256K ctx, code completion, sin tools
  Model := 'codestral-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'False');

  // ------- Vision dedicada: Pixtral ------
  // pixtral-12b-latest -> 128K ctx, vision + tools
  // Hereda defaults: ModelCaps=[cap_Image], Tool_Active=True

  // pixtral-large-latest -> 131K ctx, vision + tools (frontier multimodal)
  // Hereda defaults

  // ------- Audio STT: Voxtral ------
  // cap_Audio: procesa audio nativo; usar con ChatMode=cmTranscription
  // https://docs.mistral.ai/capabilities/audio
  Model := 'voxtral-mini-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',   '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps', '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'False');

  Model := 'voxtral-small-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',   '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps', '[cap_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'False');

  // ------- OCR: mistral-ocr ------
  // ModelCaps=[] intencionalmente: crea Gap=[cap_Pdf] para que RunNew active la Fase 1
  // (InternalRunPDFDescription → endpoint /v1/ocr). TAiMistralChat.InternalRunCompletions
  // detecta el modelo OCR-only y omite la Fase 3 (chat completions no soportado).
  // https://docs.mistral.ai/capabilities/document
  Model := 'mistral-ocr-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps', '[cap_Pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'False');

  TAiChatFactory.Instance.RegisterCustomModel('Mistral', 'aa_mistral-ocr-pdf', 'mistral-ocr-latest');
  Model := 'aa_mistral-ocr-pdf';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'SessionCaps', '[cap_Pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'False');

  // ------------------------- GROK ----------------------------------
  // https://docs.x.ai/developers/models
  // Ultima actualizacion: Feb 2026
  // ------------------------- GROK ----------------------------------

  // --- Valores globales por defecto para todos los modelos Grok ---
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Max_Tokens',  '16000');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'SessionCaps', '[]');

  // ------- Grok 3 (131K ctx, texto + tools) ------
  // grok-3: hereda defaults, no necesita config adicional

  // ------- Grok 3 Mini (131K ctx, texto + reasoning + tools) ------
  Model := 'grok-3-mini';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ThinkingLevel', 'tlLow');

  // ------- Grok 4-0709 (256K ctx, vision + reasoning + tools) ------
  Model := 'grok-4-0709';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_Image, cap_Reasoning]');

  // ------- Grok 4 Fast (2M ctx, vision + tools) ------
  // https://docs.x.ai/developers/models#grok-4-fast
  Model := 'grok-4-fast-non-reasoning';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_Image]');

  Model := 'grok-4-fast-reasoning';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Grok 4.1 Fast (2M ctx, vision + tools) ------
  Model := 'grok-4-1-fast-non-reasoning';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_Image]');

  Model := 'grok-4-1-fast-reasoning';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_Image, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Grok Code Fast (256K ctx, codigo + reasoning + tools, sin vision) ------
  Model := 'grok-code-fast-1';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Generacion de imagenes ------
  // ModelCaps=[]: usa endpoint dedicado; Gap=[cap_GenImage] activa InternalRunImageGeneration
  Model := 'grok-2-image-1212';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');

  Model := 'grok-imagine-image';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');

  Model := 'grok-imagine-image-pro';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_GenImage]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');

  // ------- Generacion de video ------
  // Gap=[cap_GenVideo] activa InternalRunImageVideoGeneration
  Model := 'grok-imagine-video';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_GenVideo]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');

  // ------- Perfiles personalizados (aa_*) ------
  // Grok con web search nativo (Agent Tools API - solo grok-4 family)
  // xAI solo soporta server-side tools en modelos grok-4+
  TAiChatFactory.Instance.RegisterCustomModel('Grok', 'aa_grok-3-search', 'grok-4-1-fast-reasoning');
  Model := 'aa_grok-3-search';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ModelCaps',   '[cap_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'SessionCaps', '[cap_WebSearch]');

  // Grok 4 Fast con reasoning alto
  TAiChatFactory.Instance.RegisterCustomModel('Grok', 'aa_grok-4-fast-high', 'grok-4-fast-reasoning');
  Model := 'aa_grok-4-fast-high';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ThinkingLevel', 'tlHigh');

  // ------------------------- COHERE ----------------------------------
  // https://docs.cohere.com/docs/models
  // Ultima actualizacion: Feb 2026
  // ------------------------- COHERE ----------------------------------

  // --- Valores globales por defecto para todos los modelos Cohere ---
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'Max_Tokens',  '8000');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'SessionCaps', '[]');

  // ------- Command A (256K ctx, texto + tools) ------
  // command-a-03-2025: hereda defaults globales

  // ------- Command A Reasoning (256K ctx, texto + tools + reasoning) ------
  Model := 'command-a-reasoning-08-2025';
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Command A Vision (128K ctx, vision, sin tools) ------
  Model := 'command-a-vision-07-2025';
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'Tool_Active', 'False');

  // ------- Command A Translate (16K ctx, traduccion especializada, sin tools) ------
  Model := 'command-a-translate-08-2025';
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'Tool_Active', 'False');

  // ------- Command R7B (128K ctx, texto + tools, ligero) ------
  // command-r7b-12-2024: hereda defaults globales

  // ------- Aya Expanse (texto multilingual, sin tools) ------
  Model := 'c4ai-aya-expanse-8b';
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'Max_Tokens',  '4000');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'Tool_Active', 'False');

  Model := 'c4ai-aya-expanse-32b';
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'Tool_Active', 'False');

  // ------- Aya Vision (vision multilingual, sin tools) ------
  Model := 'c4ai-aya-vision-8b';
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'Tool_Active', 'False');

  Model := 'c4ai-aya-vision-32b';
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', Model, 'Tool_Active', 'False');

  // ------------------------- DEEPSEEK ----------------------------------
  // https://api-docs.deepseek.com/quick_start/pricing
  // Ultima actualizacion: Feb 2026
  // ------------------------- DEEPSEEK ----------------------------------

  // --- Valores globales por defecto para todos los modelos DeepSeek ---
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Max_Tokens',  '8192');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'SessionCaps', '[]');

  // ------- DeepSeek Chat / V3.2 (128K ctx, texto + tools) ------
  // deepseek-chat: hereda defaults globales

  // ------- DeepSeek Reasoner / R1 (128K ctx, texto + reasoning + tools) ------
  Model := 'deepseek-reasoner';
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', Model, 'ThinkingLevel', 'tlMedium');

  // ------------------------- KIMI ----------------------------------
  // https://platform.moonshot.ai/docs/api/chat
  // Ultima actualizacion: Feb 2026
  // ------------------------- KIMI ----------------------------------

  // --- Valores globales por defecto para todos los modelos Kimi ---
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'Max_Tokens',  '16000');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'Temperature', '1');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'top_p', '0.95');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'SessionCaps', '[]');

  // ------- Kimi K2 (256K ctx, texto + tools) ------
  // kimi-k2: hereda defaults globales

  // ------- Kimi K2.5 (256K ctx, vision + PDF + reasoning + tools) ------
  Model := 'kimi-k2.5';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'ModelCaps',    '[cap_Image, cap_Pdf, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'SessionCaps',  '[cap_Image, cap_Pdf, cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Kimi K2 Thinking (256K ctx, reasoning + tools, sin vision) ------
  Model := 'kimi-k2-thinking';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Moonshot V1 Legacy (texto puro, sin tools) ------
  Model := 'moonshot-v1-8k';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Max_Tokens',  '4096');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Tool_Active', 'False');

  Model := 'moonshot-v1-32k';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Max_Tokens',  '16000');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Tool_Active', 'False');

  Model := 'moonshot-v1-128k';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Max_Tokens',  '65536');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Tool_Active', 'False');

  // ------- Moonshot V1 Vision Preview (vision, sin tools) ------
  Model := 'moonshot-v1-8k-vision-preview';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Max_Tokens',  '4096');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Tool_Active', 'False');

  Model := 'moonshot-v1-32k-vision-preview';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Max_Tokens',  '16000');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Tool_Active', 'False');

  Model := 'moonshot-v1-128k-vision-preview';
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Max_Tokens',  '65536');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', Model, 'Tool_Active', 'False');

  // ------------------------- LMSTUDIO ----------------------------------
  // https://lmstudio.ai
  // Ultima actualizacion: Feb 2026
  // Inferencia local OpenAI-compatible (http://127.0.0.1:1234/v1/)
  // ------------------------- LMSTUDIO ----------------------------------

  // --- Valores globales (conservadores: texto, sin tools) ---
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'Max_Tokens',  '4096');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'SessionCaps', '[]');

  // ------- Llama 3.3 70B (texto + tools) ------
  Model := 'llama-3.3-70b-instruct';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'Tool_Active', 'True');

  // ------- Qwen 2.5 7B (texto + tools) ------
  Model := 'qwen2.5-7b-instruct';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'Tool_Active', 'True');

  // ------- DeepSeek R1 7B (reasoning) ------
  Model := 'deepseek-r1-7b';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'ModelCaps',    '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'SessionCaps',  '[cap_Reasoning]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'ThinkingLevel', 'tlMedium');

  // ------- Llama 3.2 11B Vision (vision) ------
  Model := 'llama-3.2-11b-vision-instruct';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'SessionCaps', '[cap_Image]');

  // ------- Mistral 7B (texto + tools) ------
  Model := 'mistral-7b-instruct-v0.3';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'Tool_Active', 'True');

  // ------- Gemma 3 (vision + tools, tamanios: 4b/12b/27b) ------
  // En LM Studio el ID es el nombre del modelo o alias configurado en la app
  Model := 'gemma-3-4b-it';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'Tool_Active', 'True');

  Model := 'gemma-3-12b-it';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'Tool_Active', 'True');

  Model := 'gemma-3-27b-it';
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'ModelCaps',   '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'SessionCaps', '[cap_Image]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', Model, 'Tool_Active', 'True');

  // ------------------------- GENERICLLM ----------------------------------
  // Driver para cualquier API compatible con OpenAI
  // Ultima actualizacion: Feb 2026
  // Configurar: DriverName='GenericLLM', URL='http://host/v1/', Model='model-name'
  // ------------------------- GENERICLLM ----------------------------------

  // --- Valores globales conservadores (el usuario configura todo manualmente) ---
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'Max_Tokens',  '4096');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'ModelCaps',   '[]');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'SessionCaps', '[]');

End;

Procedure InitEmbeddingModels;
Begin
  // OpenAI - modelos adicionales con dimensiones espec?ficas
  TAiEmbeddingFactory.Instance.RegisterUserParam('OpenAi', 'text-embedding-3-large', 'Dimensions', '3072');
  TAiEmbeddingFactory.Instance.RegisterUserParam('OpenAi', 'text-embedding-ada-002', 'Dimensions', '1536');

  // Ollama - modelos comunes de embeddings
  TAiEmbeddingFactory.Instance.RegisterUserParam('Ollama', 'nomic-embed-text', 'Dimensions', '768');
  TAiEmbeddingFactory.Instance.RegisterUserParam('Ollama', 'mxbai-embed-large', 'Dimensions', '1024');

  // Gemini - modelo alternativo
  TAiEmbeddingFactory.Instance.RegisterUserParam('Gemini', 'text-embedding-004', 'Dimensions', '768');

  // Cohere - modelo multilingual
  TAiEmbeddingFactory.Instance.RegisterUserParam('Cohere', 'embed-multilingual-v3.0', 'Dimensions', '1024');
End;

Initialization

InitChatModels;
InitEmbeddingModels;

end.
