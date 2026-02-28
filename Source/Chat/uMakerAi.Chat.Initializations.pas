// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Drivers registrados:
//   - GenericLLM (OpenAI-compatible local)
//   - Ollama     (API nativa /api/chat)
//   - LMStudio   (OpenAI-compatible local, 127.0.0.1:1234)
//   - Groq       (OpenAI-compatible, reasoning_format/effort)
//   - Kimi       (OpenAI-compatible, moonshot.ai)
//   - Gemini     (Google, API propia generateContent)
//   - Claude     (Anthropic, API propia x-api-key + anthropic-beta)
//   - OpenAi     (OpenAI Responses API /v1/responses)
//   - DeepSeek   (OpenAI-compatible, reasoning_content para deepseek-reasoner)
//   - Mistral    (Mistral AI, OCR, Document QnA, Magistral reasoning)
//   - Grok       (xAI Grok, reasoning_format/effort, web search, image gen)
// Fase 2 agregara: Cohere, etc.
//
// Uso: incluir este unit en el proyecto consumidor (DPR o LPR).
// El InitChatModels() se llama automaticamente en initialization.

unit uMakerAi.Chat.Initializations;

{$mode objfpc}{$H+}

interface

// Registra todos los modelos/drivers disponibles en TAiChatFactory.
// Se llama automaticamente en la seccion initialization de este unit.
procedure InitChatModels;

implementation

uses
  UMakerAi.ParamsRegistry,
  uMakerAi.Chat.GenericLLM,    // Asegura que el driver se registre
  uMakerAi.Chat.Ollama,        // Driver Ollama nativo
  uMakerAi.Chat.LMStudio,      // LM Studio (OpenAI-compatible local)
  uMakerAi.Chat.Groq,          // Groq (OpenAI-compatible, reasoning)
  uMakerAi.Chat.Kimi,          // Kimi Moonshot AI (OpenAI-compatible)
  uMakerAi.Chat.Gemini,        // Gemini Google (API propia generateContent)
  uMakerAi.Chat.Claude,        // Claude Anthropic (x-api-key + anthropic-beta)
  uMakerAi.Chat.OpenAi,        // OpenAI Responses API (/v1/responses)
  uMakerAi.Chat.DeepSeek,      // DeepSeek (OpenAI-compat + reasoning_content)
  uMakerAi.Chat.Mistral,       // Mistral AI (OCR, Document QnA, Magistral reasoning)
  uMakerAi.Chat.Grok,          // xAI Grok (reasoning, web search, image gen)
  uMakerAi.Chat.Cohere;        // Cohere v2 (chat, rerank, citations, documents)

procedure InitChatModels;
begin
  // GenericLLM — defaults conservadores para cualquier endpoint OpenAI-compatible
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'Temperature',  '1');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('GenericLLM', 'SessionCaps',  '[]');

  // Ollama — API nativa, modelos locales
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'SessionCaps',  '[]');

  // LM Studio — endpoint OpenAI-compatible local (127.0.0.1:1234)
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('LMStudio', 'SessionCaps',  '[]');

  // Groq — OpenAI-compatible, agrega reasoning_format y reasoning_effort
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'SessionCaps',  '[]');

  // Kimi — Moonshot AI OpenAI-compatible (moonshot.ai)
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Kimi', 'SessionCaps',  '[]');

  // Gemini — Google API propia (generateContent / streamGenerateContent)
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Max_Tokens',   '8192');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Temperature',  '1.0');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'SessionCaps',  '[]');

  // Claude — Anthropic API propia (x-api-key + anthropic-beta + anthropic-version)
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'ApiKey',       '@CLAUDE_API_KEY');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Temperature',  '1.0');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'SessionCaps',  '[]');

  // OpenAi — Responses API (https://api.openai.com/v1/responses)
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'ApiKey',       '@OPENAI_API_KEY');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Temperature',  '1.0');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'SessionCaps',  '[]');

  // DeepSeek — OpenAI-compatible + reasoning_content (deepseek-reasoner)
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'ApiKey',       '@DEEPSEEK_API_KEY');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Temperature',  '1.0');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'SessionCaps',  '[]');

  // Mistral — API propia (OCR, Document QnA, Magistral reasoning con content-array)
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'ApiKey',       '@MISTRAL_API_KEY');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'SessionCaps',  '[]');

  // Grok — xAI (reasoning_format/effort raiz, web search via /v1/responses, image gen)
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'ApiKey',       '@GROK_API_KEY');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Temperature',  '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'SessionCaps',  '[]');

  // Cohere — v2 API (chat, rerank, citations, documents RAG)
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'ApiKey',       '@COHERE_API_KEY');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'Max_Tokens',   '4096');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'Temperature',  '0.3');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'Tool_Active',  'False');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'ModelCaps',    '[]');
  TAiChatFactory.Instance.RegisterUserParam('Cohere', 'SessionCaps',  '[]');
end;

initialization
  InitChatModels;

end.
