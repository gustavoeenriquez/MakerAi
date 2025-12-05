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
// Nombre: Gustavo Enríquez
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
  System.SysUtils, uMakerAi.ParamsRegistry;

implementation

Procedure InitChatModels;
Var
  Model: String;
Begin


  // ------------------------- OLLAMA ----------------------------------
  // https://notes.kodekloud.com/docs/Running-Local-LLMs-With-Ollama/Building-AI-Applications/Ollama-REST-API-Endpoints
  // ------------------------- OLLAMA ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Tool_Active', 'False');

  // ------- OLLAMA Modelo deepseek-r1:8b ------------------------------
  Model := 'deepseek-r1:8b';

  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'SystemPrompt', 'Eres un asistente de IA ligero y rápido  llamado PENSANTE. Proporciona respuestas concisas y directas.');

  // ------- OLLAMA Modelo bakllava ------------------------------
  Model := 'bakllava:latest';

  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ChatMediaSupports', '[Tcm_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeInputFiles', '[Tfc_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'False');

  // ------- OLLAMA Modelo llava:latest ------------------------------
  Model := 'llava:latest';

  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ChatMediaSupports', '[Tcm_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeInputFiles', '[Tfc_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'False');

  // ------- OLLAMA Modelo qwen2.5vl:7b ------------------------------
  Model := 'qwen2.5vl:7b';

  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ChatMediaSupports', '[Tcm_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeInputFiles', '[Tfc_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'False');

  // ------- OLLAMA Modelo llama3.2:latest ------------------------------
  Model := 'llama3.2:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'False');

  // ------- OLLAMA Modelo phi3:latest ------------------------------
  Model := 'phi3:latest';
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Asynchronous', 'False');

  // ------- OLLAMA Modelo gemma3n:e4b ------------------------------
  Model := 'gemma3n:e4b';

  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ChatMediaSupports', '[Tcm_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeInputFiles', '[Tfc_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'False');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Ollama', 'aa_whisper', 'whisper');



  // ------- OLLAMA Modelo Whisper localhost ------------------------------
  { Model := 'aa_whisper';
    TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Asynchronous', 'False');
    TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeInputFiles', '[Tfc_Audio]');
    TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeOutputFiles', '[]');
    TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ChatMediaSupports', '[]');
    TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Url', 'http://localhost:7890/');
  }

  {
  // ------------------------- OPENAI ----------------------------------
  // https://platform.openai.com/docs/guides/text?api-mode=responses
  // ------------------------- OPENAI ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Temperature', '1');

  // ------- OPENAI Modelo gpt-4o-mini-tts ------------------------------
  Model := 'gpt-4o-mini-tts';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[Tfc_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Voice', 'alloy');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Voice_Format', 'mp3');

  // ------- OPENAI Modelo gpt-4o-search-preview ------------------------------
  Model := 'gpt-4o-search-preview';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[Tfc_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  Model := 'gpt-4o-mini-search-preview';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[Tfc_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- OPENAI Modelo gpt-4o-audio-preview ------------------------------
  // Recibe un .wav de entrada en lugar de texto e interpreta el audio como el comando directamente y salida también
  Model := 'gpt-4o-audio-preview';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[Tfc_audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[Tfc_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  Model := 'gpt-4o-mini-audio-preview';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[Tfc_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[Tfc_Audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- OPENAI Modelo gpt-4o-transcribe ------------------------------
  // Recibe un .wav de entrada en lugar de texto e interpreta el audio como el comando directamente y salida también
  Model := 'gpt-4o-transcribe';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[Tfc_audio]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- OPENAI gpt-image-1 ------------------------------
  Model := 'gpt-image-1';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ResponseTimeOut', '36000');

  // ------- OPENAI dall-e-2 ------------------------------
  Model := 'dall-e-2';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- OPENAI dall-e-3 ------------------------------
  Model := 'dall-e-3';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // ------- OPENAI Modelo o1 ------------------------------
  Model := 'o1';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  // ------- OPENAI Modelo o3 ------------------------------
  Model := 'o3';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  // ------- OPENAI Modelo GPT-5 ------------------------------

  Model := 'gpt-5';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  Model := 'gpt-5-chat:latest';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  Model := 'gpt-5-mini';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  Model := 'gpt-5-nano';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-high', 'gpt-5');
  Model := 'aa-gpt-5-high';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlHigh');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-low', 'gpt-5');
  Model := 'aa-gpt-5-low';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tllow');

  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-chat-high', 'gpt-5-chat:latest');
  Model := 'aa-gpt-5-chat-high';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlHigh');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-chat-low', 'gpt-5-chat:latest');
  Model := 'aa-gpt-5-chat-low';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlLow');

  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-mini-high', 'gpt-5-mini');
  Model := 'aa-gpt-5-mini-high';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlHigh');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-mini-low', 'gpt-5-mini');
  Model := 'aa-gpt-5-mini-low';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlLow');

  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-nano-high', 'gpt-5-nano');
  Model := 'aa-gpt-5-nano-high';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlHigh');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa-gpt-5-nano-low', 'gpt-5-nano');
  Model := 'aa-gpt-5-nano-low';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlLow');

}

  // Falta por implmentar estos si se llega a necesitar {gpt-4.1 gpt-4.1-mini gpt-4.1-nano gpt-4o gpt-4o-mini }

  // ------------------------- OPENAI Responses ----------------------------------
  // https://platform.openai.com/docs/guides/text?api-mode=responses
  // ------------------------- OPENAI Responses  ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Tool_Active', 'False');

  // ------- OPENAI Modelo gpt-4o ------------------------------
  Model := 'gpt-4.1';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_pdf, tcm_image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[tfc_pdf, tcm_image]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_gpt-4.1-pdf', 'gpt-4.1');

  // ------- OPENAI Modelo aa_gpt-4o-pdf ------------------------------
  Model := 'aa_gpt-4.1-pdf';

  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_image, tcm_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeInputFiles', '[tcm_image, tfc_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'Tool_Active', 'False');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_o4-mini-reasoning', '4o-mini');

  // ------- OPENAI Modelo aa_o4-mini-reasoning ------------------------------
  Model := 'aa_o4-mini-reasoning';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ThinkingLevel', 'tlMedium');

  // ------- OPENAI o3-deep-research ------------------------------
  Model := 'o3-deep-research';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_code_interpreter, tcm_WebSearch]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ResponseTimeOut', '72000');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAi', 'aa_o4-mini-code-interpreter', 'gpt-4.1');

  // ------- OPENAI Modelo aa_o4-mini-code-interpreter ------------------------------
  Model := 'aa_o4-mini-code-interpreter';
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', Model, 'ChatMediaSupports', '[tcm_code_interpreter, tcm_WebSearch]');



  // ------- OPENAI Modelo GPT-5 ------------------------------

  Model := 'gpt-5.1';
  TAiChatFactory.Instance.RegisterUserParam('OpenAiRespnses', Model, 'ThinkingLevel', 'tlMedium');


  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAiRespnses', 'aa-gpt-5.1-high', 'gpt-5.1');
  Model := 'aa-gpt-5.1-high';
  TAiChatFactory.Instance.RegisterUserParam('OpenAiRespnses', Model, 'ThinkingLevel', 'tlHigh');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('OpenAiRespnses', 'aa-gpt-5.1-low', 'gpt-5.1');
  Model := 'aa-gpt-5.1-low';
  TAiChatFactory.Instance.RegisterUserParam('OpenAiRespnses', Model, 'ThinkingLevel', 'tlLow');


  // ------------------------- GEMINI ----------------------------------
  // https://ai.google.dev/gemini-api/docs/text-generation
  // ------------------------- GEMINI ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Max_Tokens', '30096');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False');

  // ------- GEMINI Modelo gemini-1.5-pro ------------------------------
  Model := 'gemini-1.5-pro';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False');

  // ------- GEMINI Modelo gemini-2.5-flash ------------------------------
  Model := 'gemini-2.5-flash';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_audio, tfc_image, Tfc_Video, Tfc_pdf, tfc_ExtracttextFile]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_audio, tfc_image, Tfc_Video, Tfc_pdf, tfc_ExtracttextFile]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_audio, tcm_image, Tcm_Video, Tcm_pdf, tcm_textFile]');

  // ------- GEMINI Modelo gemini-2.5-pro ------------------------------
  Model := 'gemini-2.5-pro';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_audio, tfc_image, Tfc_Video, Tfc_pdf, tfc_ExtracttextFile]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_audio, tcm_image, Tcm_Video, Tcm_pdf, tcm_textFile]');

  // ------- GEMINI Modelo gemini-2.5-flash-preview-tts ------------------------------
  Model := 'gemini-2.5-flash-preview-tts';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_audio]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Voice', 'Sol=Kore,Gustavo=Puck');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ResponseTimeOut', '36000');

  // ------- GEMINI gemini-2.5-pro-preview-tts ------------------------------
  Model := 'gemini-2.5-pro-preview-tts';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_audio]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Voice', 'Sol=Kore,Gustavo=Puck');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ResponseTimeOut', '36000');

  // ------- GEMINI gemini-2.5-flash-image-preview ------------------------------
  Model := 'gemini-2.5-flash-image-preview';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_image]');

  // ------- GEMINI gemini-2.0-flash-preview-image-generation ------------------------------
  Model := 'gemini-2.0-flash-preview-image-generation';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInpputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_image]');

  // ------- GEMINI gemini-2.0-flash-exp-image-generation ------------------------------
  Model := 'gemini-2.0-flash-exp-image-generation';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_image]');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_veo-2.0-generate-001', 'veo-2.0-generate-001');

  // ------- GEMINI Modelo veo-2.0-generate-001 ------------------------------
  Model := 'aa_veo-2.0-generate-001';

  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_video]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False'); // Arreglar para que funcione en modo Asincrónico

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_veo-3.0-generate-preview', 'veo-3.0-generate-preview');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-pro-pdf', 'gemini-2.5-pro');

  // ------- GEMINI Modelo gemini-2.5-pro ------------------------------
  Model := 'aa_gemini-2.5-pro-pdf';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_pdf]');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-flash-pdf', 'gemini-2.5-flash');

  // ------- GEMINI Modelo gemini-2.5-flash ------------------------------
  Model := 'aa_gemini-2.5-flash-pdf';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_pdf]');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-flash-code-interpreter', 'gemini-2.5-flash');

  // ------- GEMINI Modelo aa_gemini-2.5-flash-code-interpreter ------------------------------
  Model := 'aa_gemini-2.5-flash-code-interpreter';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[tcm_code_interpreter, tcm_WebSearch]');

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-2.5-flash-web-search', 'gemini-2.5-flash');

  // ------- GEMINI Modelo aa_gemini-2.5-flash-web-search ------------------------------
  Model := 'aa_gemini-2.5-flash-web-search';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[tcm_WebSearch]');



// ===========================================================================
  // CONFIGURACIÓN GEMINI 3 (SERIES 3.0)
  // ===========================================================================

  // ------- GEMINI 3 Pro Preview (Texto y Multimodal General) -------------


  // ------- GEMINI Modelo veo-3.0-generate-preview ------------------------------
  Model := 'aa_veo-3.0-generate-preview';

  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_video]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False'); // Arreglar para que funcione en modo Asincrónico



  Model := 'gemini-3-pro-preview';

  // Capacidades completas de entrada (Audio, Video, PDF, Imágenes)
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_audio, tfc_image, Tfc_Video, Tfc_pdf, tfc_ExtracttextFile]');

  // Salida estándar de texto (Thinking por defecto HIGH en API, aquí configurable)
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ThinkingLevel', 'tlHigh'); // Default recomendado para v3

  // Soporte de herramientas avanzadas
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_audio, tcm_image, Tcm_Video, Tcm_pdf]');

  // Control de resolución de medios (Tokens vs Calidad)
  // Por defecto Medium para balancear costos en PDFs/Video
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'MediaResolution', 'mrMedium');


  // ------- GEMINI 3 Pro Image Preview (Generación y Edición de Imágenes) ---
  Model := 'gemini-3-pro-image-preview';

  // Inputs: Texto (Prompt) e Imágenes (para edición/inpainting)
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_image]');
  // Output: Imágenes generadas
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_image]');

  // Soportes adicionales (Search para grounding visual)
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_image, tcm_WebSearch]');


  // ------- MODELO PERSONALIZADO: Gemini 3 Thinking Low (Rápido) --------
  // Útil para tareas simples donde la latencia importa más que el razonamiento profundo
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-3-pro-fast', 'gemini-3-pro-preview');
  Model := 'aa_gemini-3-pro-fast';

  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ThinkingLevel', 'tlLow');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_audio, tfc_image, Tfc_Video, Tfc_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[tcm_WebSearch]');

  // ------- MODELO PERSONALIZADO: Gemini 3 Thinking Low (Rápido) --------
  // Útil para tareas simples donde la latencia importa más que el razonamiento profundo
  TAiChatFactory.Instance.RegisterCustomModel('Gemini', 'aa_gemini-3-pro-basic', 'gemini-3-pro-preview');
  Model := 'aa_gemini-3-pro-basic';

  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ThinkingLevel', 'tlLow');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[]');


  // ------------------------- GROQ ----------------------------------
  // https://console.groq.com/docs/api-reference#chat-create
  // ------------------------- GROQ ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Asynchronous', 'False');

  // ------- GROQ Modelo whisper-large-v3-turbo ------------------------------
  Model := 'whisper-large-v3-turbo';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'NativeInputFiles', '[Tfc_audio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ChatMediaSupports', '[]');

  // ------- GROQ Modelo whisper-large-v3 ------------------------------
  Model := 'whisper-large-v3';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'NativeInputFiles', '[Tfc_audio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ChatMediaSupports', '[]');

  // ------- GROQ Modelo playai-tts ------------------------------
  // https://console.groq.com/docs/text-to-speech
  Model := 'playai-tts';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'NativeOutputFiles', '[Tfc_audio]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'voice', 'Arista-PlayAI');

  // ------- GROQ llama-3.3-70b-versatile ------------------------------
  Model := 'llama-3.3-70b-versatile';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'True');

  // ------- GROQ llama-3.1-8b-instant ------------------------------
  Model := 'llama-3.1-8b-instant';
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Groq', Model, 'Tool_Active', 'True');





// ------------------------- CLAUDE CONFIGURACIÓN GLOBAL -------------------
  // Valores por defecto para el driver "Claude"
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Asynchronous', 'False'); // Recomendado True para Streaming

  // ===========================================================================
  // CLAUDE 3.5 HAIKU
  // ===========================================================================
  Model := 'claude-3-5-haiku-20241022';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Asynchronous', 'False');
  // Haiku es rápido y barato, ideal para tools simples
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'NativeInputFiles', '[Tfc_Text]');

  // ===========================================================================
  // CLAUDE 3.7 SONNET (Modelo Actualizado)
  // ===========================================================================
  Model := 'claude-3-7-sonnet-20250219';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Asynchronous', 'False');

  // Soporte nativo de Sonnet 3.7 para Imágenes y PDF por defecto
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'NativeInputFiles', '[Tfc_Image, Tfc_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ChatMediaSupports', '[Tcm_Image, tcm_pdf]');

  // --- PERFILES PERSONALIZADOS 3.7 ---

  // 1. Perfil con Web Search
  var BaseName := 'aa_claude-3-7-web-search';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, Model);
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ChatMediaSupports', '[tcm_WebSearch]');

  // 2. Perfil con Code Interpreter
  BaseName := 'aa_claude-3-7-code-interpreter';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, Model);
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ChatMediaSupports', '[tcm_code_interpreter]');

  // 3. Perfil Thinking (Razonamiento) - Configura parámetros automáticamente
  BaseName := 'aa_claude-3-7-thinking-high';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, Model);
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ThinkingLevel', 'tlHigh'); // Activa lógica interna
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Max_Tokens', '20000');

  // ===========================================================================
  // CLAUDE 4.5 HAIKU (Nuevo)
  // ===========================================================================
  Model := 'claude-haiku-4-5-20251001';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '8000');

  // ===========================================================================
  // CLAUDE 4.5 SONNET (Nuevo Flagship)
  // ===========================================================================
  Model := 'claude-sonnet-4-5-20250929';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Max_Tokens', '16000'); // Mayor contexto de salida

  // Capacidades base: Imagen y PDF
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'NativeInputFiles', '[Tfc_Image, Tfc_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'ChatMediaSupports', '[Tcm_Image, tcm_pdf]');

  // --- PERFILES PERSONALIZADOS 4.5 ---

  // 1. Perfil Full Thinking (Deep Reasoning)
  BaseName := 'aa_claude-4-5-deep-think';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, Model);
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ThinkingLevel', 'tlHigh');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'Max_Tokens', '32000'); // Thinking consume mucho token

  // 2. Perfil con Memoria (Novedad)
  BaseName := 'aa_claude-4-5-memory';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, Model);
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'EnableMemory', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ChatMediaSupports', '[tcm_Memory]');

  // 3. Perfil Developer (Code Interpreter + Bash)
  BaseName := 'aa_claude-4-5-developer';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, Model);
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ChatMediaSupports', '[tcm_code_interpreter]');

  // 4. Perfil Web Researcher (Busqueda Web + PDF)
  BaseName := 'aa_claude-4-5-researcher';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, Model);
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'NativeInputFiles', '[Tfc_pdf]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ChatMediaSupports', '[tcm_WebSearch, tcm_pdf]');

 // 5. Perfil Editor de Archivos (String Replace)  //NOTA : Se debe implementar la función str_replace_based_edit_tool.  en Functions
  BaseName := 'aa_claude-4-5-editor';
  TAiChatFactory.Instance.RegisterCustomModel('Claude', BaseName, 'claude-sonnet-4-5-20250929');
  // Habilitamos TextEditor y PDF (para poder leer documentos y editarlos)
  TAiChatFactory.Instance.RegisterUserParam('Claude', BaseName, 'ChatMediaSupports', '[tcm_TextEditor, tcm_pdf]');



  // ------------------------- MISTRAL ----------------------------------
  // https://docs.mistral.ai/api/
  // ------------------------- MISTRAL ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Asynchronous', 'False');

  // ------- Mistral ministral-8b-latest ------------------------------
  Model := 'ministral-8b-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'True');

  // ------- pixtral-12b-latest ------------------------------
  Model := 'pixtral-12b-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'NativeInputFiles', '[tfc_image]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ChatMediaSupports', '[tcm_image]'); // Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile

  // ------- Mistral pixtral-large-latest ------------------------------
  Model := 'pixtral-large-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'NativeInputFiles', '[tfc_image]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ChatMediaSupports', '[tcm_image]'); // Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'NativeOutputFiles', '[tfc_ExtracttextFile]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile

  // ------- mistral-ocr-latest ------------------------------
  Model := 'mistral-ocr-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'NativeInputFiles', '[tfc_pdf]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ChatMediaSupports', '[]'); // Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Mistral', 'aa_mistral-ocr-latest-pdf', 'mistral-ocr-latest');

  // ------- OPENAI Modelo aa_mistral-ocr-latest-pdf ------------------------------
  Model := 'aa_mistral-ocr-latest-pdf';

  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'NativeInputFiles', '[tfc_pdf]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'ChatMediaSupports', '[]'); // Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile

  // ------------------------- GROK ----------------------------------
  // https://docs.x.ai/docs/api-reference#chat-completions
  // ------------------------- GROK ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Asynchronous', 'False');

  // ------- GROK grok-2-image-1212 ------------------------------
  Model := 'grok-2-image-1212';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'NativeOutputFiles', '[tfc_image]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ChatMediaSupports', '[]'); // Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile

  // ------- GROK grok-2-vision-1212 ------------------------------
  Model := 'grok-2-vision-1212';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'NativeInputFiles', '[tfc_image]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ChatMediaSupports', '[tcm_image]'); // Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile

  // Se adiciona un modelo personalizado para permitir el control de los parámetros por defecto
  TAiChatFactory.Instance.RegisterCustomModel('Grok', 'aa-grok-3-search', 'grok-3');

  // ------- GROK grok-3-search ------------------------------
  Model := 'aa-grok-3-search';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'NativeInputFiles', '[]'); // Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_ExtracttextFile
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ChatMediaSupports', '[tcm_WebSearch]'); // Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile


  // ------------------------- DEEPSEEK ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Max_Tokens', '8000');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('DeepSeek', 'Tool_Active', 'False');


End;

Initialization

InitChatModels;

end.
