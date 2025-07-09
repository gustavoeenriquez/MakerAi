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
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Temperature', '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Max_Tokens', '4096');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'NativeOutupFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', 'Tool_Active', 'False');

  // ------- OLLAMA Modelo deepseek-r1:8b ------------------------------
  Model := 'deepseek-r1:8b';

  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Asynchronous', 'True');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'InitialInstructions',
    'Eres un asistente de IA ligero y rápido  llamado PENSANTE. Proporciona respuestas concisas y directas.');

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
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Asynchronous', 'True');

  // ------- OLLAMA Modelo gemma3n:e4b ------------------------------
  Model := 'gemma3n:e4b';

  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'ChatMediaSupports', '[Tcm_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeInputFiles', '[Tfc_Image]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Ollama', Model, 'Tool_Active', 'False');

  // ------------------------- OPENAI ----------------------------------
  // https://platform.openai.com/docs/guides/text?api-mode=responses
  // ------------------------- OPENAI ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Temperature', '0.7');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Max_Tokens', '4096');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'NativeOutupFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('OpenAi', 'Tool_Active', 'False');

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

  // Falta por implmentar estos si se llega a necesitar {gpt-4.1 gpt-4.1-mini gpt-4.1-nano gpt-4o gpt-4o-mini }

  // ------------------------- GEMINI ----------------------------------
  // https://ai.google.dev/gemini-api/docs/text-generation
  // ------------------------- GEMINI ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Temperature', '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Max_Tokens', '30096');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False');

  // ------- GEMINI Modelo gemini-1.5-pro ------------------------------
  Model := 'gemini-1.5-pro';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'Asynchronous', 'False');

  // ------- GEMINI Modelo gemini-2.5-flash ------------------------------
  Model := 'gemini-2.5-flash';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_textFile]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile]');

  // ------- GEMINI Modelo gemini-2.5-pro ------------------------------
  Model := 'gemini-2.5-pro';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_textFile]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile]');

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


  // ------- GEMINI gemini-2.0-flash-preview-image-generation ------------------------------
  Model := 'gemini-2.0-flash-preview-image-generation';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_image]');

  // ------- GEMINI gemini-2.0-flas-exp-image-generation ------------------------------
  Model := 'gemini-2.0-flash-exp-image-generation';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_image]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[Tcm_image]');

  // ------- GEMINI aqa ------------------------------
  Model := 'aqa';
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'NativeOutputFiles', '[Tfc_video]');
  TAiChatFactory.Instance.RegisterUserParam('Gemini', Model, 'ChatMediaSupports', '[]');

  // ------------------------- GROQ ----------------------------------
  //https://console.groq.com/docs/api-reference#chat-create
  // ------------------------- GROQ ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Temperature', '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Groq', 'Max_Tokens', '4096');
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
  //https://console.groq.com/docs/text-to-speech
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



  // ------------------------- CLAUDE ----------------------------------
  //https://docs.anthropic.com/en/docs/build-with-claude/streaming
  // ------------------------- CLAUDE ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Temperature', '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Max_Tokens', '4096');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', 'Asynchronous', 'False');

  // ------- CLAUDE claude-3-5-haiku-20241022 ------------------------------
  Model := 'claude-3-5-haiku-20241022';
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Claude', Model, 'Tool_Active', 'False');


  // ------------------------- MISTRAL ----------------------------------
  //https://docs.mistral.ai/api/
  // ------------------------- CLAUDE ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Temperature', '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Max_Tokens', '4096');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', 'Asynchronous', 'False');

  // ------- Mistral ministral-8b-latest ------------------------------
  Model := 'ministral-8b-latest';
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Mistral', Model, 'Tool_Active', 'True');


  // ------------------------- GROK ----------------------------------
  //https://docs.x.ai/docs/api-reference#chat-completions
  // ------------------------- GROK ----------------------------------
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Temperature', '0.7');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Max_Tokens', '4096');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'NativeInputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'NativeOutputFiles', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'ChatMediaSupports', '[]');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', 'Asynchronous', 'False');

  // ------- GROK grok-2-image-1212 ------------------------------
  Model := 'grok-2-image-1212';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'NativeOutputFiles', '[tfc_image]'); //Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_textFile
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ChatMediaSupports', '[]');  //Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile

  // ------- GROK grok-2-vision-1212 ------------------------------
  Model := 'grok-2-vision-1212';
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Asynchronous', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'Tool_Active', 'False');
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'NativeInputFiles', '[tfc_image]'); //Tfc_audio, tfc_image, Tfc_Video, Tfc_Document, tfc_textFile
  TAiChatFactory.Instance.RegisterUserParam('Grok', Model, 'ChatMediaSupports', '[tcm_image]');  //Tcm_audio, tcm_image, Tcm_Video, Tcm_Document, tcm_textFile
End;

Initialization

InitChatModels;

end.
