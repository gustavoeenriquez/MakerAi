{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MakerAI;

{$warn 5023 off : no warning about unused units}
interface

uses
  uMakerAi.MCPClient.Core, uMakerAi.Chat.AiConnection, 
  uMakerAi.Chat.Claude, uMakerAi.Chat.DeepSeek, uMakerAi.Chat.Gemini, 
  uMakerAi.Chat.Grok, uMakerAi.Chat.Groq, uMakerAi.Chat.Initializations, 
  uMakerAi.Chat.Mistral, uMakerAi.Chat.Ollama, uMakerAi.Chat.OpenAi, 
  uMakerAi.Chat.LMStudio, uMakerAi.Chat.Kimi, uMakerAi.Chat.GenericLLM, 
  uMakerAi.Chat.Cohere, uMakerAi.Agents.GraphBuilder, 
  uMakerAi.Agents.DmGenerator, uMakerAi.Chat, uMakerAi.Core, 
  uMakerAi.Embeddings, uMakerAi.Embeddings.Core, uMakerAi.Prompts, 
  uMakerAi.Utils.CodeExtractor, uMakerAi.Utils.PcmToWav, 
  uThreadingHelper, uMakerAi.Whisper, 
  uMakerAi.Tools.Shell, uMakerAi.Tools.TextEditor, uMakerAi.Agents, 
  uMakerAi.Agents.Attributes, uMakerAi.Agents.EngineRegistry, 
  uMakerAi.MCPServer.Http, uMakerAi.MCPServer.Core, uMakerAi.MCPServer.Stdio, 
  uMakerAi.MCPServer.Direct, uMakerAi.Utils.Python, 
  uMakerAi.Utils.VoiceMonitor, uMakerAi.RAG.Graph.Builder, 
  uMakerAi.RAG.Graph.Core, uMakerAi.RAG.Vectors, uMakerAi.Tools.Functions, 
  uMakerAi.MCPServer.SSE, uMakerAi.Tools.ComputerUse, uMakerAi.Chat.Tools, 
  uMakerAi.Gemini.Speech, uMakerAi.Chat.Bridge, uMakerAi.Gemini.WebSearch, 
  uMakerAi.Ollama.Ocr, uMakerAi.Chat.Messages, uMakerAi.RAG.Graph.GQL, 
  uMakerAi.MCPServer.Bridge, uMakerAi.RAG.MetaData, uMakerAi.RAG.Vectors.VQL, 
  uMakerAi.RAG.Vectors.Index, uMakerAi.Gemini.Video, uMakerAi.OpenAI.Sora, 
  uMakerAi.OpenAI.Audio, uMakerAi.OpenAi.Dalle, uMakerAi.Utils.DiffUpdater, 
  uHttpHelper, uJsonHelper, uBase64Helper, uRegularExpressionsHelper, 
  uExpressionHelper, uXMLHelper, uSSEHelper, uEnvHelper, uGenericsHelper, 
  uMakerAi.ParamsRegistry, uRttiHelper, 
  uSysUtilsHelper, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uMakerAi.Chat.AiConnection', 
    @uMakerAi.Chat.AiConnection.Register);
  RegisterUnit('uMakerAi.Chat.Claude', @uMakerAi.Chat.Claude.Register);
  RegisterUnit('uMakerAi.Chat.DeepSeek', @uMakerAi.Chat.DeepSeek.Register);
  RegisterUnit('uMakerAi.Chat.Gemini', @uMakerAi.Chat.Gemini.Register);
  RegisterUnit('uMakerAi.Chat.Grok', @uMakerAi.Chat.Grok.Register);
  RegisterUnit('uMakerAi.Chat.Groq', @uMakerAi.Chat.Groq.Register);
  RegisterUnit('uMakerAi.Chat.Mistral', @uMakerAi.Chat.Mistral.Register);
  RegisterUnit('uMakerAi.Chat.Ollama', @uMakerAi.Chat.Ollama.Register);
  RegisterUnit('uMakerAi.Chat.OpenAi', @uMakerAi.Chat.OpenAi.Register);
  RegisterUnit('uMakerAi.Chat.LMStudio', @uMakerAi.Chat.LMStudio.Register);
  RegisterUnit('uMakerAi.Chat.Kimi', @uMakerAi.Chat.Kimi.Register);
  RegisterUnit('uMakerAi.Chat.GenericLLM', @uMakerAi.Chat.GenericLLM.Register);
  RegisterUnit('uMakerAi.Chat.Cohere', @uMakerAi.Chat.Cohere.Register);
  RegisterUnit('uMakerAi.Prompts', @uMakerAi.Prompts.Register);
  RegisterUnit('uMakerAi.Whisper', @uMakerAi.Whisper.Register);
  RegisterUnit('uMakerAi.Agents', @uMakerAi.Agents.Register);
  RegisterUnit('uMakerAi.MCPServer.Http', @uMakerAi.MCPServer.Http.Register);
  RegisterUnit('uMakerAi.MCPServer.Stdio', @uMakerAi.MCPServer.Stdio.Register);
  RegisterUnit('uMakerAi.MCPServer.Direct', @uMakerAi.MCPServer.Direct.Register
    );
  RegisterUnit('uMakerAi.Utils.VoiceMonitor', 
    @uMakerAi.Utils.VoiceMonitor.Register);
  RegisterUnit('uMakerAi.RAG.Graph.Builder', 
    @uMakerAi.RAG.Graph.Builder.Register);
  RegisterUnit('uMakerAi.RAG.Graph.Core', @uMakerAi.RAG.Graph.Core.Register);
  RegisterUnit('uMakerAi.RAG.Vectors', @uMakerAi.RAG.Vectors.Register);
  RegisterUnit('uMakerAi.Chat.AiConnection', 
    @uMakerAi.Chat.AiConnection.Register);
  RegisterUnit('uMakerAi.Chat.Claude', @uMakerAi.Chat.Claude.Register);
  RegisterUnit('uMakerAi.Chat.DeepSeek', @uMakerAi.Chat.DeepSeek.Register);
  RegisterUnit('uMakerAi.Chat.Gemini', @uMakerAi.Chat.Gemini.Register);
  RegisterUnit('uMakerAi.Chat.Grok', @uMakerAi.Chat.Grok.Register);
  RegisterUnit('uMakerAi.Chat.Groq', @uMakerAi.Chat.Groq.Register);
  RegisterUnit('uMakerAi.Chat.Mistral', @uMakerAi.Chat.Mistral.Register);
  RegisterUnit('uMakerAi.Chat.Ollama', @uMakerAi.Chat.Ollama.Register);
  RegisterUnit('uMakerAi.Chat.OpenAi', @uMakerAi.Chat.OpenAi.Register);
  RegisterUnit('uMakerAi.Chat.LMStudio', @uMakerAi.Chat.LMStudio.Register);
  RegisterUnit('uMakerAi.Chat.Kimi', @uMakerAi.Chat.Kimi.Register);
  RegisterUnit('uMakerAi.Chat.GenericLLM', @uMakerAi.Chat.GenericLLM.Register);
  RegisterUnit('uMakerAi.Chat.Cohere', @uMakerAi.Chat.Cohere.Register);
  RegisterUnit('uMakerAi.Prompts', @uMakerAi.Prompts.Register);
  RegisterUnit('uMakerAi.Whisper', @uMakerAi.Whisper.Register);
  RegisterUnit('uMakerAi.Agents', @uMakerAi.Agents.Register);
  RegisterUnit('uMakerAi.MCPServer.Http', @uMakerAi.MCPServer.Http.Register);
  RegisterUnit('uMakerAi.MCPServer.Stdio', @uMakerAi.MCPServer.Stdio.Register);
  RegisterUnit('uMakerAi.MCPServer.Direct', @uMakerAi.MCPServer.Direct.Register
    );
  RegisterUnit('uMakerAi.Utils.VoiceMonitor', 
    @uMakerAi.Utils.VoiceMonitor.Register);
  RegisterUnit('uMakerAi.RAG.Graph.Builder', 
    @uMakerAi.RAG.Graph.Builder.Register);
  RegisterUnit('uMakerAi.RAG.Graph.Core', @uMakerAi.RAG.Graph.Core.Register);
  RegisterUnit('uMakerAi.RAG.Vectors', @uMakerAi.RAG.Vectors.Register);
end;

initialization
  RegisterPackage('MakerAI', @Register);
end.
