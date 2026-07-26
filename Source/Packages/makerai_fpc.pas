{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit makerai_fpc;

{$warn 5023 off : no warning about unused units}
interface

uses
  uMakerAi.Agents.Attributes, uMakerAi.Agents.Checkpoint,
  uMakerAi.Agents.EngineRegistry, uMakerAi.Agents.GraphBuilder,
  uMakerAi.Agents, uMakerAi.Agents.Tools.Approval,
  uMakerAi.Chat.AiConnection, uMakerAi.Chat.Claude, uMakerAi.Chat.Cohere,
  uMakerAi.Chat.DeepSeek, uMakerAi.Chat.Gemini, uMakerAi.Chat.GenericLLM,
  uMakerAi.Chat.Grok, uMakerAi.Chat.Groq, uMakerAi.Chat.Initializations,
  uMakerAi.Chat.Kimi, uMakerAi.Chat.LMStudio, uMakerAi.Chat.Mistral,
  uMakerAi.Chat.Ollama, uMakerAi.Chat.OpenAi, EncdDecd,
  uMakerAi.Chat.Messages, uMakerAi.Chat, uMakerAi.Chat.Tools, uMakerAi.Core,
  uMakerAi.Embeddings.Core, uMakerAi.Embeddings,
  uMakerAi.Utils.CodeExtractor, UMakerAi.ParamsRegistry,
  uMakerAi.MCPClient.Core, uMakerAi.MCPServer.Bridge,
  uMakerAi.MCPServer.Core, uMakerAi.MCPServer.Direct,
  uMakerAi.MCPServer.Http, uMakerAi.MCPServer.SSE, uMakerAi.MCPServer.Stdio,
  uMakerAi.RAG.Graph.Core, uMakerAi.RAG.Graph.GQL, uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vectors.Index, uMakerAi.RAG.Vectors, uMakerAi.RAG.Vectors.VQL,
  uMakerAi.Gemini.Speech, uMakerAi.OpenAI.Audio, uMakerAi.OpenAi.Dalle,
  uMakerAi.Tools.ComputerUse, uMakerAi.Tools.Functions, uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor, uMakerAi.Whisper, uMakerAi.Utils.DiffUpdater,
  uMakerAi.Utils.PcmToWav, uMakerAi.Utils.System, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uMakerAi.Agents', @uMakerAi.Agents.Register);
  RegisterUnit('uMakerAi.Chat.Claude', @uMakerAi.Chat.Claude.Register);
  RegisterUnit('uMakerAi.Chat.OpenAi', @uMakerAi.Chat.OpenAi.Register);
  RegisterUnit('uMakerAi.RAG.Graph.Core', @uMakerAi.RAG.Graph.Core.Register);
  RegisterUnit('uMakerAi.Gemini.Speech', @uMakerAi.Gemini.Speech.Register);
  RegisterUnit('uMakerAi.OpenAI.Audio', @uMakerAi.OpenAI.Audio.Register);
  RegisterUnit('uMakerAi.OpenAi.Dalle', @uMakerAi.OpenAi.Dalle.Register);
  RegisterUnit('uMakerAi.Tools.ComputerUse', @uMakerAi.Tools.ComputerUse.Register);
  RegisterUnit('uMakerAi.Tools.Functions', @uMakerAi.Tools.Functions.Register);
  RegisterUnit('uMakerAi.Tools.Shell', @uMakerAi.Tools.Shell.Register);
  RegisterUnit('uMakerAi.Tools.TextEditor', @uMakerAi.Tools.TextEditor.Register);
  RegisterUnit('uMakerAi.Whisper', @uMakerAi.Whisper.Register);
end;

initialization
  RegisterPackage('makerai_fpc', @Register);
end.