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

// Registra TSelectionEditor para que al soltar un componente MakerAI en un
// formulario se a?adan autom?ticamente las units que el usuario necesita
// (tipos de eventos, tipos base, etc.) sin tener que agregarlas a mano.

unit uAiEditors.RequiresUnits;

interface

uses
  System.Classes, System.SysUtils, DesignIntf, DesignEditors;

procedure Register;

implementation

uses
  // --- Conectores universales ---
  uMakerAi.Chat.AiConnection,
  uMakerAi.Embeddings.Connection,

  // --- Chat drivers ---
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Ollama,
  uMakerAi.Chat.Groq,
  uMakerAi.Chat.DeepSeek,
  uMakerAi.Chat.Mistral,
  uMakerAi.Chat.Kimi,
  uMakerAi.Chat.Grok,
  uMakerAi.Chat.Cohere,
  uMakerAi.Chat.LMStudio,
  uMakerAi.Chat.GenericLLM,

  // --- Core (tipos base) ---
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Embeddings,
  uMakerAi.Embeddings.core,

  // --- Tools ---
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Shell,
  uMakerAi.Tools.TextEditor,
  uMakerAi.Tools.ComputerUse,
  uMakerAi.Whisper,
  uMakerAi.OpenAi.Dalle,
  uMakerAi.OpenAI.Sora,
  uMakerAi.OpenAI.Audio,
  uMakerAi.Gemini.Video,
  uMakerAi.Gemini.Speech,
  uMakerAi.Gemini.WebSearch,
  uMakerAi.Ollama.Ocr,

  // --- RAG ---
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.RAG.Graph.Builder,

  // --- MCP ---
  uMakerAi.MCPServer.Core,
  UMakerAi.MCPServer.Http,
  UMakerAi.MCPServer.Stdio,
  UMakerAi.MCPServer.SSE,
  UMakerAi.MCPServer.Direct,

  // --- Agents ---
  uMakerAi.Agents,

  // --- Otros ---
  uMakerAi.Chat.Bridge,
  uMakerAi.Prompts,
  uMakerAi.Utils.VoiceMonitor;


type

  // =========================================================================
  // TAiChatConnection y TAiChat drivers
  // Necesitan: uMakerAi.Core (TAiMediaFile, TAiChatState, TAiErrorEvent),
  //            uMakerAi.Chat.Messages (TAiChatMessage, TAiToolsFunction)
  // =========================================================================

  TAiChatConnectionSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TAiChatDriverSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // =========================================================================
  // TAiEmbeddingConnection y TAiEmbeddings drivers
  // Necesitan: uMakerAi.Embeddings.core (TAiEmbeddingData, TOnGetEmbedding)
  // =========================================================================

  TAiEmbeddingConnectionSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TAiEmbeddingDriverSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // =========================================================================
  // TAiFunctions
  // Necesita: uMakerAi.Core, uMakerAi.Chat.Messages (TAiToolsFunction)
  // =========================================================================

  TAiFunctionsSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // =========================================================================
  // RAG (Vector y Graph)
  // Necesitan: uMakerAi.Embeddings.core (TAiEmbeddingData)
  // =========================================================================

  TAiRAGSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // =========================================================================
  // Agents
  // Necesitan: uMakerAi.Chat.AiConnection
  // =========================================================================

  TAiAgentsSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // =========================================================================
  // MCP Servers
  // Necesitan: uMakerAi.MCPServer.Core (IAiMCPTool, tipos base)
  // =========================================================================

  TAiMCPServerSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // =========================================================================
  // Tools gen?ricos (Shell, TextEditor, ComputerUse, Whisper, Dalle, etc.)
  // Necesitan: uMakerAi.Core
  // =========================================================================

  TAiToolSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  // =========================================================================
  // Bridges
  // Necesitan: uMakerAi.Core, uMakerAi.Chat.Tools
  // =========================================================================

  TAiBridgeSelEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;


// ==========================================================================
// Implementaciones
// ==========================================================================

{ TAiChatConnectionSelEditor }

procedure TAiChatConnectionSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Core');
  Proc('uMakerAi.Chat.Messages');
  Proc('System.JSON');
end;

{ TAiChatDriverSelEditor }

procedure TAiChatDriverSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Core');
  Proc('uMakerAi.Chat.Messages');
  Proc('System.JSON');
end;

{ TAiEmbeddingConnectionSelEditor }

procedure TAiEmbeddingConnectionSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Embeddings.core');
end;

{ TAiEmbeddingDriverSelEditor }

procedure TAiEmbeddingDriverSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Embeddings');
  Proc('uMakerAi.Embeddings.core');
end;

{ TAiFunctionsSelEditor }

procedure TAiFunctionsSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Core');
  Proc('uMakerAi.Chat.Messages');
end;

{ TAiRAGSelEditor }

procedure TAiRAGSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Embeddings');
  Proc('uMakerAi.Embeddings.core');
end;

{ TAiAgentsSelEditor }

procedure TAiAgentsSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Chat.AiConnection');
end;

{ TAiMCPServerSelEditor }

procedure TAiMCPServerSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.MCPServer.Core');
  Proc('System.JSON');
end;

{ TAiToolSelEditor }

procedure TAiToolSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Core');
end;

{ TAiBridgeSelEditor }

procedure TAiBridgeSelEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uMakerAi.Core');
  Proc('uMakerAi.Chat.Tools');
end;


// ==========================================================================
// Registro
// ==========================================================================

procedure Register;
begin

  // --- Conectores universales ---
  RegisterSelectionEditor(TAiChatConnection, TAiChatConnectionSelEditor);
  RegisterSelectionEditor(TAiEmbeddingConnection, TAiEmbeddingConnectionSelEditor);

  // --- Chat drivers ---
  RegisterSelectionEditor(TAiOpenChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiClaudeChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiGeminiChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiOllamaChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiGroqChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiDeepSeekChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiMistralChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiKimiChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiGrokChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TCohereChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiLMStudioChat, TAiChatDriverSelEditor);
  RegisterSelectionEditor(TAiGenericChat, TAiChatDriverSelEditor);

  // --- Embedding drivers ---
  RegisterSelectionEditor(TAiOpenAiEmbeddings, TAiEmbeddingDriverSelEditor);
  RegisterSelectionEditor(TAiOllamaEmbeddings, TAiEmbeddingDriverSelEditor);
  RegisterSelectionEditor(TAiGeminiEmbeddings, TAiEmbeddingDriverSelEditor);
  RegisterSelectionEditor(TAiMistralEmbeddings, TAiEmbeddingDriverSelEditor);
  RegisterSelectionEditor(TAiCohereEmbeddings, TAiEmbeddingDriverSelEditor);
  RegisterSelectionEditor(TAiLMStudioEmbeddings, TAiEmbeddingDriverSelEditor);

  // --- TAiFunctions ---
  RegisterSelectionEditor(TAiFunctions, TAiFunctionsSelEditor);

  // --- RAG ---
  RegisterSelectionEditor(TAiRAGVector, TAiRAGSelEditor);
  RegisterSelectionEditor(TAiRagGraph, TAiRAGSelEditor);
  RegisterSelectionEditor(TAiRagGraphBuilder, TAiRAGSelEditor);

  // --- Agents ---
  RegisterSelectionEditor(TAIAgentManager, TAiAgentsSelEditor);
  RegisterSelectionEditor(TAIAgentsNode, TAiAgentsSelEditor);
  RegisterSelectionEditor(TAIAgentsLink, TAiAgentsSelEditor);
  RegisterSelectionEditor(TAiAgentsToolSample, TAiAgentsSelEditor);
  RegisterSelectionEditor(TAIAgents, TAiAgentsSelEditor);

  // --- MCP Servers ---
  RegisterSelectionEditor(TAiMCPHttpServer, TAiMCPServerSelEditor);
  RegisterSelectionEditor(TAiMCPStdioServer, TAiMCPServerSelEditor);
  RegisterSelectionEditor(TAiMCPSSEHttpServer, TAiMCPServerSelEditor);
  RegisterSelectionEditor(TAiMCPDirectConnection, TAiMCPServerSelEditor);

  // --- Tools ---
  RegisterSelectionEditor(TAiShell, TAiToolSelEditor);
  RegisterSelectionEditor(TAiTextEditorTool, TAiToolSelEditor);
  RegisterSelectionEditor(TAiComputerUseTool, TAiToolSelEditor);
  RegisterSelectionEditor(TAIWhisper, TAiToolSelEditor);
  RegisterSelectionEditor(TAiDalle, TAiToolSelEditor);
  RegisterSelectionEditor(TAiSoraGenerator, TAiToolSelEditor);
  RegisterSelectionEditor(TAiOpenAiAudio, TAiToolSelEditor);
  RegisterSelectionEditor(TAiGeminiVideoTool, TAiToolSelEditor);
  RegisterSelectionEditor(TAiGeminiSpeechTool, TAiToolSelEditor);
  RegisterSelectionEditor(TAiGeminiWebSearchTool, TAiToolSelEditor);
  RegisterSelectionEditor(TAiOllamaOcrTool, TAiToolSelEditor);
  RegisterSelectionEditor(TAIVoiceMonitor, TAiToolSelEditor);

  // --- Bridges ---
  RegisterSelectionEditor(TAiChatSpeechBridge, TAiBridgeSelEditor);
  RegisterSelectionEditor(TAiChatVisionBridge, TAiBridgeSelEditor);
  RegisterSelectionEditor(TAiChatImageBridge, TAiBridgeSelEditor);
  RegisterSelectionEditor(TAiChatVideoBridge, TAiBridgeSelEditor);
  RegisterSelectionEditor(TAiChatDocumentBridge, TAiBridgeSelEditor);
  RegisterSelectionEditor(TAiChatCodeInterpreterBridge, TAiBridgeSelEditor);
  RegisterSelectionEditor(TAiChatWebSearchBridge, TAiBridgeSelEditor);

  // --- Otros ---
  RegisterSelectionEditor(TAiPrompts, TAiToolSelEditor);

end;

end.
