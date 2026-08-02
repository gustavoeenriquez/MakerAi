//uMakerAi.Version;
// Version Information
const
  MAKERAI_VERSION_MAJOR    = 3;
  MAKERAI_VERSION_MINOR    = 6;
  MAKERAI_VERSION_RELEASE  = 0;
  MAKERAI_VERSION_BUILD    = 0;

// Version String Formats
const
  MAKERAI_VERSION_SHORT    = '3.6';
  MAKERAI_VERSION_FULL     = '3.6.0.0';
  MAKERAI_VERSION_STRING   = 'MakerAi Suite v3.6.0.0';
  MAKERAI_VERSION_DISPLAY  = 'MakerAi Suite Library v3.6 - MCP 2026 Stateless, OpenTelemetry, A2A, Guardrails & Evals (Aug 2026)';

// Product Information
const
  MAKERAI_PRODUCT_NAME     = 'MakerAi Library';
  MAKERAI_COMPANY_NAME     = 'Gustavo Enr�quez';
  MAKERAI_COPYRIGHT        = 'Copyright � 2024-2026 Gustavo Enr�quez - CimaMaker';
  MAKERAI_DESCRIPTION      = 'Advanced AI Integration Library for Delphi';

// Build Information
const
  MAKERAI_BUILD_DATE       = '2026-08-02';
  MAKERAI_BUILD_TIME       = '12:00:00';
  MAKERAI_BUILD_PLATFORM   = {$IFDEF WIN32}'Win32'{$ENDIF}
                            {$IFDEF WIN64}'Win64'{$ENDIF}
                            {$IFDEF LINUX64}'Linux64'{$ENDIF}
                            {$IFDEF MACOS64}'macOS64'{$ENDIF}
                            {$IFDEF ANDROID}'Android'{$ENDIF}
                            {$IFDEF IOS}'iOS'{$ENDIF};

// Compatibility Information
const
  MAKERAI_MIN_DELPHI_VERSION = '10.4';  // Minimum Delphi version required (10.4 Sydney)
  MAKERAI_TESTED_DELPHI      = '13.1';  // Latest tested Delphi version (Delphi 13.1 Florence)

// API Compatibility
const
  MAKERAI_API_LEVEL        = 36;        // Internal API level for compatibility
  MAKERAI_BREAKING_CHANGE  = False;     // True if this version has breaking changes
  MAKERAI_LEGACY_SUPPORT   = True;      // True = Support for v2.x components

{******************************************************************************}
{                              VERSION HISTORY                                 }
{******************************************************************************}
{
  Version 1.0.0 (2024-01-XX)
  - Initial release
  - Individual components as API wrappers
  - Basic OpenAI integration

  Version 1.3.0 (2024-XX-XX)
  - Added Whisper component for speech recognition
  - Added Embeddings components
  - Enhanced error handling

  Version 1.5.0 (2024-XX-XX)
  - Added Tool Calling support
  - Function execution capabilities
  - Extended API coverage

  Version 1.7.0 (2024-XX-XX)
  - Enhanced Embeddings components
  - Vector similarity search
  - Performance improvements

  Version 1.8.0 (2024-XX-XX)
  - RAG (Retrieval-Augmented Generation) support
  - AI Assistants integration
  - Knowledge base management

  Version 2.0.0 (2024-XX-XX) - BREAKING CHANGES
  - Complete architecture redesign
  - Introduction of TAiChatConnection component
  - Unified AI tools management
  - NOT compatible with v1.x
  - Migration from API wrapper to AI assistant tool

  Version 2.5.0 (2025-08-04)
  - Added MCP (Model Context Protocol) support
  - Enhanced multi-platform compatibility
  - Improved interactive process management
  - Linux/POSIX full support

  Version 2.5.2 (2025-08-17)
  - Added MCP Server (Model Context Protocol)
  - Added Agents for complex flow

  Version 3.0.0 (2025-10-14) - MAJOR RELEASE
  - NEW: Visual UI components for rapid chat development
  - NEW: Graph-based vector database (RAG-Graph)
  - ENHANCED: Traditional vector database (RAG-Vector)
  - Compatible with Delphi 10.4 Sydney through Delphi 13 Athens
  - Improved visual component integration
  - Enhanced RAG capabilities with graph relationships
  - Backward compatible with v2.x components

  Version 3.1.0 (2025-11-4) -
  - NEW: update for new models  Gemini 3, Claude 4.5 and GPT-5.2

  Version 3.2.0 (2026-01-04)
  - NEW: ChatTools plugin framework for chat models
  - IAiPdfTool, IAiVisionTool, IAiSpeechTool, IAiWebSearchTool, IAiImageTool, IAiVideoTool
  - Unified deterministic tool orchestration and capability bridges

  Version 3.3.0 (2026-02-26)
  - NEW: TAiCapabilities system (ModelCaps / SessionCaps / ThinkingLevel)
  - UPDATED: All provider models - OpenAI gpt-5.2, Claude 4.6, Gemini 3.0, Grok 4,
             Mistral Magistral, DeepSeek-reasoner, Kimi k2.5 (February 2026)
  - NEW: Agents durable execution - TAiFileCheckpointer, TAiNullCheckpointer
  - NEW: Human-in-the-loop - TAiWaitApprovalTool, OnSuspend event, ResumeThread
  - NEW: RAG Graph Document management (uMakerAi.RAG.Graph.Documents)
  - NEW: TAiEmbeddingsConnection - abstract connector for swappable embedding providers
  - NEW: TAiAudioPushStream - push-based audio streaming
  - FIX: reasoning_content preserved in multi-turn tool calls (DeepSeek, Kimi, Groq)
  - FIX: ExtractToolCallFromJson - default type 'function' when field missing (Mistral)
  - FIX: Double-add/double-free in RunNew+RunLegacy (Cohere tool calls)
  - FIX: Mistral json_schema.name field required by API

  Version 3.4.0 (2026-05-18)
  - Tested with Delphi 13.1 Florence (CompilerVersion 37.1)
  - FIX: Selective driver registration restored — TAiChatConnection no longer force-loads
         all drivers via uMakerAi.Chat.Initializations. Each driver self-registers only
         when explicitly imported. To load all drivers at once, add
         uMakerAi.Chat.Initializations to your uses clause explicitly.
  - FIX: Agent jmAll join node — FJoinInputs now cleared after each execution, restoring
         correct barrier semantics. Previous behavior (not clearing) caused premature
         firing on retries/loops because stale inputs from prior execution satisfied
         the Count >= InEdges.Count condition immediately.
  - FIX: TAiAgentManager.Run declared virtual to allow proper subclassing
  - FIX: TChatBubble — TMemo ShowScrollBars := False eliminates spurious vertical bars
  - FIX: TChatInput — StyledSettings removes Size so Font.Size property is respected
  - FIX: Claude Opus 4.7 Adaptive Thinking — temperature, top_p, top_k and thinking block
         now correctly omitted for claude-opus-4-7 models; Anthropic manages sampling
         internally. Sending these parameters caused HTTP 400 errors.
  - FIX: RegisterDefaultParams Max_Tokens key — corrected in 10 drivers (Claude, Gemini,
         Mistral, Groq, DeepSeek, Grok, Kimi, LMStudio, GenericLLM, Ollama). The wrong
         key "MaxTokens" was never resolved by RTTI to the Max_tokens property.
  - FIX: ApplyParamsToChat TryStrToFloat — now locale-independent: tries invariant format
         (dot decimal) first, then system locale fallback. Both "0.7" and "0,7" are valid
         regardless of regional settings.
  - NEW: TChatInput.EnterAsSend property (default False) — Enter sends prompt,
         Shift+Enter / Ctrl+Enter inserts line break
  - NEW: TAiRealtimeConnection — universal connector for real-time STT (OpenAI WebSocket)
  - NEW: cmSmartDispatch chat mode — two-pass routing with automatic capability bridging
  - NEW: Demo 08-DriverRegistration illustrating driver registration behavior

  Version 3.5.0 (2026-08-01)
  - NEW: Typed ModelConfig channel — ModelCaps/SessionCaps/Tool_Active/ThinkingLevel
         moved out of Params/RTTI into a typed surface with per-field user pins
         (UserFields) and transparent compatibility migration. Single source of
         truth for capability configuration.
  - NEW: Full-duplex voice suite — TAiRealtimeVoiceBase shared base;
         TAiGrokRealtimeChat (xAI Grok Voice speech-to-speech: function calling,
         session resumption with replay, binary audio, ephemeral tokens);
         TAiOpenAiRealtimeTranslate (continuous streaming speech translation);
         gpt-transcribe / gpt-live-transcribe (Whisper successors)
  - NEW: Grok native video generation — grok-imagine async job (POST
         /videos/generations + polling + mp4 download as TAiMediaFile),
         VideoDurationSeconds property; image generation via grok-imagine-image
  - NEW: August 2026 provider refresh, runtime-tested per provider:
         Claude 5 family (adaptive thinking, output_config.effort, FastMode,
         compaction, server-side fallbacks, web_search_20260209), Gemini 3.5/3.6
         + Nano Banana GA, Mistral Voxtral TTS + OCR 4, Kimi K3 (top_p fix),
         DeepSeek V4 (explicit thinking control), Cohere Command A+ /
         north-mini-code (thinking capture), Groq qwen3.6-27b, xAI grok-4.3/4.5/
         build with full catalog turnover and compatibility aliases
  - NEW: MSSQL driver for RAG Vector (FireDAC SQL Server)
  - NEW: Agents hardening (from MakerFlow review): strict JSON graph validation,
         public RTTI mapper TAiToolParams, [TSecret] attribute, out_failure in
         conditional mode, Compile no longer clears the Blackboard
  - NEW: ChatTools single surface with OnChange propagation; ToolCall.ResMsg
         available in streaming; media delivered at OnReceiveDataEnd
  - FIX: LastError now populated on every error path (DoError assigns
         FLastError) — synchronous callers can diagnose HTTP 4xx/5xx
  - FIX: Cohere synchronous tool-calling returned empty (second round now
         reuses the same ResMsg, base pattern)
  - FIX: FreeNotification audit across components; TAiMemory usable without
         Connection

  Version 3.6.0 (2026-08-02)
  - NEW: MCP specification 2026-07-28 (stateless) with dual-era support.
         Server implements server/discover, per-request _meta (protocol
         version, client identity and capabilities), resultType + serverInfo
         on every result, ttlMs/cacheScope cache hints and the reserved error
         codes -32020 (HeaderMismatch) / -32022 (UnsupportedProtocolVersion).
         Modern stateless requests bypass the session gate with per-request
         OnClientConnect vetting, while the legacy initialize handshake and
         Mcp-Session-Id gating keep working unchanged. Clients probe with
         server/discover and fall back automatically (NegotiatedProtocol)
  - NEW: MRTR (Multi Round-Trip Requests) — tools can request user input via
         elicitation: server plumbs params.inputResponses / params.requestState
         into TAiAuthContext; the client's OnInputRequired event drives the
         retry loop (max 3 rounds, opaque requestState echoed verbatim)
  - NEW: OpenTelemetry tracing — TAiTelemetry, opt-in OTLP/HTTP JSON exporter
         (standard collector endpoint localhost:4318; works with Jaeger,
         Grafana Tempo, Langfuse, Arize Phoenix). Spans follow the GenAI
         semantic conventions and cover chat turns (model, provider, token
         usage; sync and async), tool executions, agent graphs and nodes, RAG
         retrieval and MCP client/server requests, with W3C traceparent
         propagated through MCP _meta so client and server share one
         distributed trace. Zero overhead when no instance is enabled
  - NEW: A2A protocol (Agent-to-Agent, Linux Foundation) — first Delphi
         implementation of spec 1.0. TAiA2AServer exposes any TAIAgentManager
         graph as an A2A agent (Agent Card at /.well-known/agent-card.json,
         JSON-RPC SendMessage/GetTask/CancelTask with 0.x aliases; graph
         suspension maps to TASK_STATE_INPUT_REQUIRED). TAiA2AClient consumes
         remote agents, and TAiA2ARemoteAgentTool federates: any graph node can
         delegate its input to a remote A2A agent
  - NEW: Guardrails — TAiGuardrails intercepts every tool call before
         execution at the single choke point (TAiFunctions.DoCallFunction), so
         it covers local functions, MCP tools and AutoMCP alike. Allowlist and
         blocklist with wildcard masks, forbidden argument patterns and a
         programmatic OnCheckToolCall veto; blocked calls never execute and the
         LLM receives the reason so it can replan
  - NEW: Evals — TAiEvalRunner, fluent evaluation cases against a generic
         target function, with deterministic checks plus optional
         LLM-as-judge; ToText / ToJSON reports and an eval.case span per case
  - NEW: Regression suite (Tests/RegressionSuite) — 17 in-process cases, no
         API keys, under a second; the framework's first automated safety net,
         built on TAiEvalRunner
  - NEW: Demo 072-A2AFederation showcasing the full A2A stack without an LLM
  - NOTE: the legacy HTTP+SSE MCP transport is formally Deprecated by spec
         2026-07-28 (earliest removal July 2027). Prefer HTTP or StdIO

}
{******************************************************************************}

// Feature Flags (Current Version)
const
  MAKERAI_HAS_OPENAI       = True;
  MAKERAI_HAS_WHISPER      = True;
  MAKERAI_HAS_EMBEDDINGS   = True;
  MAKERAI_HAS_TOOL_CALLING = True;
  MAKERAI_HAS_RAG_VECTOR   = True;  // Traditional vector-based RAG
  MAKERAI_HAS_RAG_GRAPH    = True;  // NEW: Graph-based RAG
  MAKERAI_HAS_ASSISTANTS   = True;
  MAKERAI_HAS_MCP              = True;
  MAKERAI_HAS_CHAT_CONNECTION  = True;
  MAKERAI_HAS_UI_COMPONENTS    = True;
  MAKERAI_HAS_AGENTS           = True;
  MAKERAI_HAS_CHECKPOINTS      = True;  // NEW v3.3: Durable agent execution
  MAKERAI_HAS_CHAT_TOOLS       = True;  // NEW v3.2: ChatTools framework
  MAKERAI_HAS_TELEMETRY        = True;  // NEW v3.6: OpenTelemetry tracing (TAiTelemetry)
  MAKERAI_HAS_A2A              = True;  // NEW v3.6: Agent-to-Agent protocol (spec 1.0)
  MAKERAI_HAS_GUARDRAILS       = True;  // NEW v3.6: Tool-call policy (TAiGuardrails)
  MAKERAI_HAS_EVALS            = True;  // NEW v3.6: Evaluation framework (TAiEvalRunner)

// Platform Support
const
  MAKERAI_SUPPORT_WINDOWS  = True;
  MAKERAI_SUPPORT_LINUX    = True;
  MAKERAI_SUPPORT_MACOS    = False;
  MAKERAI_SUPPORT_MOBILE   = True;

// License Information
const
  MAKERAI_LICENSE          = 'MIT License';
  MAKERAI_LICENSE_URL      = 'https://opensource.org/licenses/MIT';

{******************************************************************************}
{                                UTILITIES                                     }
{******************************************************************************}
{
// Function to get full version info at runtime
function GetMakerAiVersionInfo: string;
begin
  Result := Format('%s %s (Build: %s %s) [%s]', [
    MAKERAI_PRODUCT_NAME,
    MAKERAI_VERSION_FULL,
    MAKERAI_BUILD_DATE,
    MAKERAI_BUILD_TIME,
    MAKERAI_BUILD_PLATFORM
  ]);
end;

// Function to check version compatibility
function IsMakerAiVersionCompatible(RequiredMajor, RequiredMinor: Integer): Boolean;
begin
  Result := (MAKERAI_VERSION_MAJOR > RequiredMajor) or
           ((MAKERAI_VERSION_MAJOR = RequiredMajor) and (MAKERAI_VERSION_MINOR >= RequiredMinor));
end;

// Function to get feature availability
function IsMakerAiFeatureAvailable(const FeatureName: string): Boolean;
begin
  Result := False;
  if SameText(FeatureName, 'OpenAI') then Result := MAKERAI_HAS_OPENAI
  else if SameText(FeatureName, 'Whisper') then Result := MAKERAI_HAS_WHISPER
  else if SameText(FeatureName, 'Embeddings') then Result := MAKERAI_HAS_EMBEDDINGS
  else if SameText(FeatureName, 'ToolCalling') then Result := MAKERAI_HAS_TOOL_CALLING
  else if SameText(FeatureName, 'RAG') then Result := MAKERAI_HAS_RAG_VECTOR
  else if SameText(FeatureName, 'RAG-Vector') then Result := MAKERAI_HAS_RAG_VECTOR
  else if SameText(FeatureName, 'RAG-Graph') then Result := MAKERAI_HAS_RAG_GRAPH
  else if SameText(FeatureName, 'Assistants') then Result := MAKERAI_HAS_ASSISTANTS
  else if SameText(FeatureName, 'MCP') then Result := MAKERAI_HAS_MCP
  else if SameText(FeatureName, 'ChatConnection') then Result := MAKERAI_HAS_CHAT_CONNECTION
  else if SameText(FeatureName, 'UI') then Result := MAKERAI_HAS_UI_COMPONENTS
  else if SameText(FeatureName, 'UIComponents') then Result := MAKERAI_HAS_UI_COMPONENTS
  else if SameText(FeatureName, 'Agents') then Result := MAKERAI_HAS_AGENTS;
end;

//Mostrar en el versi�n about dialog
procedure TFormAbout.FormShow(Sender: TObject);
begin
  lblVersion.Caption := MAKERAI_VERSION_STRING;
  lblCopyright.Caption := MAKERAI_COPYRIGHT;
  lblDescription.Caption := MAKERAI_DESCRIPTION;
  memoVersionInfo.Text := GetMakerAiVersionInfo;
end;

//Para verificar compatibilidad
if not IsMakerAiVersionCompatible(3, 0) then
  raise Exception.Create('This feature requires MakerAi v3.0 or higher');

if not IsMakerAiFeatureAvailable('RAG-Graph') then
  raise Exception.Create('RAG-Graph support not available in this version');

if not IsMakerAiFeatureAvailable('UIComponents') then
  raise Exception.Create('UI Components not available in this version');

//Para utilizar en recurso .rc de Windows
// En tu archivo de recursos
1 VERSIONINFO
FILEVERSION MAKERAI_VERSION_MAJOR,MAKERAI_VERSION_MINOR,MAKERAI_VERSION_RELEASE,MAKERAI_VERSION_BUILD
}
