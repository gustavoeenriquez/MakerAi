//uMakerAi.Version;
// Version Information
const
  MAKERAI_VERSION_MAJOR    = 3;
  MAKERAI_VERSION_MINOR    = 3;
  MAKERAI_VERSION_RELEASE  = 0;
  MAKERAI_VERSION_BUILD    = 0;

// Version String Formats
const
  MAKERAI_VERSION_SHORT    = '3.3';
  MAKERAI_VERSION_FULL     = '3.3.0.0';
  MAKERAI_VERSION_STRING   = 'MakerAi Suite v3.3.0.0';
  MAKERAI_VERSION_DISPLAY  = 'MakerAi Suite Library v3.3 - TAiCapabilities, Agents Checkpoints & Updated Models (Feb 2026)';

// Product Information
const
  MAKERAI_PRODUCT_NAME     = 'MakerAi Library';
  MAKERAI_COMPANY_NAME     = 'Gustavo Enr�quez';
  MAKERAI_COPYRIGHT        = 'Copyright � 2024-2026 Gustavo Enr�quez - CimaMaker';
  MAKERAI_DESCRIPTION      = 'Advanced AI Integration Library for Delphi';

// Build Information
const
  MAKERAI_BUILD_DATE       = '2026-02-26';
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
  MAKERAI_TESTED_DELPHI      = '13.0';  // Latest tested Delphi version (Delphi 13 Athens)

// API Compatibility
const
  MAKERAI_API_LEVEL        = 33;        // Internal API level for compatibility
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
