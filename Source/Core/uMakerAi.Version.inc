//uMakerAi.Version;


// Version Information
const
  MAKERAI_VERSION_MAJOR    = 2;
  MAKERAI_VERSION_MINOR    = 5;
  MAKERAI_VERSION_RELEASE  = 0;
  MAKERAI_VERSION_BUILD    = 1;

// Version String Formats
const
  MAKERAI_VERSION_SHORT    = '2.5';
  MAKERAI_VERSION_FULL     = '2.5.0.2';
  MAKERAI_VERSION_STRING   = 'MakerAi Suite v2.5.0.2';
  MAKERAI_VERSION_DISPLAY  = 'MakerAi Suite Library v2.5 - MCP Support';

// Product Information
const
  MAKERAI_PRODUCT_NAME     = 'MakerAi Library';
  MAKERAI_COMPANY_NAME     = 'Gustavo Enríquez';
  MAKERAI_COPYRIGHT        = 'Copyright © 2024-2025 Gustavo Enríquez - CimaMaker';
  MAKERAI_DESCRIPTION      = 'Advanced AI Integration Library for Delphi';

// Build Information
const
  MAKERAI_BUILD_DATE       = '2025-08-17';
  MAKERAI_BUILD_TIME       = '12:00:00';
  MAKERAI_BUILD_PLATFORM   = {$IFDEF WIN32}'Win32'{$ENDIF}
                            {$IFDEF WIN64}'Win64'{$ENDIF}
                            {$IFDEF LINUX64}'Linux64'{$ENDIF}
                            {$IFDEF MACOS64}'macOS64'{$ENDIF}
                            {$IFDEF ANDROID}'Android'{$ENDIF}
                            {$IFDEF IOS}'iOS'{$ENDIF};

// Compatibility Information
const
  MAKERAI_MIN_DELPHI_VERSION = '12.0';  // Minimum Delphi version required
  MAKERAI_TESTED_DELPHI      = '12.2';  // Latest tested Delphi version

// API Compatibility
const
  MAKERAI_API_LEVEL        = 25;        // Internal API level for compatibility
  MAKERAI_BREAKING_CHANGE  = True;      // True if this version has breaking changes
  MAKERAI_LEGACY_SUPPORT   = False;     // False = No support for v1.x

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

  Version 2.5.0 (2025-08-04) -
  - Added MCP (Model Context Protocol) support
  - Enhanced multi-platform compatibility
  - Improved interactive process management
  - Linux/POSIX full support


  Version 2.5.2 (2025-08-17) - CURRENT
  - Added MCP Server (Model Context Protocol)
  - added Agents for complex flow
  }
{******************************************************************************}

// Feature Flags (Current Version)
const
  MAKERAI_HAS_OPENAI       = True;
  MAKERAI_HAS_WHISPER      = True;
  MAKERAI_HAS_EMBEDDINGS   = True;
  MAKERAI_HAS_TOOL_CALLING = True;
  MAKERAI_HAS_RAG          = True;
  MAKERAI_HAS_ASSISTANTS   = True;
  MAKERAI_HAS_MCP          = True;
  MAKERAI_HAS_CHAT_CONNECTION = True;

// Platform Support
const
  MAKERAI_SUPPORT_WINDOWS  = True;
  MAKERAI_SUPPORT_LINUX    = True;
  MAKERAI_SUPPORT_MACOS    = True;
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
  else if SameText(FeatureName, 'RAG') then Result := MAKERAI_HAS_RAG
  else if SameText(FeatureName, 'Assistants') then Result := MAKERAI_HAS_ASSISTANTS
  else if SameText(FeatureName, 'MCP') then Result := MAKERAI_HAS_MCP
  else if SameText(FeatureName, 'ChatConnection') then Result := MAKERAI_HAS_CHAT_CONNECTION;
end;



//Mostrar en el versión about dialog

procedure TFormAbout.FormShow(Sender: TObject);
begin
  lblVersion.Caption := MAKERAI_VERSION_STRING;
  lblCopyright.Caption := MAKERAI_COPYRIGHT;
  lblDescription.Caption := MAKERAI_DESCRIPTION;
  memoVersionInfo.Text := GetMakerAiVersionInfo;
end;


//para verificar compatibilidad

if not IsMakerAiVersionCompatible(2, 0) then
  raise Exception.Create('This feature requires MakerAi v2.0 or higher');

if not IsMakerAiFeatureAvailable('MCP') then
  raise Exception.Create('MCP support not available in this version');


  //para utilizar en recurso .rc de windows

  // En tu archivo de recursos
1 VERSIONINFO
FILEVERSION MAKERAI_VERSION_MAJOR,MAKERAI_VERSION_MINOR,MAKERAI_VERSION_RELEASE,MAKERAI_VERSION_BUILD


}
