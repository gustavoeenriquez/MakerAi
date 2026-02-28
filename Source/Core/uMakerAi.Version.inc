// uMakerAi.Version.inc — Free Pascal port
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
  MAKERAI_VERSION_DISPLAY  = 'MakerAi Suite Library v3.3 - FPC Port (Feb 2026)';

// Product Information
const
  MAKERAI_PRODUCT_NAME     = 'MakerAi Library';
  MAKERAI_COMPANY_NAME     = 'Gustavo Enriquez';
  MAKERAI_COPYRIGHT        = 'Copyright 2024-2026 Gustavo Enriquez - CimaMaker';
  MAKERAI_DESCRIPTION      = 'Advanced AI Integration Library for Free Pascal';

// Build Information
const
  MAKERAI_BUILD_DATE       = '2026-02-27';
  MAKERAI_BUILD_PLATFORM   = {$IFDEF WIN32}'Win32'{$ENDIF}
                            {$IFDEF WIN64}'Win64'{$ENDIF}
                            {$IFDEF LINUX}'Linux'{$ENDIF}
                            {$IFDEF DARWIN}'macOS'{$ENDIF};

// Compatibility Information
const
  MAKERAI_MIN_FPC_VERSION  = '3.2.0';   // Free Pascal 3.2.0+
  MAKERAI_API_LEVEL        = 33;
  MAKERAI_BREAKING_CHANGE  = False;
  MAKERAI_LEGACY_SUPPORT   = True;

// Feature Flags
const
  MAKERAI_HAS_OPENAI           = True;
  MAKERAI_HAS_WHISPER          = True;
  MAKERAI_HAS_EMBEDDINGS       = True;
  MAKERAI_HAS_TOOL_CALLING     = True;
  MAKERAI_HAS_RAG_VECTOR       = True;
  MAKERAI_HAS_RAG_GRAPH        = True;
  MAKERAI_HAS_ASSISTANTS       = True;
  MAKERAI_HAS_MCP              = True;
  MAKERAI_HAS_CHAT_CONNECTION  = True;
  MAKERAI_HAS_UI_COMPONENTS    = False; // UI: pendiente (depende del framework visual)
  MAKERAI_HAS_AGENTS           = True;
  MAKERAI_HAS_CHECKPOINTS      = True;
  MAKERAI_HAS_CHAT_TOOLS       = True;

// Platform Support
const
  MAKERAI_SUPPORT_WINDOWS  = True;
  MAKERAI_SUPPORT_LINUX    = True;
  MAKERAI_SUPPORT_MACOS    = False;   // pendiente
  MAKERAI_SUPPORT_MOBILE   = False;   // pendiente en FPC

// License Information
const
  MAKERAI_LICENSE          = 'MIT License';
  MAKERAI_LICENSE_URL      = 'https://opensource.org/licenses/MIT';
