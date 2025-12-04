// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MakerAI.dpk' rev: 36.00 (Windows)

#ifndef MakerAIHPP
#define MakerAIHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>	// (rtl)
#include <SysInit.hpp>
#include <uMakerAi.MCPClient.Core.hpp>
#include <uMakerAi.Chat.AiConnection.hpp>
#include <uMakerAi.Chat.Claude.hpp>
#include <uMakerAi.Chat.DeepSeek.hpp>
#include <uMakerAi.Chat.Gemini.hpp>
#include <uMakerAi.Chat.Grok.hpp>
#include <uMakerAi.Chat.Groq.hpp>
#include <uMakerAi.Chat.Initializations.hpp>
#include <uMakerAi.Chat.Mistral.hpp>
#include <uMakerAi.Chat.Ollama.hpp>
#include <uMakerAi.Chat.OpenAi.hpp>
#include <uMakerAi.Tools.Functions.hpp>
#include <uMakerAi.Chat.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.Prompts.hpp>
#include <uMakerAi.Utils.CodeExtractor.hpp>
#include <uMakerAi.Utils.PcmToWav.hpp>
#include <uMakerAi.Utils.system.hpp>
#include <uMakerAi.OpenAi.Dalle.hpp>
#include <uMakerAi.Whisper.hpp>
#include <uMakerAi.Agents.hpp>
#include <UMakerAi.MCPServer.Http.hpp>
#include <uMakerAi.MCPServer.Core.hpp>
#include <UMakerAi.MCPServer.Stdio.hpp>
#include <UMakerAi.MCPServer.Direct.hpp>
#include <uMakerAi.Utils.VoiceMonitor.hpp>
#include <uMakerAi.Embeddings.core.hpp>
#include <uMakerAi.RAG.Graph.Builder.hpp>
#include <uMakerAi.RAG.Graph.Core.hpp>
#include <uMakerAi.RAG.Vectors.hpp>
#include <uMakerAi.Agents.Attributes.hpp>
#include <uMakerAi.Agents.EngineRegistry.hpp>
#include <UMakerAi.ParamsRegistry.hpp>
#include <uMakerAi.Chat.Kimi.hpp>
#include <uMakerAi.Chat.LMStudio.hpp>
#include <uMakerAi.Gemini.Veo.hpp>
#include <uMakerAi.OpenAI.Sora.hpp>
#include <uMakerAi.OpenAI.Audio.hpp>
#include <uMakerAi.Utils.DiffUpdater.hpp>
#include <uMakerAi.Tools.Shell.hpp>
#include <uMakerAi.Chat.GenericLLM.hpp>
#include <UMakerAi.MCPServer.SSE.hpp>
#include <uMakerAi.Tools.TextEditor.hpp>
#include <System.UITypes.hpp>	// (rtl)
#include <Winapi.Windows.PkgHelper.hpp>	// (rtl)
#include <Winapi.PsAPI.hpp>	// (rtl)
#include <System.Character.hpp>	// (rtl)
#include <System.Internal.ExcUtils.hpp>	// (rtl)
#include <System.SysUtils.hpp>	// (rtl)
#include <System.VarUtils.hpp>	// (rtl)
#include <System.Variants.hpp>	// (rtl)
#include <System.TypInfo.hpp>	// (rtl)
#include <System.Math.hpp>	// (rtl)
#include <System.Generics.Defaults.hpp>	// (rtl)
#include <System.TimeSpan.hpp>	// (rtl)
#include <System.SyncObjs.hpp>	// (rtl)
#include <System.Rtti.hpp>	// (rtl)
#include <System.Classes.hpp>	// (rtl)
#include <System.DateUtils.hpp>	// (rtl)
#include <System.JSON.hpp>	// (rtl)
#include <System.IOUtils.hpp>	// (rtl)
#include <System.IniFiles.hpp>	// (rtl)
#include <System.Win.Registry.hpp>	// (rtl)
#include <System.Net.Mime.hpp>	// (rtl)
#include <System.Threading.hpp>	// (rtl)
#include <System.NetEncoding.hpp>	// (rtl)
#include <System.Net.URLClient.hpp>	// (rtl)
#include <System.Net.HttpClient.Win.hpp>	// (rtl)
#include <System.AnsiStrings.hpp>	// (rtl)
#include <System.JSON.Types.hpp>	// (rtl)
#include <REST.JsonReflect.hpp>	// (RESTComponents)
#include <REST.Json.hpp>	// (RESTComponents)
#include <REST.Types.hpp>	// (RESTComponents)
#include <System.JSON.Utils.hpp>	// (rtl)
#include <System.JSON.Readers.hpp>	// (rtl)
#include <System.JSON.Writers.hpp>	// (rtl)
#include <System.NetEncoding.Sqids.hpp>	// (rtl)
#include <System.Bindings.Search.hpp>	// (bindengine)
#include <System.Bindings.Evaluator.hpp>	// (bindengine)
#include <System.Bindings.EvalSys.hpp>	// (bindengine)
#include <System.Bindings.Graph.hpp>	// (bindengine)
#include <System.Bindings.Factories.hpp>	// (bindengine)
#include <System.Bindings.Outputs.hpp>	// (bindengine)
#include <System.Bindings.Methods.hpp>	// (bindengine)
#include <Data.Bind.Grid.hpp>	// (bindcomp)
#include <Data.Bind.Components.hpp>	// (bindcomp)
#include <Data.Bind.ObjectScope.hpp>	// (bindcomp)
#include <Data.Bind.JSON.hpp>	// (bindcomp)
#include <Data.SqlTimSt.hpp>	// (dbrtl)
#include <Data.FmtBcd.hpp>	// (dbrtl)
#include <Data.DB.hpp>	// (dbrtl)
#include <IdGlobal.hpp>	// (IndySystem)
#include <IdWinsock2.hpp>	// (IndySystem)
#include <IdWship6.hpp>	// (IndySystem)
#include <IdIDN.hpp>	// (IndySystem)
#include <IdStackWindows.hpp>	// (IndySystem)
#include <IdStack.hpp>	// (IndySystem)
#include <IdComponent.hpp>	// (IndySystem)
#include <IdIOHandler.hpp>	// (IndyCore)
#include <IdIOHandlerStack.hpp>	// (IndyCore)
#include <IdGlobalProtocols.hpp>	// (IndyProtocols)
#include <IdThread.hpp>	// (IndyCore)
#include <IdCoderMIME.hpp>	// (IndyProtocols)
#include <IdAuthentication.hpp>	// (IndyProtocols)
#include <IdHTTPHeaderInfo.hpp>	// (IndyProtocols)
#include <IdSSL.hpp>	// (IndyProtocols)
#include <System.ConvUtils.hpp>	// (rtl)
#include <System.Permissions.hpp>	// (rtl)
#include <System.Win.ComObj.hpp>	// (rtl)
#include <Xml.Win.msxmldom.hpp>	// (xmlrtl)
#include <Xml.xmldom.hpp>	// (xmlrtl)
#include <Xml.XMLSchema.hpp>	// (xmlrtl)
#include <Xml.xmlutil.hpp>	// (xmlrtl)
// PRG_EXT: .bpl
// BPI_DIR: C:\Users\Public\Documents\Embarcadero\Studio\23.0\Dcp
// OBJ_DIR: C:\Users\Public\Documents\Embarcadero\Studio\23.0\Dcp
// OBJ_EXT: .obj

//-- user supplied -----------------------------------------------------------

namespace Makerai
{
//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
}	/* namespace Makerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_MAKERAI)
using namespace Makerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MakerAIHPP
