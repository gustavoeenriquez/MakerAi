// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.AiConnection.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_AiConnectionHPP
#define uMakerAi_Chat_AiConnectionHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Threading.hpp>
#include <System.NetEncoding.hpp>
#include <System.Rtti.hpp>
#include <System.TypInfo.hpp>
#include <System.StrUtils.hpp>
#include <System.Net.URLClient.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <System.JSON.hpp>
#include <REST.Json.hpp>
#include <UMakerAi.ParamsRegistry.hpp>
#include <uMakerAi.Tools.Functions.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Chat.hpp>
#include <uMakerAi.Chat.Initializations.hpp>
#include <uMakerAi.Tools.Shell.hpp>
#include <uMakerAi.Tools.TextEditor.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Aiconnection
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiChatConnection;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnChatModelChangeEvent)(System::TObject* Sender, Umakerai::Chat::TAiChat* const OldChat, Umakerai::Chat::TAiChat* const NewChat);

class PASCALIMPLEMENTATION TAiChatConnection : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Umakerai::Chat::TAiChat* FChat;
	System::UnicodeString FDriverName;
	System::UnicodeString FModel;
	System::Classes::TStrings* FParams;
	Umakerai::Chat::TAiChatMessages* FMessages;
	Umakerai::Chat::TAiChatMessages* FMessagesOwn;
	System::Classes::TStrings* FInitialInstructions;
	System::Classes::TStrings* FMemory;
	Umakerai::Tools::Functions::TAiFunctions* FAiFunctions;
	int FPrompt_tokens;
	int FCompletion_tokens;
	int FTotal_tokens;
	Umakerai::Chat::TAiChatOnDataEvent FOnReceiveData;
	Umakerai::Chat::TAiChatOnDataEvent FOnReceiveDataEnd;
	Umakerai::Chat::TAiChatOnDataEvent FOnAddMessage;
	Umakerai::Chat::TOnCallToolFunction FOnCallToolFunction;
	Umakerai::Chat::TAiChatOnBeforeSendEvent FOnBeforeSendMessage;
	Umakerai::Chat::TAiChatOnInitChatEvent FOnInitChat;
	Umakerai::Chat::TAiChatOnMediaFileEvent FOnProcessMediaFile;
	Umakerai::Core::TAiErrorEvent FOnError;
	TOnChatModelChangeEvent FOnChatModelChange;
	Umakerai::Chat::TAiChatOnProcessResponseEvent FOnProcessResponse;
	System::UnicodeString FVersion;
	Umakerai::Chat::TAiChatOnDataEvent FOnReceiveThinking;
	Umakerai::Tools::Shell::TAiShell* FShellTool;
	Umakerai::Tools::Texteditor::TAiTextEditorTool* FTextEditorTool;
	void __fastcall SetDriverName(const System::UnicodeString Value);
	void __fastcall SetModel(const System::UnicodeString Value);
	void __fastcall SetParams(System::Classes::TStrings* const Value);
	void __fastcall SetChat(Umakerai::Chat::TAiChat* const Value);
	System::UnicodeString __fastcall GetLastError();
	bool __fastcall GetBusy();
	void __fastcall ParamsChanged(System::TObject* Sender);
	void __fastcall SetAiFunctions(Umakerai::Tools::Functions::TAiFunctions* const Value);
	void __fastcall SetCompletion_tokens(const int Value);
	void __fastcall SetInitialInstructions(System::Classes::TStrings* const Value);
	void __fastcall SetMemory(System::Classes::TStrings* const Value);
	void __fastcall SetOnAddMessage(const Umakerai::Chat::TAiChatOnDataEvent Value);
	void __fastcall SetOnBeforeSendMessage(const Umakerai::Chat::TAiChatOnBeforeSendEvent Value);
	void __fastcall SetOnCallToolFunction(const Umakerai::Chat::TOnCallToolFunction Value);
	void __fastcall SetOnError(const Umakerai::Core::TAiErrorEvent Value);
	void __fastcall SetOnInitChat(const Umakerai::Chat::TAiChatOnInitChatEvent Value);
	void __fastcall SetOnProcessMediaFile(const Umakerai::Chat::TAiChatOnMediaFileEvent Value);
	void __fastcall SetOnProcessResponse(const Umakerai::Chat::TAiChatOnProcessResponseEvent Value);
	void __fastcall SetOnReceiveData(const Umakerai::Chat::TAiChatOnDataEvent Value);
	void __fastcall SetOnReceiveDataEnd(const Umakerai::Chat::TAiChatOnDataEvent Value);
	void __fastcall SetPrompt_tokens(const int Value);
	void __fastcall SetTotal_tokens(const int Value);
	void __fastcall SetOnReceiveThinking(const Umakerai::Chat::TAiChatOnDataEvent Value);
	void __fastcall SetShellTool(Umakerai::Tools::Shell::TAiShell* const Value);
	void __fastcall SetTextEditorTool(Umakerai::Tools::Texteditor::TAiTextEditorTool* const Value);
	
protected:
	void __fastcall ValideChat();
	void __fastcall UpdateAndApplyParams();
	void __fastcall SetupChatFromDriver();
	void __fastcall ApplyParamsToChat(Umakerai::Chat::TAiChat* AChat, System::Classes::TStrings* AParams);
	void __fastcall ApplyEventsToChat(Umakerai::Chat::TAiChat* AChat, bool SetToNil = false);
	void __fastcall OnInternalReceiveDataEnd(System::TObject* const Sender, Umakerai::Chat::TAiChatMessage* aMsg, System::Json::TJSONObject* aResponse, System::UnicodeString aRole, System::UnicodeString aText);
	System::Classes::TStrings* __fastcall MergeParams(System::Classes::TStrings* Origin, System::Classes::TStrings* Destination);
	virtual void __fastcall Loaded();
	
public:
	__fastcall virtual TAiChatConnection(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiChatConnection();
	void __fastcall UpdateParamsFromRegistry();
	System::UnicodeString __fastcall AddMessageAndRun(System::UnicodeString aPrompt, System::UnicodeString aRole, Umakerai::Core::TAiMediaFilesArray aMediaFiles)/* overload */;
	Umakerai::Chat::TAiChatMessage* __fastcall AddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole);
	Umakerai::Chat::TAiChatMessage* __fastcall NewMessage(System::UnicodeString aPrompt, System::UnicodeString aRole);
	virtual System::UnicodeString __fastcall Run(Umakerai::Chat::TAiChatMessage* aMsg = (Umakerai::Chat::TAiChatMessage*)(0x0));
	Umakerai::Chat::TAiChatMessage* __fastcall GetLastMessage();
	bool __fastcall RemoveMesage(Umakerai::Chat::TAiChatMessage* Msg)/* overload */;
	bool __fastcall RemoveMesage(int IdMsg)/* overload */;
	void __fastcall AddToMemory(System::UnicodeString Key, System::UnicodeString Value);
	void __fastcall RemoveFromMemory(System::UnicodeString Key);
	void __fastcall NewChat();
	void __fastcall Abort();
	virtual System::Json::TJSONArray* __fastcall GetMessages();
	virtual System::Classes::TStringList* __fastcall GetDriversNames();
	System::DynamicArray<System::UnicodeString> __fastcall GetAvailableDrivers();
	virtual System::Classes::TStringList* __fastcall GetModels()/* overload */;
	bool __fastcall IsDriverAvailable(const System::UnicodeString DriverName);
	void __fastcall ResetParamsToDefaults();
	void __fastcall RegisterUserParam(const System::UnicodeString DriverName, const System::UnicodeString ModelName, const System::UnicodeString ParamName, const System::UnicodeString ParamValue)/* overload */;
	void __fastcall RegisterUserParam(const System::UnicodeString DriverName, const System::UnicodeString ParamName, const System::UnicodeString ParamValue)/* overload */;
	void __fastcall ClearRegisterParams(const System::UnicodeString DriverName, System::UnicodeString ModelName = System::UnicodeString());
	TAiChatConnection* __fastcall CreateChatForDriver(const System::UnicodeString aDriverName, const System::UnicodeString aModel);
	System::UnicodeString __fastcall UploadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	System::UnicodeString __fastcall CheckFileState(Umakerai::Core::TAiMediaFile* aMediaFile);
	System::UnicodeString __fastcall DeleteFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	System::UnicodeString __fastcall UploadFileToCache(Umakerai::Core::TAiMediaFile* aMediaFile, int aTTL_Seconds = 0xe10);
	__property Umakerai::Chat::TAiChatMessages* Messages = {read=FMessages};
	__property System::UnicodeString LastError = {read=GetLastError};
	__property bool Busy = {read=GetBusy, nodefault};
	__property Umakerai::Chat::TAiChat* AiChat = {read=FChat};
	
__published:
	__property System::UnicodeString DriverName = {read=FDriverName, write=SetDriverName};
	__property System::UnicodeString Model = {read=FModel, write=SetModel};
	__property System::Classes::TStrings* Params = {read=FParams, write=SetParams};
	__property System::Classes::TStrings* InitialInstructions = {read=FInitialInstructions, write=SetInitialInstructions};
	__property System::Classes::TStrings* Memory = {read=FMemory, write=SetMemory};
	__property Umakerai::Tools::Functions::TAiFunctions* AiFunctions = {read=FAiFunctions, write=SetAiFunctions};
	__property int Prompt_tokens = {read=FPrompt_tokens, write=SetPrompt_tokens, nodefault};
	__property int Completion_tokens = {read=FCompletion_tokens, write=SetCompletion_tokens, nodefault};
	__property int Total_tokens = {read=FTotal_tokens, write=SetTotal_tokens, nodefault};
	__property Umakerai::Chat::TAiChatOnDataEvent OnReceiveThinking = {read=FOnReceiveThinking, write=SetOnReceiveThinking};
	__property Umakerai::Chat::TAiChatOnDataEvent OnReceiveData = {read=FOnReceiveData, write=SetOnReceiveData};
	__property Umakerai::Chat::TAiChatOnDataEvent OnReceiveDataEnd = {read=FOnReceiveDataEnd, write=SetOnReceiveDataEnd};
	__property Umakerai::Chat::TAiChatOnDataEvent OnAddMessage = {read=FOnAddMessage, write=SetOnAddMessage};
	__property Umakerai::Chat::TOnCallToolFunction OnCallToolFunction = {read=FOnCallToolFunction, write=SetOnCallToolFunction};
	__property Umakerai::Chat::TAiChatOnBeforeSendEvent OnBeforeSendMessage = {read=FOnBeforeSendMessage, write=SetOnBeforeSendMessage};
	__property Umakerai::Chat::TAiChatOnInitChatEvent OnInitChat = {read=FOnInitChat, write=SetOnInitChat};
	__property Umakerai::Chat::TAiChatOnMediaFileEvent OnProcessMediaFile = {read=FOnProcessMediaFile, write=SetOnProcessMediaFile};
	__property Umakerai::Core::TAiErrorEvent OnError = {read=FOnError, write=SetOnError};
	__property TOnChatModelChangeEvent OnChatModelChange = {read=FOnChatModelChange, write=FOnChatModelChange};
	__property Umakerai::Chat::TAiChatOnProcessResponseEvent OnProcessResponse = {read=FOnProcessResponse, write=SetOnProcessResponse};
	__property System::UnicodeString Version = {read=FVersion};
	__property Umakerai::Tools::Shell::TAiShell* ShellTool = {read=FShellTool, write=SetShellTool};
	__property Umakerai::Tools::Texteditor::TAiTextEditorTool* TextEditorTool = {read=FTextEditorTool, write=SetTextEditorTool};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Aiconnection */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_AICONNECTION)
using namespace Umakerai::Chat::Aiconnection;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT)
using namespace Umakerai::Chat;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_Chat_AiConnectionHPP
