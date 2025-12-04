// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.OpenAi.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_OpenAiHPP
#define uMakerAi_Chat_OpenAiHPP

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
#include <System.Threading.hpp>
#include <System.Generics.Collections.hpp>
#include <System.NetEncoding.hpp>
#include <System.Net.URLClient.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.StrUtils.hpp>
#include <System.Net.Mime.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <System.JSON.hpp>
#include <REST.Json.hpp>
#include <UMakerAi.ParamsRegistry.hpp>
#include <uMakerAi.Chat.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.Embeddings.core.hpp>
#include <uMakerAi.Tools.Functions.hpp>
#include <uMakerAi.Utils.CodeExtractor.hpp>
#include <uMakerAi.Utils.system.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Openai
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiOpenChat;
class DELPHICLASS TAiOpenAiEmbeddings;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TAiApplyPatchEvent)(System::TObject* Sender, const System::UnicodeString OperationType, const System::UnicodeString Path, const System::UnicodeString Diff, const System::UnicodeString CallId, System::UnicodeString &aStatus, System::UnicodeString &aOutput);

typedef void __fastcall (__closure *TAiShellCommandEvent)(System::TObject* Sender, const System::UnicodeString Command, const System::UnicodeString CallId, System::UnicodeString &StdOut, System::UnicodeString &StdErr, int &ExitCode, bool &Handled);

enum DECLSPEC_DENUM TAiReasoningSummary : unsigned char { rsmDefault, rsmAuto, rsmConcise, rsmDetailed };

class PASCALIMPLEMENTATION TAiOpenChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
private:
	bool FStore;
	System::UnicodeString FTruncation;
	bool FParallel_ToolCalls;
	System::UnicodeString FVerbosity;
	System::UnicodeString FResponseId;
	System::UnicodeString FResponseStatus;
	System::Json::TJSONObject* FLastApiResponse;
	TAiApplyPatchEvent FOnApplyPatch;
	TAiShellCommandEvent FOnShellCommand;
	bool FAllowAutoShell;
	TAiReasoningSummary FReasoningSummary;
	bool FRecursionNeeded;
	void __fastcall SetStore(const bool Value);
	void __fastcall SetTruncation(const System::UnicodeString Value);
	void __fastcall SetParallel_ToolCalls(const bool Value);
	void __fastcall SetVerbosity(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall OnInternalReceiveData(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &AAbort);
	virtual void __fastcall ParseChat(System::Json::TJSONObject* jObj, Umakerai::Chat::TAiChatMessage* ResMsg);
	virtual System::UnicodeString __fastcall InitChatCompletions();
	HIDESBASE System::Json::TJSONArray* __fastcall GetTools(Umakerai::Tools::Functions::TAiFunctions* Funcion);
	void __fastcall UpdateResponseStatus(System::UnicodeString aStatus);
	virtual void __fastcall DoCallFunction(Umakerai::Core::TAiToolsFunction* ToolCall);
	virtual System::UnicodeString __fastcall InternalRunCompletions(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunSpeechGeneration(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunTranscription(Umakerai::Core::TAiMediaFile* aMediaFile, Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	
public:
	__fastcall virtual TAiOpenChat(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiOpenChat();
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
	virtual System::UnicodeString __fastcall UploadFileToCache(Umakerai::Core::TAiMediaFile* aMediaFile, int aTTL_Seconds = 0xe10);
	virtual System::UnicodeString __fastcall DeleteFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DownLoadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	int __fastcall DeleteAllUploadedFiles();
	virtual void __fastcall NewChat();
	virtual System::UnicodeString __fastcall InternalRunImageVideoGeneration(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	
__published:
	__property bool Store = {read=FStore, write=SetStore, default=1};
	__property System::UnicodeString Truncation = {read=FTruncation, write=SetTruncation};
	__property bool Parallel_ToolCalls = {read=FParallel_ToolCalls, write=SetParallel_ToolCalls, default=1};
	__property System::UnicodeString Verbosity = {read=FVerbosity, write=SetVerbosity};
	__property TAiApplyPatchEvent OnApplyPatch = {read=FOnApplyPatch, write=FOnApplyPatch};
	__property TAiShellCommandEvent OnShellCommand = {read=FOnShellCommand, write=FOnShellCommand};
	__property bool AllowAutoShell = {read=FAllowAutoShell, write=FAllowAutoShell, default=0};
	__property TAiReasoningSummary ReasoningSummary = {read=FReasoningSummary, write=FReasoningSummary, default=0};
};


class PASCALIMPLEMENTATION TAiOpenAiEmbeddings : public Umakerai::Embeddings::TAiEmbeddings
{
	typedef Umakerai::Embeddings::TAiEmbeddings inherited;
	
public:
	__fastcall virtual TAiOpenAiEmbeddings(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiOpenAiEmbeddings();
	virtual Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall CreateEmbedding(System::UnicodeString aInput, System::UnicodeString aUser, int aDimensions = 0xffffffff, System::UnicodeString aModel = System::UnicodeString(), System::UnicodeString aEncodingFormat = L"float");
	virtual void __fastcall ParseEmbedding(System::Json::TJSONObject* jObj);
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Openai */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_OPENAI)
using namespace Umakerai::Chat::Openai;
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
#endif	// uMakerAi_Chat_OpenAiHPP
