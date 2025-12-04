// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.Gemini.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_GeminiHPP
#define uMakerAi_Chat_GeminiHPP

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
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.Threading.hpp>
#include <System.NetConsts.hpp>
#include <System.Variants.hpp>
#include <System.Net.Mime.hpp>
#include <System.IOUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <System.NetEncoding.hpp>
#include <System.JSON.hpp>
#include <System.StrUtils.hpp>
#include <System.Net.URLClient.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <REST.Json.hpp>
#include <REST.Types.hpp>
#include <REST.Client.hpp>
#include <UMakerAi.ParamsRegistry.hpp>
#include <uMakerAi.Chat.hpp>
#include <uMakerAi.Tools.Functions.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Utils.PcmToWav.hpp>
#include <uMakerAi.Utils.CodeExtractor.hpp>
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.Embeddings.core.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Gemini
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiMediaFileGemini;
class DELPHICLASS TAiGeminiChat;
class DELPHICLASS TAiGeminiEmbeddings;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiMediaFileGemini : public Umakerai::Core::TAiMediaFile
{
	typedef Umakerai::Core::TAiMediaFile inherited;
	
protected:
	virtual void __fastcall DownloadFileFromUrl(System::UnicodeString Url);
public:
	/* TAiMediaFile.Create */ inline __fastcall TAiMediaFileGemini() : Umakerai::Core::TAiMediaFile() { }
	/* TAiMediaFile.Destroy */ inline __fastcall virtual ~TAiMediaFileGemini() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiGeminiChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
private:
	int FThinkingBudget;
	bool FIncludeThoughts;
	Umakerai::Core::TAiMediaResolution FMediaResolution;
	System::Generics::Collections::TObjectDictionary__2<Umakerai::Chat::TAiChatMessage*,System::Classes::TStringList*>* FThoughtSignatures;
	void __fastcall AddThoughtSignature(Umakerai::Chat::TAiChatMessage* Msg, const System::UnicodeString Signature);
	System::Json::TJSONArray* __fastcall GetToolJSon();
	System::Json::TJSONObject* __fastcall GetSystemInstructionJson();
	void __fastcall SetThinkingBudget(const int Value);
	void __fastcall SetIncludeThoughts(const bool Value);
	System::UnicodeString __fastcall MediaTypeToResolutionString(Umakerai::Core::TAiMediaResolution Res);
	void __fastcall InternalCompleteRequest();
	HIDESBASE void __fastcall SetMediaResolution(const Umakerai::Core::TAiMediaResolution Value);
	
protected:
	virtual void __fastcall OnInternalReceiveData(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &AAbort);
	virtual System::UnicodeString __fastcall InternalRunSpeechGeneration(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunImageGeneration(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunImageVideoGeneration(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunCompletions(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunTranscription(Umakerai::Core::TAiMediaFile* aMediaFile, Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunImageDescription(Umakerai::Core::TAiMediaFile* aMediaFile, Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual Umakerai::Chat::TAiChatMessage* __fastcall InternalAddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole, System::UnicodeString aToolCallId = System::UnicodeString(), System::UnicodeString aFunctionName = System::UnicodeString())/* overload */;
	virtual Umakerai::Chat::TAiChatMessage* __fastcall InternalAddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole, Umakerai::Core::TAiMediaFilesArray aMediaFiles)/* overload */;
	virtual Umakerai::Chat::TAiChatMessage* __fastcall InternalAddMessage(Umakerai::Chat::TAiChatMessage* aMsg)/* overload */;
	void __fastcall InternalExtractCodeFiles(const System::UnicodeString AText, Umakerai::Chat::TAiChatMessage* AMessage);
	virtual System::UnicodeString __fastcall InitChatCompletions();
	virtual void __fastcall ParseChat(System::Json::TJSONObject* jObj, Umakerai::Chat::TAiChatMessage* ResMsg);
	virtual Umakerai::Core::TAiToolsFunctions* __fastcall ExtractToolCallFromJson(System::Json::TJSONArray* jChoices);
	System::Json::TJSONObject* __fastcall BuildSpeechConfigJson();
	void __fastcall ParseGroundingMetadata(System::Json::TJSONObject* jCandidate, Umakerai::Chat::TAiChatMessage* ResMsg);
	virtual void __fastcall OnRequestCompletedEvent(System::TObject* const Sender, const System::Net::Httpclient::_di_IHTTPResponse aResponse);
	
public:
	__fastcall virtual TAiGeminiChat(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiGeminiChat();
	virtual System::Json::TJSONArray* __fastcall GetMessages();
	__classmethod virtual System::Classes::TStringList* __fastcall GetModels(System::UnicodeString aApiKey, System::UnicodeString aUrl = System::UnicodeString())/* overload */;
	virtual System::UnicodeString __fastcall UploadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DownLoadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DeleteFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall CheckFileState(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall UploadFileToCache(Umakerai::Core::TAiMediaFile* aMediaFile, int aTTL_Seconds = 0xe10);
	System::Json::TJSONObject* __fastcall RetrieveCache(const System::UnicodeString CacheName);
	System::Json::TJSONObject* __fastcall ListCaches();
	bool __fastcall DeleteCache(const System::UnicodeString CacheName);
	System::UnicodeString __fastcall CreateBatchJob(const System::UnicodeString SourceFileUri, const System::UnicodeString OutputUri = System::UnicodeString());
	System::UnicodeString __fastcall GetBatchJob(const System::UnicodeString JobName);
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
	
__published:
	__property int ThinkingBudget = {read=FThinkingBudget, write=SetThinkingBudget, default=0};
	__property bool IncludeThoughts = {read=FIncludeThoughts, write=SetIncludeThoughts, default=0};
	__property Umakerai::Core::TAiMediaResolution MediaResolution = {read=FMediaResolution, write=SetMediaResolution, nodefault};
	/* Hoisted overloads: */
	
public:
	inline System::Classes::TStringList* __fastcall  GetModels(){ return Umakerai::Chat::TAiChat::GetModels(); }
	
};


class PASCALIMPLEMENTATION TAiGeminiEmbeddings : public Umakerai::Embeddings::TAiEmbeddings
{
	typedef Umakerai::Embeddings::TAiEmbeddings inherited;
	
public:
	__fastcall virtual TAiGeminiEmbeddings(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAiGeminiEmbeddings();
	virtual Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall CreateEmbedding(System::UnicodeString aInput, System::UnicodeString aUser, int aDimensions = 0xffffffff, System::UnicodeString aModel = System::UnicodeString(), System::UnicodeString aEncodingFormat = L"float");
	virtual void __fastcall ParseEmbedding(System::Json::TJSONObject* jObj);
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Gemini */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_GEMINI)
using namespace Umakerai::Chat::Gemini;
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
#endif	// uMakerAi_Chat_GeminiHPP
