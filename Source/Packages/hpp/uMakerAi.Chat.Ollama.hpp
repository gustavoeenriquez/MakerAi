// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.Ollama.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_OllamaHPP
#define uMakerAi_Chat_OllamaHPP

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
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.Utils.CodeExtractor.hpp>
#include <uMakerAi.Embeddings.core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Ollama
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiOllamaChat;
class DELPHICLASS TAiOllamaEmbeddings;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAiOllamaChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
private:
	System::UnicodeString Fkeep_alive;
	void __fastcall Setkeep_alive(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall OnInternalReceiveData(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &AAbort);
	virtual System::UnicodeString __fastcall InitChatCompletions();
	virtual System::UnicodeString __fastcall InternalRunCompletions(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual void __fastcall ParseChat(System::Json::TJSONObject* JObj, Umakerai::Chat::TAiChatMessage* ResMsg);
	virtual Umakerai::Core::TAiToolsFunctions* __fastcall ExtractToolCallFromJson(System::Json::TJSONArray* jChoices);
	
public:
	__classmethod virtual System::Classes::TStringList* __fastcall GetModels(System::UnicodeString aApiKey, System::UnicodeString aUrl = System::UnicodeString())/* overload */;
	__fastcall virtual TAiOllamaChat(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiOllamaChat();
	virtual System::Json::TJSONArray* __fastcall GetMessages();
	void __fastcall PullModel(const System::UnicodeString aModelName);
	void __fastcall CreateModel(const System::UnicodeString aNewModelName, const System::UnicodeString aModelfileContent);
	System::Json::TJSONObject* __fastcall ShowModelInfo(const System::UnicodeString aModelName);
	void __fastcall CopyModel(const System::UnicodeString aSourceModel, const System::UnicodeString aDestinationModel);
	void __fastcall DeleteModel(const System::UnicodeString aModelName);
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
	
__published:
	__property System::UnicodeString keep_alive = {read=Fkeep_alive, write=Setkeep_alive};
	/* Hoisted overloads: */
	
public:
	inline System::Classes::TStringList* __fastcall  GetModels(){ return Umakerai::Chat::TAiChat::GetModels(); }
	
};


class PASCALIMPLEMENTATION TAiOllamaEmbeddings : public Umakerai::Embeddings::TAiEmbeddings
{
	typedef Umakerai::Embeddings::TAiEmbeddings inherited;
	
public:
	__fastcall virtual TAiOllamaEmbeddings(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAiOllamaEmbeddings();
	virtual Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall CreateEmbedding(System::UnicodeString aInput, System::UnicodeString aUser, int aDimensions = 0xffffffff, System::UnicodeString aModel = System::UnicodeString(), System::UnicodeString aEncodingFormat = L"float");
	virtual void __fastcall ParseEmbedding(System::Json::TJSONObject* JObj);
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Ollama */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_OLLAMA)
using namespace Umakerai::Chat::Ollama;
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
#endif	// uMakerAi_Chat_OllamaHPP
