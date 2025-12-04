// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.Mistral.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_MistralHPP
#define uMakerAi_Chat_MistralHPP

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
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.Embeddings.core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Mistral
{
//-- forward type declarations -----------------------------------------------
struct TMistralFile;
class DELPHICLASS TAiMistralChat;
class DELPHICLASS TAiMistralEmbeddings;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TMistralFile
{
public:
	System::UnicodeString Id;
	System::UnicodeString Object;
	__int64 Size_Bytes;
	__int64 Created_At;
	System::UnicodeString Filename;
	System::UnicodeString Purpose;
};


class PASCALIMPLEMENTATION TAiMistralChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
private:
	bool FOcrIncludeImages;
	int FDocumentImageLimit;
	int FDocumentPageLimit;
	System::Classes::TStringList* FOcrBboxAnnotationSchema;
	System::Classes::TStringList* FOcrDocumentAnnotationSchema;
	System::UnicodeString FOcrAnnotationPages;
	System::UnicodeString FOcrPagesNumbers;
	void __fastcall SetOcrIncludeImages(const bool Value);
	void __fastcall SetDocumentImageLimit(const int Value);
	void __fastcall SetDocumentPageLimit(const int Value);
	void __fastcall SetOcrAnnotationPages(const System::UnicodeString Value);
	void __fastcall SetOcrBboxAnnotationSchema(System::Classes::TStringList* const Value);
	void __fastcall SetOcrDocumentAnnotationSchema(System::Classes::TStringList* const Value);
	void __fastcall SetOcrPagesNumbers(const System::UnicodeString Value);
	
protected:
	virtual System::UnicodeString __fastcall InitChatCompletions();
	virtual System::UnicodeString __fastcall InitChatCompletionsFim(System::UnicodeString aPrompt, System::UnicodeString aSuffix);
	virtual Umakerai::Core::TAiToolsFunctions* __fastcall ExtractToolCallFromJson(System::Json::TJSONArray* jChoices);
	virtual System::UnicodeString __fastcall InternalRunPDFDescription(Umakerai::Core::TAiMediaFile* aMediaFile, Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	System::UnicodeString __fastcall ParseOcrResponse(System::Json::TJSONObject* jResponse, Umakerai::Chat::TAiChatMessage* ResMsg);
	virtual TMistralFile __fastcall RetrieveFileMetadata(System::UnicodeString aFileId);
	System::UnicodeString __fastcall GetSignedUrl(Umakerai::Core::TAiMediaFile* aMediaFile);
	System::UnicodeString __fastcall GetSignedUrlById(const System::UnicodeString aFileId);
	
public:
	__fastcall virtual TAiMistralChat(System::Classes::TComponent* Sender);
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
	virtual System::UnicodeString __fastcall UploadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DeleteFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual Umakerai::Core::TAiMediaFile* __fastcall RetrieveFile(System::UnicodeString aFileId);
	virtual System::UnicodeString __fastcall UploadFileToCache(Umakerai::Core::TAiMediaFile* aMediaFile, int aTTL_Seconds = 0xe10);
	
__published:
	__property bool OcrIncludeImages = {read=FOcrIncludeImages, write=SetOcrIncludeImages, nodefault};
	__property int DocumentImageLimit = {read=FDocumentImageLimit, write=SetDocumentImageLimit, nodefault};
	__property int DocumentPageLimit = {read=FDocumentPageLimit, write=SetDocumentPageLimit, nodefault};
	__property System::Classes::TStringList* OcrDocumentAnnotationSchema = {read=FOcrDocumentAnnotationSchema, write=SetOcrDocumentAnnotationSchema};
	__property System::Classes::TStringList* OcrBboxAnnotationSchema = {read=FOcrBboxAnnotationSchema, write=SetOcrBboxAnnotationSchema};
	__property System::UnicodeString OcrAnnotationPages = {read=FOcrAnnotationPages, write=SetOcrAnnotationPages};
	__property System::UnicodeString OcrPagesNumbers = {read=FOcrPagesNumbers, write=SetOcrPagesNumbers};
public:
	/* TAiChat.Destroy */ inline __fastcall virtual ~TAiMistralChat() { }
	
};


class PASCALIMPLEMENTATION TAiMistralEmbeddings : public Umakerai::Embeddings::TAiEmbeddings
{
	typedef Umakerai::Embeddings::TAiEmbeddings inherited;
	
public:
	__fastcall virtual TAiMistralEmbeddings(System::Classes::TComponent* aOwner);
	virtual Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall CreateEmbedding(System::UnicodeString aInput, System::UnicodeString aUser, int aDimensions = 0x600, System::UnicodeString aModel = L"mistral-embed", System::UnicodeString aEncodingFormat = L"float");
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiMistralEmbeddings() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Mistral */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_MISTRAL)
using namespace Umakerai::Chat::Mistral;
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
#endif	// uMakerAi_Chat_MistralHPP
