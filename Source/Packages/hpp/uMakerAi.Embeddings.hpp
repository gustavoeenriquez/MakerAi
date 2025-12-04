// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Embeddings.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_EmbeddingsHPP
#define uMakerAi_EmbeddingsHPP

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
#include <uMakerAi.Embeddings.core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Embeddings
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiEmbeddings;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAiEmbeddings : public Umakerai::Embeddings::Core::TAiEmbeddingsCore
{
	typedef Umakerai::Embeddings::Core::TAiEmbeddingsCore inherited;
	
private:
	void __fastcall SetApiKey(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetApiKey();
	void __fastcall SetUrl(const System::UnicodeString Value);
	
protected:
	System::UnicodeString FApiKey;
	System::UnicodeString FUrl;
	
public:
	__fastcall virtual TAiEmbeddings(System::Classes::TComponent* aOwner);
	virtual void __fastcall ParseEmbedding(System::Json::TJSONObject* JObj);
	virtual Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall CreateEmbedding(System::UnicodeString aInput, System::UnicodeString aUser, int aDimensions = 0xffffffff, System::UnicodeString aModel = System::UnicodeString(), System::UnicodeString aEncodingFormat = L"float");
	
__published:
	__property System::UnicodeString ApiKey = {read=GetApiKey, write=SetApiKey};
	__property System::UnicodeString Url = {read=FUrl, write=SetUrl};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiEmbeddings() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Embeddings */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_EMBEDDINGS)
using namespace Umakerai::Embeddings;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_EmbeddingsHPP
