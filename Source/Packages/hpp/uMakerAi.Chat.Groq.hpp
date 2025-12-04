// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.Groq.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_GroqHPP
#define uMakerAi_Chat_GroqHPP

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
#include <uMakerAi.Embeddings.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Embeddings.core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Groq
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiGroqChat;
class DELPHICLASS TAiGroqEmbeddings;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAiReasoningFormat : unsigned char { rfAuto, rfParsed, rfRaw, rfHidden };

enum DECLSPEC_DENUM TAiReasoningEffort : unsigned char { reAuto, reNone, reDefault };

class PASCALIMPLEMENTATION TAiGroqChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
private:
	TAiReasoningFormat FReasoningFormat;
	TAiReasoningEffort FReasoningEffort;
	
protected:
	virtual System::UnicodeString __fastcall InitChatCompletions();
	
public:
	__fastcall virtual TAiGroqChat(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiGroqChat();
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
};


class PASCALIMPLEMENTATION TAiGroqEmbeddings : public Umakerai::Embeddings::TAiEmbeddings
{
	typedef Umakerai::Embeddings::TAiEmbeddings inherited;
	
public:
	virtual Umakerai::Embeddings::Core::TAiEmbeddingData __fastcall CreateEmbedding(System::UnicodeString Input, System::UnicodeString User, int Dimensions = 0x600, System::UnicodeString Model = L"Llama3-8b-8192", System::UnicodeString EncodingFormat = L"float");
public:
	/* TAiEmbeddings.Create */ inline __fastcall virtual TAiGroqEmbeddings(System::Classes::TComponent* aOwner) : Umakerai::Embeddings::TAiEmbeddings(aOwner) { }
	
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiGroqEmbeddings() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Groq */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_GROQ)
using namespace Umakerai::Chat::Groq;
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
#endif	// uMakerAi_Chat_GroqHPP
