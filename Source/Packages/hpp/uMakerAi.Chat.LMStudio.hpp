// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.LMStudio.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_LMStudioHPP
#define uMakerAi_Chat_LMStudioHPP

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
#include <System.JSON.hpp>
#include <System.StrUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Net.URLClient.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <REST.Types.hpp>
#include <REST.Client.hpp>
#include <UMakerAi.ParamsRegistry.hpp>
#include <uMakerAi.Chat.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Embeddings.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Lmstudio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiLMStudioChat;
class DELPHICLASS TAiLMStudioEmbeddings;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAiLMStudioChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
public:
	__fastcall virtual TAiLMStudioChat(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiLMStudioChat();
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
};


class PASCALIMPLEMENTATION TAiLMStudioEmbeddings : public Umakerai::Embeddings::TAiEmbeddings
{
	typedef Umakerai::Embeddings::TAiEmbeddings inherited;
	
public:
	__fastcall virtual TAiLMStudioEmbeddings(System::Classes::TComponent* aOwner);
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiLMStudioEmbeddings() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Lmstudio */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_LMSTUDIO)
using namespace Umakerai::Chat::Lmstudio;
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
#endif	// uMakerAi_Chat_LMStudioHPP
