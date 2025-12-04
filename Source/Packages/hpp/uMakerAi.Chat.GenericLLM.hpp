// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.GenericLLM.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_GenericLLMHPP
#define uMakerAi_Chat_GenericLLMHPP

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
namespace Genericllm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiGenericChat;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAiGenericChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
public:
	__fastcall virtual TAiGenericChat(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiGenericChat();
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Genericllm */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_GENERICLLM)
using namespace Umakerai::Chat::Genericllm;
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
#endif	// uMakerAi_Chat_GenericLLMHPP
