// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Prompts.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_PromptsHPP
#define uMakerAi_PromptsHPP

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

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Prompts
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiPromptItem;
class DELPHICLASS TAiPrompts;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiPromptItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString fNombre;
	System::Classes::TStrings* FString;
	System::UnicodeString fDescripcion;
	System::Classes::TStrings* __fastcall GetString();
	
protected:
	void __fastcall SetStrings(System::Classes::TStrings* aValue);
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TAiPromptItem(System::Classes::TCollection* Collection);
	
__published:
	__property System::UnicodeString Nombre = {read=fNombre, write=fNombre};
	__property System::UnicodeString Descripcion = {read=fDescripcion, write=fDescripcion};
	__property System::Classes::TStrings* Strings = {read=GetString, write=SetStrings};
public:
	/* TCollectionItem.Destroy */ inline __fastcall virtual ~TAiPromptItem() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiPrompts : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Classes::TCollection* FItems;
	
public:
	__fastcall virtual TAiPrompts(System::Classes::TComponent* aOwner);
	int __fastcall IndexOf(System::UnicodeString Nombre);
	System::UnicodeString __fastcall GetString(System::UnicodeString Nombre);
	TAiPromptItem* __fastcall AddString(System::UnicodeString Nombre, System::UnicodeString Data);
	System::UnicodeString __fastcall GetTemplate(System::UnicodeString Nombre, System::UnicodeString *Params, const System::NativeInt Params_High)/* overload */;
	System::UnicodeString __fastcall GetTemplate(System::UnicodeString Nombre, System::Classes::TStringList* Params)/* overload */;
	System::UnicodeString __fastcall GetTemplate(System::UnicodeString Nombre, System::Json::TJSONObject* Params)/* overload */;
	
__published:
	__property System::Classes::TCollection* Items = {read=FItems, write=FItems};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiPrompts() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Prompts */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_PROMPTS)
using namespace Umakerai::Prompts;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_PromptsHPP
