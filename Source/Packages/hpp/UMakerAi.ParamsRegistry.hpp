// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UMakerAi.ParamsRegistry.pas' rev: 36.00 (Windows)

#ifndef UMakerAi_ParamsRegistryHPP
#define UMakerAi_ParamsRegistryHPP

#pragma delphiheader begin
#pragma option push
#if defined(__BORLANDC__) && !defined(__clang__)
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#endif
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <uMakerAi.Chat.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Paramsregistry
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiChatFactory;
//-- type declarations -------------------------------------------------------
typedef System::TMetaClass* TAiChatClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiChatFactory : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	static TAiChatFactory* FInstance;
	static System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiChatClass>* FRegisteredClasses;
	static System::Generics::Collections::TDictionary__2<System::UnicodeString,System::Classes::TStringList*>* FUserParams;
	static System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>* FCustomModels;
	static System::UnicodeString __fastcall GetCompositeKey(const System::UnicodeString DriverName, const System::UnicodeString ModelName);
	
public:
	__fastcall TAiChatFactory();
	__fastcall virtual ~TAiChatFactory();
	__classmethod TAiChatFactory* __fastcall Instance();
	void __fastcall RegisterDriver(TAiChatClass AClass);
	void __fastcall GetDriverParams(const System::UnicodeString DriverName, const System::UnicodeString ModelName, System::Classes::TStrings* Params, bool ExpandVariables = true);
	Umakerai::Chat::TAiChat* __fastcall CreateDriver(const System::UnicodeString DriverName);
	System::DynamicArray<System::UnicodeString> __fastcall GetRegisteredDrivers();
	bool __fastcall HasDriver(const System::UnicodeString DriverName);
	void __fastcall RegisterUserParam(const System::UnicodeString DriverName, const System::UnicodeString ModelName, const System::UnicodeString ParamName, const System::UnicodeString ParamValue)/* overload */;
	void __fastcall RegisterUserParam(const System::UnicodeString DriverName, const System::UnicodeString ParamName, const System::UnicodeString ParamValue)/* overload */;
	void __fastcall ClearRegisterParams(const System::UnicodeString DriverName, System::UnicodeString ModelName = System::UnicodeString());
	void __fastcall RegisterCustomModel(const System::UnicodeString DriverName, const System::UnicodeString CustomModelName, const System::UnicodeString ModelBaseName);
	System::UnicodeString __fastcall GetBaseModel(const System::UnicodeString DriverName, const System::UnicodeString CustomModel);
	System::DynamicArray<System::UnicodeString> __fastcall GetCustomModels(const System::UnicodeString DriverName);
	bool __fastcall HasCustomModel(const System::UnicodeString DriverName, const System::UnicodeString CustomModelName);
	void __fastcall ClearCustomModels(const System::UnicodeString DriverName);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Paramsregistry */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_PARAMSREGISTRY)
using namespace Umakerai::Paramsregistry;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UMakerAi_ParamsRegistryHPP
