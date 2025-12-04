// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Agents.EngineRegistry.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Agents_EngineRegistryHPP
#define uMakerAi_Agents_EngineRegistryHPP

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
#include <System.Generics.Collections.hpp>
#include <System.Variants.hpp>
#include <System.TypInfo.hpp>
#include <uMakerAi.Agents.hpp>
#include <uMakerAi.Agents.Attributes.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Agents
{
namespace Engineregistry
{
//-- forward type declarations -----------------------------------------------
struct TToolBlueprint;
struct TToolInfo;
class DELPHICLASS TEngineRegistry;
class DELPHICLASS TAgentHandlerRegistry;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TToolBlueprint
{
public:
	System::UnicodeString ToolClassName;
	System::UnicodeString DisplayName;
	System::UnicodeString Description;
	System::UnicodeString Category;
	System::Json::TJSONObject* Schema;
};


struct DECLSPEC_DRECORD TToolInfo
{
public:
	System::TClass ToolClass;
	System::UnicodeString UnitName;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TEngineRegistry : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	static TEngineRegistry* FInstance;
	static System::Generics::Collections::TDictionary__2<System::UnicodeString,TToolInfo>* FRegisteredTools;
	
protected:
	__fastcall TEngineRegistry();
	
public:
	__fastcall virtual ~TEngineRegistry();
	/* static */ __property TEngineRegistry* Instance = {read=FInstance};
	void __fastcall RegisterTool(System::TClass ToolClass, const System::UnicodeString AUnitName);
	System::TClass __fastcall FindToolClass(const System::UnicodeString AToolClassName);
	System::UnicodeString __fastcall GetUnitForToolClass(const System::UnicodeString AToolClassName);
	System::DynamicArray<TToolBlueprint> __fastcall GetToolBlueprints();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAgentHandlerRegistry : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	static TAgentHandlerRegistry* FInstance;
	static System::Generics::Collections::TDictionary__2<System::UnicodeString,Umakerai::Agents::TAIAgentsNodeOnExecute>* FNodeHandlers;
	static System::Generics::Collections::TDictionary__2<System::UnicodeString,Umakerai::Agents::TAIAgentsLinkOnExecute>* FLinkHandlers;
	
protected:
	__fastcall TAgentHandlerRegistry();
	
public:
	__fastcall virtual ~TAgentHandlerRegistry();
	/* static */ __property TAgentHandlerRegistry* Instance = {read=FInstance};
	void __fastcall RegisterNodeHandler(const System::UnicodeString AName, Umakerai::Agents::TAIAgentsNodeOnExecute AHandler);
	void __fastcall RegisterLinkHandler(const System::UnicodeString AName, Umakerai::Agents::TAIAgentsLinkOnExecute AHandler);
	Umakerai::Agents::TAIAgentsNodeOnExecute __fastcall FindNodeHandler(const System::UnicodeString AName);
	Umakerai::Agents::TAIAgentsLinkOnExecute __fastcall FindLinkHandler(const System::UnicodeString AName);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Engineregistry */
}	/* namespace Agents */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_AGENTS_ENGINEREGISTRY)
using namespace Umakerai::Agents::Engineregistry;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_AGENTS)
using namespace Umakerai::Agents;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_Agents_EngineRegistryHPP
