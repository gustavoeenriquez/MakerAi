// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Agents.Attributes.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Agents_AttributesHPP
#define uMakerAi_Agents_AttributesHPP

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
#include <System.Rtti.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Agents
{
namespace Attributes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAgentCustomAttribute;
class DELPHICLASS TToolAttribute;
class DELPHICLASS TToolParameterAttribute;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAgentCustomAttribute : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TAgentCustomAttribute() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAgentCustomAttribute() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TToolAttribute : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FDescription;
	System::UnicodeString FCategory;
	
public:
	__fastcall TToolAttribute(const System::UnicodeString AName, const System::UnicodeString ADescription, const System::UnicodeString ACategory);
	__property System::UnicodeString Name = {read=FName};
	__property System::UnicodeString Description = {read=FDescription};
	__property System::UnicodeString Category = {read=FCategory};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TToolAttribute() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TToolParameterAttribute : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	System::UnicodeString FDisplayName;
	System::UnicodeString FHint;
	System::UnicodeString FDefaultValue;
	
public:
	__fastcall TToolParameterAttribute(const System::UnicodeString ADisplayName, const System::UnicodeString AHint, const System::UnicodeString ADefaultValue);
	__property System::UnicodeString DisplayName = {read=FDisplayName};
	__property System::UnicodeString Hint = {read=FHint};
	__property System::UnicodeString DefaultValue = {read=FDefaultValue};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TToolParameterAttribute() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Attributes */
}	/* namespace Agents */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_AGENTS_ATTRIBUTES)
using namespace Umakerai::Agents::Attributes;
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
#endif	// uMakerAi_Agents_AttributesHPP
