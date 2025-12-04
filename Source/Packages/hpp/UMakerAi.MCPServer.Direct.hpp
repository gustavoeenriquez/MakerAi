// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UMakerAi.MCPServer.Direct.pas' rev: 36.00 (Windows)

#ifndef UMakerAi_MCPServer_DirectHPP
#define UMakerAi_MCPServer_DirectHPP

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
#include <uMakerAi.MCPServer.Core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Mcpserver
{
namespace Direct
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiMCPDirectConnection;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAiMCPDirectConnection : public Umakerai::Mcpserver::Core::TAiMCPServer
{
	typedef Umakerai::Mcpserver::Core::TAiMCPServer inherited;
	
private:
	int FRequestIDCounter;
	System::Json::TJSONObject* __fastcall ExecuteDirectRequest(const System::UnicodeString AMethod, System::Json::TJSONObject* AParams);
	
public:
	__fastcall virtual TAiMCPDirectConnection(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiMCPDirectConnection();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
	System::Json::TJSONObject* __fastcall ListTools();
	System::Json::TJSONObject* __fastcall ListResources();
	System::Json::TJSONObject* __fastcall ReadResource(const System::UnicodeString AURI);
	System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Json::TJSONObject* AArguments)/* overload */;
	System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Classes::TStrings* AArguments)/* overload */;
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Direct */
}	/* namespace Mcpserver */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPSERVER_DIRECT)
using namespace Umakerai::Mcpserver::Direct;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPSERVER)
using namespace Umakerai::Mcpserver;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UMakerAi_MCPServer_DirectHPP
