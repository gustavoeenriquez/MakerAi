// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UMakerAi.MCPServer.Stdio.pas' rev: 36.00 (Windows)

#ifndef UMakerAi_MCPServer_StdioHPP
#define UMakerAi_MCPServer_StdioHPP

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
#include <System.SyncObjs.hpp>
#include <System.Threading.hpp>
#include <System.AnsiStrings.hpp>
#include <System.IOUtils.hpp>
#include <uMakerAi.MCPServer.Core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Mcpserver
{
namespace Stdio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStdioWorkerThread;
class DELPHICLASS TAiMCPStdioServer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStdioWorkerThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TAiMCPStdioServer* FServer;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TStdioWorkerThread(TAiMCPStdioServer* AServer);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TStdioWorkerThread() { }
	
};


class PASCALIMPLEMENTATION TAiMCPStdioServer : public Umakerai::Mcpserver::Core::TAiMCPServer
{
	typedef Umakerai::Mcpserver::Core::TAiMCPServer inherited;
	
private:
	TStdioWorkerThread* FWorkerThread;
	System::Syncobjs::TCriticalSection* FOutputLock;
	void __fastcall ProcessRequest(const System::UnicodeString ARequestJson);
	void __fastcall SendResponse(const System::UnicodeString AResponseJson);
	
public:
	__fastcall virtual TAiMCPStdioServer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiMCPStdioServer();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Stdio */
}	/* namespace Mcpserver */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPSERVER_STDIO)
using namespace Umakerai::Mcpserver::Stdio;
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
#endif	// UMakerAi_MCPServer_StdioHPP
