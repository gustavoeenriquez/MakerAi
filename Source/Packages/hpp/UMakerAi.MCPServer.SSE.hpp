// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UMakerAi.MCPServer.SSE.pas' rev: 36.00 (Windows)

#ifndef UMakerAi_MCPServer_SSEHPP
#define UMakerAi_MCPServer_SSEHPP

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
#include <System.SyncObjs.hpp>
#include <IdContext.hpp>
#include <IdCustomHTTPServer.hpp>
#include <IdHTTPServer.hpp>
#include <IdGlobal.hpp>
#include <uMakerAi.MCPServer.Core.hpp>
#include <System.Types.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Mcpserver
{
namespace Sse
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TMCPSSESession;
class DELPHICLASS TAiMCPSSEHttpServer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TMCPSSESession : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString SessionID;
	System::Generics::Collections::TThreadedQueue__1<System::UnicodeString>* Outbox;
	System::TDateTime LastActivity;
	__fastcall TMCPSSESession(const System::UnicodeString AID);
	__fastcall virtual ~TMCPSSESession();
};


class PASCALIMPLEMENTATION TAiMCPSSEHttpServer : public Umakerai::Mcpserver::Core::TAiMCPServer
{
	typedef Umakerai::Mcpserver::Core::TAiMCPServer inherited;
	
private:
	Idhttpserver::TIdHTTPServer* FHttpServer;
	System::Generics::Collections::TObjectDictionary__2<System::UnicodeString,TMCPSSESession*>* FSessions;
	System::Syncobjs::TCriticalSection* FSessionsLock;
	System::UnicodeString FSseEndpoint;
	System::UnicodeString FMessagesEndpoint;
	void __fastcall OnCommandGet(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	void __fastcall HandleSSEConnection(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	void __fastcall HandlePostMessage(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	TMCPSSESession* __fastcall GetOrCreateSession(const System::UnicodeString AID);
	System::UnicodeString __fastcall GenerateSessionID();
	void __fastcall CleanUpExpiredSessions();
	bool __fastcall VerifyAndSetCORSHeaders(Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	
public:
	__fastcall virtual TAiMCPSSEHttpServer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiMCPSSEHttpServer();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
	void __fastcall PushNotification(const System::UnicodeString ASessionID, const System::UnicodeString ANotificationJson);
	
__published:
	__property System::UnicodeString SseEndpoint = {read=FSseEndpoint, write=FSseEndpoint};
	__property System::UnicodeString MessagesEndpoint = {read=FMessagesEndpoint, write=FMessagesEndpoint};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Sse */
}	/* namespace Mcpserver */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPSERVER_SSE)
using namespace Umakerai::Mcpserver::Sse;
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
#endif	// UMakerAi_MCPServer_SSEHPP
