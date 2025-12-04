// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UMakerAi.MCPServer.Http.pas' rev: 36.00 (Windows)

#ifndef UMakerAi_MCPServer_HttpHPP
#define UMakerAi_MCPServer_HttpHPP

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
#include <IdContext.hpp>
#include <IdCustomHTTPServer.hpp>
#include <IdHTTPServer.hpp>
#include <uMakerAi.MCPServer.Core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Mcpserver
{
namespace Http
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiMCPHttpServer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TValidateRequestEvent)(System::TObject* Sender, Idcustomhttpserver::TIdHTTPRequestInfo* const ARequestInfo, /* out */ Umakerai::Mcpserver::Core::TAiAuthContext &AAuthContext, /* out */ bool &AIsValid);

class PASCALIMPLEMENTATION TAiMCPHttpServer : public Umakerai::Mcpserver::Core::TAiMCPServer
{
	typedef Umakerai::Mcpserver::Core::TAiMCPServer inherited;
	
private:
	Idhttpserver::TIdHTTPServer* FHttpServer;
	TValidateRequestEvent FOnValidateRequest;
	void __fastcall HttpCommand(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	void __fastcall HandleOptionsRequest(Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	void __fastcall HandleGetRequest(Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	void __fastcall HandlePostRequest(Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	bool __fastcall VerifyAndSetCORSHeaders(Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	
public:
	__fastcall virtual TAiMCPHttpServer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiMCPHttpServer();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Http */
}	/* namespace Mcpserver */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPSERVER_HTTP)
using namespace Umakerai::Mcpserver::Http;
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
#endif	// UMakerAi_MCPServer_HttpHPP
