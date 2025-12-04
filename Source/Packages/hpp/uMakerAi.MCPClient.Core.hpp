// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.MCPClient.Core.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_MCPClient_CoreHPP
#define uMakerAi_MCPClient_CoreHPP

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
#include <System.IOUtils.hpp>
#include <System.Net.URLClient.hpp>
#include <System.NetEncoding.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.Mime.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <System.Threading.hpp>
#include <System.Diagnostics.hpp>
#include <System.Types.hpp>
#include <Winapi.Windows.hpp>
#include <uMakerAi.Utils.system.hpp>
#include <uMakerAi.Core.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Mcpclient
{
namespace Core
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS EMCPClientException;
class DELPHICLASS TMCPClientCustom;
class DELPHICLASS TMCPClientStdIo;
class DELPHICLASS TMCPClientHttp;
class DELPHICLASS TMCPClientMakerAi;
class DELPHICLASS TMCPClientSSE;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION EMCPClientException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EMCPClientException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EMCPClientException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall EMCPClientException(System::NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EMCPClientException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EMCPClientException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall EMCPClientException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall EMCPClientException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EMCPClientException(const System::UnicodeString Msg, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EMCPClientException(System::NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EMCPClientException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EMCPClientException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EMCPClientException(System::NativeUInt Ident, const System::TVarRec *Args, const System::NativeInt Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EMCPClientException() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TMCPStreamMessageEvent)(System::TObject* Sender, const System::UnicodeString EventType, const System::UnicodeString Data);

class PASCALIMPLEMENTATION TMCPClientCustom : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FName;
	Umakerai::Core::TMCPLogEvent FOnLog;
	Umakerai::Core::TMCPStatusEvent FOnStatusUpdate;
	Umakerai::Core::TToolTransportType FTransportType;
	bool FInitialized;
	bool FEnabled;
	System::Classes::TStrings* FTools;
	bool FAvailable;
	System::Classes::TStrings* FParams;
	System::Classes::TStrings* FEnvVars;
	System::Classes::TStrings* FDisabledFunctions;
	System::UnicodeString FURL;
	Umakerai::Utils::System::TInteractiveProcessInfo* FServerProcess;
	bool FOwnsServerProcess;
	TMCPStreamMessageEvent FOnStreamMessage;
	void __fastcall SetTransportType(const Umakerai::Core::TToolTransportType Value);
	void __fastcall SetAvailable(const bool Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetInitialized(const bool Value);
	void __fastcall SetParams(System::Classes::TStrings* const Value);
	System::Classes::TStrings* __fastcall GetParams();
	System::Classes::TStrings* __fastcall GetEnvVars();
	void __fastcall SetEnvVars(System::Classes::TStrings* const Value);
	void __fastcall SetURL(const System::UnicodeString Value);
	
protected:
	System::UnicodeString FLastError;
	bool FBusy;
	virtual void __fastcall DoLog(const System::UnicodeString Msg);
	virtual void __fastcall DoStatusUpdate(const System::UnicodeString StatusMsg);
	bool __fastcall IsBinaryContentType(const System::UnicodeString ContentType);
	System::Json::TJSONObject* __fastcall ProcessAndExtractMedia(System::Json::TJSONObject* const AJsonResult, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia);
	virtual bool __fastcall InternalStartLocalServerProcess();
	virtual void __fastcall InternalStopLocalServerProcess();
	virtual void __fastcall DoStreamMessage(const System::UnicodeString EventType, const System::UnicodeString Data);
	
public:
	__fastcall virtual TMCPClientCustom(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMCPClientCustom();
	System::UnicodeString __fastcall GetParamByName(System::UnicodeString ParamName);
	virtual System::Classes::TStringList* __fastcall GetDefaultParams();
	virtual bool __fastcall Initialize();
	virtual System::Json::TJSONObject* __fastcall ListTools();
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Json::TJSONObject* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Classes::TStrings* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property Umakerai::Core::TToolTransportType TransportType = {read=FTransportType, write=SetTransportType, nodefault};
	__property System::Classes::TStrings* Tools = {read=FTools};
	__property bool Initialized = {read=FInitialized, write=SetInitialized, nodefault};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property bool Available = {read=FAvailable, write=SetAvailable, nodefault};
	__property System::Classes::TStrings* Params = {read=GetParams, write=SetParams};
	__property System::Classes::TStrings* EnvVars = {read=GetEnvVars, write=SetEnvVars};
	__property System::UnicodeString URL = {read=FURL, write=SetURL};
	__property Umakerai::Core::TMCPLogEvent OnLog = {read=FOnLog, write=FOnLog};
	__property Umakerai::Core::TMCPStatusEvent OnStatusUpdate = {read=FOnStatusUpdate, write=FOnStatusUpdate};
	__property TMCPStreamMessageEvent OnStreamMessage = {read=FOnStreamMessage, write=FOnStreamMessage};
};


class PASCALIMPLEMENTATION TMCPClientStdIo : public TMCPClientCustom
{
	typedef TMCPClientCustom inherited;
	
private:
	Umakerai::Utils::System::TInteractiveProcessInfo* FInteractiveProcess;
	System::Classes::TThread* FReadThread;
	System::Generics::Collections::TThreadedQueue__1<System::Json::TJSONObject*>* FIncomingMessages;
	bool FIsRunning;
	int FRequestIDCounter;
	void __fastcall InternalStartServerProcess();
	void __fastcall InternalStopServerProcess();
	System::Json::TJSONObject* __fastcall InternalInitialize();
	void __fastcall InternalSendInitializedNotification();
	System::Json::TJSONObject* __fastcall InternalListTools();
	System::Json::TJSONObject* __fastcall InternalCallTool(const System::UnicodeString AToolName, System::Json::TJSONObject* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia);
	void __fastcall InternalSendRawMessage(const System::UnicodeString AJsonString);
	System::Json::TJSONObject* __fastcall InternalReceiveJSONResponse(int AExpectedID, unsigned ATimeoutMs = (unsigned)(0x2710));
	bool __fastcall IsServerRunning();
	
public:
	__fastcall virtual TMCPClientStdIo(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMCPClientStdIo();
	void __fastcall ReadProcessOutput();
	virtual System::Json::TJSONObject* __fastcall ListTools();
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Json::TJSONObject* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Classes::TStrings* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
};


class PASCALIMPLEMENTATION TMCPClientHttp : public TMCPClientCustom
{
	typedef TMCPClientCustom inherited;
	
private:
	System::Net::Httpclientcomponent::TNetHTTPClient* FHttpClient;
	int FRequestIDCounter;
	System::Json::TJSONObject* FServerCapabilities;
	void __fastcall InternalPerformMCPInitialize();
	void __fastcall InternalSendInitializedNotification();
	System::Json::TJSONObject* __fastcall InternalSendRequest(const System::UnicodeString AMethod, System::Json::TJSONObject* AParams);
	
public:
	__fastcall virtual TMCPClientHttp(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMCPClientHttp();
	virtual bool __fastcall Initialize();
	virtual System::Json::TJSONObject* __fastcall ListTools();
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Json::TJSONObject* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Classes::TStrings* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
};


class PASCALIMPLEMENTATION TMCPClientMakerAi : public TMCPClientCustom
{
	typedef TMCPClientCustom inherited;
	
private:
	System::Net::Httpclientcomponent::TNetHTTPClient* FHttpClient;
	System::Json::TJSONObject* __fastcall InternalSendRequest(const System::UnicodeString AMethodName, const System::UnicodeString AHttpVerb, System::Classes::TStream* ABodyStream);
	
public:
	__fastcall virtual TMCPClientMakerAi(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMCPClientMakerAi();
	virtual System::Json::TJSONObject* __fastcall ListTools();
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Json::TJSONObject* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Classes::TStrings* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
};


class PASCALIMPLEMENTATION TMCPClientSSE : public TMCPClientCustom
{
	typedef TMCPClientCustom inherited;
	
private:
	System::Net::Httpclientcomponent::TNetHTTPClient* FHttpClient;
	System::Generics::Collections::TThreadedQueue__1<System::Json::TJSONObject*>* FIncomingMessages;
	System::UnicodeString FPostEndpoint;
	int FRequestIDCounter;
	bool FIsConnected;
	bool FStopRequested;
	System::UnicodeString FBuffer;
	void __fastcall DoReceiveDataEx(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, void * AChunk, unsigned AChunkLength, bool &ABort);
	void __fastcall DoRequestCompleted(System::TObject* const Sender, const System::Net::Httpclient::_di_IHTTPResponse AResponse);
	void __fastcall DoRequestError(System::TObject* const Sender, const System::UnicodeString AError);
	void __fastcall DoRequestException(System::TObject* const Sender, System::Sysutils::Exception* const AException);
	void __fastcall ProcessBuffer();
	void __fastcall ProcessSSELine(const System::UnicodeString ALine);
	System::Json::TJSONObject* __fastcall InternalSendRequest(const System::UnicodeString AMethod, System::Json::TJSONObject* AParams);
	System::Json::TJSONObject* __fastcall InternalReceiveJSONResponse(int AExpectedID, unsigned ATimeoutMs = (unsigned)(0x3a98));
	bool __fastcall WaitForInitialization(int ATimeout);
	void __fastcall StartEventStream();
	void __fastcall StopEventStream();
	
public:
	__fastcall virtual TMCPClientSSE(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TMCPClientSSE();
	virtual bool __fastcall Initialize();
	virtual System::Json::TJSONObject* __fastcall ListTools();
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Json::TJSONObject* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
	virtual System::Json::TJSONObject* __fastcall CallTool(const System::UnicodeString AToolName, System::Classes::TStrings* AArguments, System::Generics::Collections::TObjectList__1<Umakerai::Core::TAiMediaFile*>* AExtractedMedia)/* overload */;
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Core */
}	/* namespace Mcpclient */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPCLIENT_CORE)
using namespace Umakerai::Mcpclient::Core;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPCLIENT)
using namespace Umakerai::Mcpclient;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_MCPClient_CoreHPP
