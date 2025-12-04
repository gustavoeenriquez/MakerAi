// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.MCPServer.Core.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_MCPServer_CoreHPP
#define uMakerAi_MCPServer_CoreHPP

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
#include <REST.Json.hpp>
#include <System.Rtti.hpp>
#include <System.StrUtils.hpp>
#include <System.ConvUtils.hpp>
#include <System.IOUtils.hpp>
#include <System.NetEncoding.hpp>
#include <System.Net.Mime.hpp>
#include <IdGlobalProtocols.hpp>
#include <System.Generics.Collections.hpp>
#include <System.TypInfo.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Mcpserver
{
namespace Core
{
//-- forward type declarations -----------------------------------------------
struct TAiAuthContext;
class DELPHICLASS TAiMCPResponseBuilder;
class DELPHICLASS TAiMCPSchemaUtils;
class DELPHICLASS TAiMCPSerializerUtils;
class DELPHICLASS AiMCPOptionalAttribute;
class DELPHICLASS AiMCPSchemaDescriptionAttribute;
class DELPHICLASS AiMCPSchemaEnumAttribute;
__interface DELPHIINTERFACE IAiMCPTool;
typedef System::DelphiInterface<IAiMCPTool> _di_IAiMCPTool;
template<typename T> class DELPHICLASS TAiMCPToolBase__1;
__interface DELPHIINTERFACE IAiMCPResource;
typedef System::DelphiInterface<IAiMCPResource> _di_IAiMCPResource;
template<typename T> class DELPHICLASS TAiMCPResourceBase__1;
__interface DELPHIINTERFACE TAiMCPToolFactory;
typedef System::DelphiInterface<TAiMCPToolFactory> _di_TAiMCPToolFactory;
__interface DELPHIINTERFACE TAiMCPResourceFactory;
typedef System::DelphiInterface<TAiMCPResourceFactory> _di_TAiMCPResourceFactory;
class DELPHICLASS TAiMCPLogicServer;
class DELPHICLASS TAiMCPServer;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TAiAuthContext
{
public:
	bool IsAuthenticated;
	System::UnicodeString UserID;
	System::UnicodeString UserName;
	System::DynamicArray<System::UnicodeString> Roles;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiMCPResponseBuilder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Json::TJSONArray* FContentArray;
	
protected:
	__fastcall TAiMCPResponseBuilder();
	
public:
	__classmethod TAiMCPResponseBuilder* __fastcall New();
	__fastcall virtual ~TAiMCPResponseBuilder();
	TAiMCPResponseBuilder* __fastcall AddText(const System::UnicodeString AText);
	TAiMCPResponseBuilder* __fastcall AddFile(const System::UnicodeString AFilePath, System::UnicodeString AFileName = System::UnicodeString());
	TAiMCPResponseBuilder* __fastcall AddFileFromStream(System::Classes::TStream* AStream, const System::UnicodeString AFileName, const System::UnicodeString AMimeType);
	System::Json::TJSONObject* __fastcall Build();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiMCPSchemaUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod System::Json::TJSONObject* __fastcall GenerateSchema(System::TClass AClass);
public:
	/* TObject.Create */ inline __fastcall TAiMCPSchemaUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAiMCPSchemaUtils() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiMCPSerializerUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	template<typename T> __classmethod T __fastcall Deserialize(System::Json::TJSONObject* JSON);
	__classmethod void __fastcall DeserializeObject(System::TObject* Instance, System::Json::TJSONObject* JSON);
public:
	/* TObject.Create */ inline __fastcall TAiMCPSerializerUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAiMCPSerializerUtils() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION AiMCPOptionalAttribute : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
public:
	/* TObject.Create */ inline __fastcall AiMCPOptionalAttribute() : System::TCustomAttribute() { }
	/* TObject.Destroy */ inline __fastcall virtual ~AiMCPOptionalAttribute() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION AiMCPSchemaDescriptionAttribute : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	System::UnicodeString FDescription;
	
public:
	__fastcall AiMCPSchemaDescriptionAttribute(const System::UnicodeString ADescription);
	__property System::UnicodeString Description = {read=FDescription};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~AiMCPSchemaDescriptionAttribute() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION AiMCPSchemaEnumAttribute : public System::TCustomAttribute
{
	typedef System::TCustomAttribute inherited;
	
private:
	System::DynamicArray<System::UnicodeString> FValues;
	
public:
	__fastcall AiMCPSchemaEnumAttribute(const System::UnicodeString *AValues, const System::NativeInt AValues_High);
	__property System::DynamicArray<System::UnicodeString> Values = {read=FValues};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~AiMCPSchemaEnumAttribute() { }
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{B1A4D0F8-9A7B-4C6C-8D1F-4B9E3A5F7C1E}") IAiMCPTool  : public System::IInterface 
{
	virtual System::UnicodeString __fastcall GetName() = 0 ;
	virtual System::UnicodeString __fastcall GetDescription() = 0 ;
	virtual System::Json::TJSONObject* __fastcall GetInputSchema() = 0 ;
	virtual System::Json::TJSONObject* __fastcall Execute(System::Json::TJSONObject* const Arguments, const TAiAuthContext &AuthContext) = 0 ;
	__property System::UnicodeString Name = {read=GetName};
	__property System::UnicodeString Description = {read=GetDescription};
	__property System::Json::TJSONObject* InputSchema = {read=GetInputSchema};
};

#pragma pack(push,4)
// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION TAiMCPToolBase__1 : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	System::UnicodeString FName;
	System::UnicodeString FDescription;
	virtual System::Json::TJSONObject* __fastcall ExecuteWithParams(const T Params, const TAiAuthContext &AuthContext) = 0 ;
	
public:
	__fastcall virtual TAiMCPToolBase__1();
	System::UnicodeString __fastcall GetName();
	System::UnicodeString __fastcall GetDescription();
	System::Json::TJSONObject* __fastcall GetInputSchema();
	System::Json::TJSONObject* __fastcall Execute(System::Json::TJSONObject* const Arguments, const TAiAuthContext &AuthContext);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TAiMCPToolBase__1() { }
	
private:
	void *__IAiMCPTool;	// IAiMCPTool 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {B1A4D0F8-9A7B-4C6C-8D1F-4B9E3A5F7C1E}
	operator _di_IAiMCPTool()
	{
		_di_IAiMCPTool intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IAiMCPTool*(void) { return (IAiMCPTool*)&__IAiMCPTool; }
	#endif
	
};

#pragma pack(pop)

__interface  INTERFACE_UUID("{E6A3B2C9-5D8F-4A1E-B9C2-7D8F9A0B1C3D}") IAiMCPResource  : public System::IInterface 
{
	virtual System::UnicodeString __fastcall GetURI() = 0 ;
	virtual System::UnicodeString __fastcall GetName() = 0 ;
	virtual System::UnicodeString __fastcall GetDescription() = 0 ;
	virtual System::UnicodeString __fastcall GetMimeType() = 0 ;
	virtual System::UnicodeString __fastcall Read() = 0 ;
	__property System::UnicodeString URI = {read=GetURI};
	__property System::UnicodeString Name = {read=GetName};
	__property System::UnicodeString Description = {read=GetDescription};
	__property System::UnicodeString MimeType = {read=GetMimeType};
};

#pragma pack(push,4)
// Template declaration generated by Delphi parameterized types is
// used only for accessing Delphi variables and fields.
// Don't instantiate with new type parameters in user code.
template<typename T> class PASCALIMPLEMENTATION TAiMCPResourceBase__1 : public System::TInterfacedObject
{
	typedef System::TInterfacedObject inherited;
	
protected:
	System::UnicodeString FURI;
	System::UnicodeString FName;
	System::UnicodeString FDescription;
	System::UnicodeString FMimeType;
	virtual T __fastcall GetResourceData() = 0 ;
	
public:
	__fastcall virtual TAiMCPResourceBase__1();
	System::UnicodeString __fastcall GetURI();
	System::UnicodeString __fastcall GetName();
	System::UnicodeString __fastcall GetDescription();
	System::UnicodeString __fastcall GetMimeType();
	System::UnicodeString __fastcall Read();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TAiMCPResourceBase__1() { }
	
private:
	void *__IAiMCPResource;	// IAiMCPResource 
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {E6A3B2C9-5D8F-4A1E-B9C2-7D8F9A0B1C3D}
	operator _di_IAiMCPResource()
	{
		_di_IAiMCPResource intf;
		this->GetInterface(intf);
		return intf;
	}
	#else
	operator IAiMCPResource*(void) { return (IAiMCPResource*)&__IAiMCPResource; }
	#endif
	
};

#pragma pack(pop)

__interface TAiMCPToolFactory  : public System::IInterface 
{
	virtual _di_IAiMCPTool __fastcall Invoke() = 0 ;
};

__interface TAiMCPResourceFactory  : public System::IInterface 
{
	virtual _di_IAiMCPResource __fastcall Invoke() = 0 ;
};

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiMCPLogicServer : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FPort;
	System::UnicodeString FHost;
	System::UnicodeString FServerName;
	System::UnicodeString FServerVersion;
	System::UnicodeString FProtocolVersion;
	bool FCorsEnabled;
	System::UnicodeString FCorsAllowedOrigins;
	System::UnicodeString FSettingsFile;
	bool FIsActive;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,_di_TAiMCPToolFactory>* FToolFactories;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,_di_TAiMCPResourceFactory>* FResourceFactories;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,_di_IAiMCPTool>* FActiveTools;
	System::Generics::Collections::TDictionary__2<System::UnicodeString,_di_IAiMCPResource>* FActiveResources;
	System::UnicodeString FUser;
	System::Json::TJSONObject* __fastcall ParseJSONRequest(const System::UnicodeString RequestBody);
	System::Rtti::TValue __fastcall ExtractRequestID(System::Json::TJSONObject* JSONRequest);
	System::Json::TJSONObject* __fastcall CreateJSONResponse(const System::Rtti::TValue &RequestID);
	void __fastcall AddRequestIDToResponse(System::Json::TJSONObject* Response, const System::Rtti::TValue &RequestID);
	System::UnicodeString __fastcall CreateErrorResponse(const System::Rtti::TValue &RequestID, int ErrorCode, const System::UnicodeString ErrorMessage);
	System::Rtti::TValue __fastcall ExecuteMethodCall(const System::UnicodeString MethodName, System::Json::TJSONObject* Params, const System::UnicodeString SessionID);
	System::Rtti::TValue __fastcall HandleCoreMethod(const System::UnicodeString Method, System::Json::TJSONObject* const Params);
	System::Rtti::TValue __fastcall HandleToolsMethod(const System::UnicodeString Method, System::Json::TJSONObject* const Params);
	System::Rtti::TValue __fastcall HandleResourcesMethod(const System::UnicodeString Method, System::Json::TJSONObject* const Params);
	System::Rtti::TValue __fastcall Core_Initialize(System::Json::TJSONObject* const Params);
	System::Rtti::TValue __fastcall Core_Ping();
	System::Rtti::TValue __fastcall Tools_ListTools();
	System::Rtti::TValue __fastcall Tools_CallTool(System::Json::TJSONObject* const Params);
	System::Rtti::TValue __fastcall Resources_ListResources();
	System::Rtti::TValue __fastcall Resources_ReadResource(System::Json::TJSONObject* const Params);
	System::Rtti::TValue __fastcall Resources_ListTemplates();
	void __fastcall SetUser(const System::UnicodeString Value);
	void __fastcall SetSettingsFile(const System::UnicodeString Value);
	
protected:
	void __fastcall LoadSettingsDefaults();
	void __fastcall CreateDefaultSettingsFile();
	void __fastcall LoadSettingsFromFile();
	
public:
	__fastcall TAiMCPLogicServer(const System::UnicodeString ASettingsFile);
	__fastcall virtual ~TAiMCPLogicServer();
	void __fastcall RegisterTool(const System::UnicodeString Name, _di_TAiMCPToolFactory Factory);
	void __fastcall RegisterResource(const System::UnicodeString URI, _di_TAiMCPResourceFactory Factory);
	void __fastcall Start();
	void __fastcall Stop();
	System::UnicodeString __fastcall ExecuteRequest(const System::UnicodeString ARequestJson, const System::UnicodeString ASessionID);
	__property System::UnicodeString SettingsFile = {read=FSettingsFile, write=SetSettingsFile};
	__property bool IsActive = {read=FIsActive, nodefault};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property System::UnicodeString ServerName = {read=FServerName, write=FServerName};
	__property System::UnicodeString ProtocolVersion = {read=FProtocolVersion, write=FProtocolVersion};
	__property bool CorsEnabled = {read=FCorsEnabled, write=FCorsEnabled, nodefault};
	__property System::UnicodeString CorsAllowedOrigins = {read=FCorsAllowedOrigins, write=FCorsAllowedOrigins};
	__property System::UnicodeString User = {read=FUser, write=SetUser};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiMCPServer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	bool FActive;
	bool FCorsEnabled;
	System::UnicodeString FEndPoint;
	System::UnicodeString FCorsAllowedOrigins;
	System::UnicodeString __fastcall GetEndpoint();
	int __fastcall GetPort();
	void __fastcall SetPort(const int Value);
	System::UnicodeString __fastcall GetUser();
	void __fastcall SetUser(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetSettingsFile();
	void __fastcall SetSettingsFile(const System::UnicodeString Value);
	
protected:
	TAiMCPLogicServer* FLogicServer;
	void __fastcall SetActive(const bool Value);
	
public:
	__fastcall virtual TAiMCPServer(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiMCPServer();
	virtual void __fastcall Start();
	virtual void __fastcall Stop();
	void __fastcall RegisterTool(const System::UnicodeString Name, _di_TAiMCPToolFactory Factory);
	void __fastcall RegisterResource(const System::UnicodeString URI, _di_TAiMCPResourceFactory Factory);
	void __fastcall LoadSettingsDefaults();
	void __fastcall CreateDefaultSettingsFile();
	void __fastcall LoadSettingsFromFile();
	__property int Port = {read=GetPort, write=SetPort, nodefault};
	__property System::UnicodeString Endpoint = {read=GetEndpoint};
	__property bool CorsEnabled = {read=FCorsEnabled, write=FCorsEnabled, nodefault};
	__property System::UnicodeString CorsAllowedOrigins = {read=FCorsAllowedOrigins, write=FCorsAllowedOrigins};
	__property TAiMCPLogicServer* LogicServer = {read=FLogicServer};
	__property bool IsActive = {read=FActive, nodefault};
	__property System::UnicodeString User = {read=GetUser, write=SetUser};
	__property System::UnicodeString SettingsFile = {read=GetSettingsFile, write=SetSettingsFile};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Core */
}	/* namespace Mcpserver */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_MCPSERVER_CORE)
using namespace Umakerai::Mcpserver::Core;
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
#endif	// uMakerAi_MCPServer_CoreHPP
