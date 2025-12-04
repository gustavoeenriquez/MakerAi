// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Tools.Functions.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Tools_FunctionsHPP
#define uMakerAi_Tools_FunctionsHPP

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
#include <System.Generics.Collections.hpp>
#include <System.JSON.hpp>
#include <REST.Json.hpp>
#include <System.IOUtils.hpp>
#include <Data.DB.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.MCPClient.Core.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Tools
{
namespace Functions
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TFunctionParamsItem;
class DELPHICLASS TFunctionParamsItems;
class DELPHICLASS TFunctionActionItem;
class DELPHICLASS TFunctionActionItems;
class DELPHICLASS TMCPClientItem;
class DELPHICLASS TMCPClientItems;
class DELPHICLASS TAiFunctions;
class DELPHICLASS TNormalizedTool;
class DELPHICLASS TJsonToolUtils;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TFunctionEvent)(System::TObject* Sender, TFunctionActionItem* FunctionAction, System::UnicodeString FunctionName, Umakerai::Core::TAiToolsFunction* ToolCall, bool &Handled);

enum DECLSPEC_DENUM TToolstype : unsigned char { tt_function, ttNone };

enum DECLSPEC_DENUM TToolsParamType : unsigned char { ptString, ptInteger, ptBoolean, ptFloat, ptDate, ptTime, ptDateTime, ptBase64 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFunctionParamsItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	System::UnicodeString FName;
	System::Classes::TCollection* FCollection;
	TToolsParamType FParamType;
	bool FRequired;
	System::Classes::TStrings* FDescription;
	System::UnicodeString FEnum;
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall SetDescription(System::Classes::TStrings* const Value);
	void __fastcall SetEnum(const System::UnicodeString Value);
	void __fastcall SetParamType(const TToolsParamType Value);
	void __fastcall SetRequired(const bool Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	virtual void __fastcall SetDisplayName(const System::UnicodeString Value);
	
public:
	__fastcall virtual TFunctionParamsItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TFunctionParamsItem();
	DYNAMIC System::UnicodeString __fastcall GetNamePath();
	System::Json::TJSONObject* __fastcall ToJSon(bool Detail = false);
	void __fastcall SetJSon(System::Json::TJSONObject* Value);
	
__published:
	__property System::UnicodeString Name = {read=FName, write=SetName};
	__property TToolsParamType ParamType = {read=FParamType, write=SetParamType, nodefault};
	__property System::Classes::TStrings* Description = {read=FDescription, write=SetDescription};
	__property System::UnicodeString Enum = {read=FEnum, write=SetEnum};
	__property bool Required = {read=FRequired, write=SetRequired, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFunctionParamsItems : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TFunctionParamsItem* operator[](int Index) { return this->Params[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	TFunctionParamsItem* __fastcall GetParams(int Index);
	void __fastcall SetParams(int Index, TFunctionParamsItem* const Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall SetItemName(System::Classes::TCollectionItem* Item);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	TFunctionParamsItem* __fastcall GetParamByName(System::UnicodeString aParamName);
	
public:
	__fastcall TFunctionParamsItems(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass);
	__fastcall virtual ~TFunctionParamsItems();
	HIDESBASE TFunctionParamsItem* __fastcall Add();
	System::Json::TJSONObject* __fastcall ToJSon(bool Detail = false);
	__property TFunctionParamsItem* Params[int Index] = {read=GetParams, write=SetParams/*, default*/};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TFunctionActionItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FName;
	TFunctionEvent FOnAction;
	System::Classes::TCollection* FCollection;
	bool FDefault;
	System::Classes::TStrings* FDescription;
	System::TObject* FTagObject;
	int FTag;
	TFunctionParamsItems* FParams;
	TToolstype FToolType;
	System::Classes::TStrings* FScript;
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetOnAction(const TFunctionEvent Value);
	void __fastcall SetDefault(const bool Value);
	void __fastcall SetFunctionDoc(System::Classes::TStrings* const Value);
	void __fastcall SetTag(const int Value);
	void __fastcall SetTagObject(System::TObject* const Value);
	void __fastcall SetToolType(const TToolstype Value);
	void __fastcall SetScript(System::Classes::TStrings* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	virtual void __fastcall SetDisplayName(const System::UnicodeString Value);
	
public:
	__fastcall virtual TFunctionActionItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TFunctionActionItem();
	DYNAMIC System::UnicodeString __fastcall GetNamePath();
	System::Json::TJSONObject* __fastcall ToJSon(bool Detail = false);
	void __fastcall SetJSon(System::Json::TJSONObject* Value);
	__property System::TObject* TagObject = {read=FTagObject, write=SetTagObject};
	
__published:
	__property bool Enabled = {read=FEnabled, write=SetEnabled, default=1};
	__property System::UnicodeString FunctionName = {read=GetDisplayName, write=SetDisplayName};
	__property TFunctionEvent OnAction = {read=FOnAction, write=SetOnAction};
	__property System::Classes::TStrings* Description = {read=FDescription, write=SetFunctionDoc};
	__property System::Classes::TStrings* Script = {read=FScript, write=SetScript};
	__property TFunctionParamsItems* Parameters = {read=FParams, write=FParams};
	__property bool Default = {read=FDefault, write=SetDefault, nodefault};
	__property int Tag = {read=FTag, write=SetTag, nodefault};
	__property TToolstype ToolType = {read=FToolType, write=SetToolType, nodefault};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TFunctionActionItems : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TFunctionActionItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	TFunctionActionItem* __fastcall GetActionItem(int Index);
	void __fastcall SetActionItem(int Index, TFunctionActionItem* Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall SetItemName(System::Classes::TCollectionItem* Item);
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	TFunctionActionItem* __fastcall GetItemByName(System::UnicodeString aTagName);
	
public:
	__fastcall TFunctionActionItems(System::Classes::TPersistent* AOwner, System::Classes::TCollectionItemClass ItemClass);
	__fastcall virtual ~TFunctionActionItems();
	HIDESBASE TFunctionActionItem* __fastcall Add();
	int __fastcall IndexOf(System::UnicodeString Nombre);
	TFunctionActionItem* __fastcall GetFunction(System::UnicodeString Nombre);
	TFunctionActionItem* __fastcall AddFunction(System::UnicodeString Nombre, bool Enabled, TFunctionEvent Action);
	System::Json::TJSONArray* __fastcall ToJSon(bool aDetail = false);
	void __fastcall SaveToFile(System::UnicodeString FileName);
	void __fastcall LoadFromFile(System::UnicodeString FileName);
	__property TFunctionActionItem* Items[int Index] = {read=GetActionItem, write=SetActionItem/*, default*/};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMCPClientItem : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	bool FEnabled;
	Umakerai::Mcpclient::Core::TMCPClientCustom* FMCPClient;
	bool FConnected;
	System::Classes::TStrings* FParams;
	System::Classes::TStrings* FEnvVars;
	System::UnicodeString __fastcall GetName();
	Umakerai::Core::TToolTransportType __fastcall GetTransportType();
	void __fastcall SetName(const System::UnicodeString Value);
	void __fastcall SetTransportType(const Umakerai::Core::TToolTransportType Value);
	void __fastcall SetEnabled(const bool Value);
	void __fastcall SetConnected(const bool Value);
	System::Classes::TStrings* __fastcall GetParams();
	void __fastcall SetParams(System::Classes::TStrings* const Value);
	System::UnicodeString __fastcall GetConfiguration();
	void __fastcall SetConfiguration(const System::UnicodeString Value);
	System::Classes::TStrings* __fastcall GetEnvVars();
	void __fastcall SetEnvVars(System::Classes::TStrings* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetDisplayName();
	
public:
	__fastcall virtual TMCPClientItem(System::Classes::TCollection* Collection);
	__fastcall virtual ~TMCPClientItem();
	void __fastcall UpdateClientProperties();
	__property Umakerai::Mcpclient::Core::TMCPClientCustom* MCPClient = {read=FMCPClient};
	
__published:
	__property bool Connected = {read=FConnected, write=SetConnected, nodefault};
	__property bool Enabled = {read=FEnabled, write=SetEnabled, nodefault};
	__property System::UnicodeString Name = {read=GetName, write=SetName};
	__property Umakerai::Core::TToolTransportType TransportType = {read=GetTransportType, write=SetTransportType, nodefault};
	__property System::Classes::TStrings* Params = {read=GetParams, write=SetParams};
	__property System::Classes::TStrings* EnvVars = {read=GetEnvVars, write=SetEnvVars};
	__property System::UnicodeString Configuration = {read=GetConfiguration, write=SetConfiguration};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMCPClientItems : public System::Classes::TOwnedCollection
{
	typedef System::Classes::TOwnedCollection inherited;
	
public:
	TMCPClientItem* operator[](int Index) { return this->Items[Index]; }
	
private:
	System::Classes::TPersistent* FOwner;
	TMCPClientItem* __fastcall GetClient(int Index);
	void __fastcall SetClient(int Index, TMCPClientItem* const Value);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner();
	virtual void __fastcall Update(System::Classes::TCollectionItem* Item);
	
public:
	__fastcall TMCPClientItems(System::Classes::TPersistent* AOwner);
	__fastcall virtual ~TMCPClientItems();
	HIDESBASE TMCPClientItem* __fastcall Add();
	TMCPClientItem* __fastcall GetClientByName(const System::UnicodeString AName);
	System::Classes::TStringList* __fastcall GetClientsList();
	System::Classes::TStringList* __fastcall GetFunctionList(System::UnicodeString Name);
	__property TMCPClientItem* Items[int Index] = {read=GetClient, write=SetClient/*, default*/};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiFunctions : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TFunctionActionItems* FFunctions;
	TMCPClientItems* FMCPClients;
	Umakerai::Core::TMCPStatusEvent FOnStatusUpdate;
	Umakerai::Core::TMCPLogEvent FOnLog;
	Umakerai::Mcpclient::Core::TMCPStreamMessageEvent FOnMCPStreamMessage;
	void __fastcall SetOnMCPStreamMessage(const Umakerai::Mcpclient::Core::TMCPStreamMessageEvent Value);
	
protected:
	virtual void __fastcall DoLog(const System::UnicodeString Msg);
	virtual void __fastcall DoStatusUpdate(const System::UnicodeString StatusMsg);
	
public:
	__fastcall virtual TAiFunctions(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiFunctions();
	virtual void __fastcall Loaded();
	virtual System::UnicodeString __fastcall GetTools(Umakerai::Core::TToolFormat aToolFormat);
	virtual bool __fastcall DoCallFunction(Umakerai::Core::TAiToolsFunction* ToolCall);
	bool __fastcall SetFunctionEnable(System::UnicodeString FunctionName, bool Enabled);
	bool __fastcall SetMCPClientEnable(System::UnicodeString Name, bool Enabled);
	System::Classes::TStringList* __fastcall ExtractFunctionNames();
	void __fastcall AddMCPClient(Umakerai::Mcpclient::Core::TMCPClientCustom* aMCPClient);
	int __fastcall ImportClaudeMCPConfiguration(System::Json::TJSONObject* AConfig)/* overload */;
	int __fastcall ImportClaudeMCPConfiguration(const System::UnicodeString AJsonFilePath = System::UnicodeString())/* overload */;
	
__published:
	__property TFunctionActionItems* Functions = {read=FFunctions, write=FFunctions};
	__property TMCPClientItems* MCPClients = {read=FMCPClients, write=FMCPClients};
	__property Umakerai::Core::TMCPLogEvent OnLog = {read=FOnLog, write=FOnLog};
	__property Umakerai::Core::TMCPStatusEvent OnStatusUpdate = {read=FOnStatusUpdate, write=FOnStatusUpdate};
	__property Umakerai::Mcpclient::Core::TMCPStreamMessageEvent OnMCPStreamMessage = {read=FOnMCPStreamMessage, write=SetOnMCPStreamMessage};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TNormalizedTool : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FName;
	System::UnicodeString FDescription;
	System::Json::TJSONObject* FInputSchema;
	
public:
	__fastcall TNormalizedTool(const System::UnicodeString AName, const System::UnicodeString ADescription, System::Json::TJSONObject* AInputSchema);
	__fastcall virtual ~TNormalizedTool();
	__property System::UnicodeString Name = {read=FName};
	__property System::UnicodeString Description = {read=FDescription};
	__property System::Json::TJSONObject* InputSchema = {read=FInputSchema};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TJsonToolUtils : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__classmethod Umakerai::Core::TToolFormat __fastcall DetectInputFormat(System::Json::TJSONObject* AJsonTool);
	__classmethod void __fastcall NormalizeFromMCP(System::Json::TJSONObject* AJsonTool, System::Generics::Collections::TList__1<TNormalizedTool*>* AToolList);
	__classmethod void __fastcall NormalizeFromOpenAI(System::Json::TJSONObject* AJsonTool, System::Generics::Collections::TList__1<TNormalizedTool*>* AToolList);
	__classmethod void __fastcall NormalizeFromAnthropic(System::Json::TJSONObject* AJsonTool, System::Generics::Collections::TList__1<TNormalizedTool*>* AToolList);
	__classmethod void __fastcall NormalizeFromGemini(System::Json::TJSONObject* AJsonTool, System::Generics::Collections::TList__1<TNormalizedTool*>* AToolList);
	__classmethod System::Json::TJSONObject* __fastcall FormatAsMCP(TNormalizedTool* ANormalizedTool);
	__classmethod System::Json::TJSONObject* __fastcall FormatAsOpenAI(TNormalizedTool* ANormalizedTool);
	__classmethod System::Json::TJSONObject* __fastcall FormatAsOpenAIResponses(TNormalizedTool* ANormalizedTool);
	__classmethod System::Json::TJSONObject* __fastcall FormatAsAnthropic(TNormalizedTool* ANormalizedTool);
	__classmethod System::Json::TJSONObject* __fastcall FormatAsGeminiFunctionDeclaration(TNormalizedTool* ANormalizedTool);
	__classmethod System::Json::TJSONObject* __fastcall MergeToolLists(const System::UnicodeString ASourceName, System::Json::TJSONObject* ASourceJson, Umakerai::Core::TToolFormat AInputFormat, System::Json::TJSONObject* ATargetJson, Umakerai::Core::TToolFormat AOutputFormat)/* overload */;
	__classmethod void __fastcall CleanInputSchema(System::Json::TJSONObject* ASchema);
	__classmethod void __fastcall CleanJsonTree(System::Json::TJSONValue* AValue);
	__classmethod void __fastcall EnforceStrictSchema(System::Json::TJSONValue* ASchema);
	
public:
	__classmethod System::Json::TJSONObject* __fastcall MergeToolLists(const System::UnicodeString ASourceName, System::Json::TJSONObject* ASourceJson, System::Json::TJSONObject* ATargetJson, Umakerai::Core::TToolFormat AOutputFormat)/* overload */;
	__classmethod void __fastcall NormalizeToolsFromSource(const System::UnicodeString ASourceName, System::Json::TJSONObject* ASourceJson, System::Generics::Collections::TList__1<TNormalizedTool*>* ANormalizedList);
	__classmethod System::Json::TJSONObject* __fastcall FormatToolList(System::Generics::Collections::TList__1<TNormalizedTool*>* ANormalizedList, Umakerai::Core::TToolFormat AOutputFormat);
public:
	/* TObject.Create */ inline __fastcall TJsonToolUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TJsonToolUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Functions */
}	/* namespace Tools */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_TOOLS_FUNCTIONS)
using namespace Umakerai::Tools::Functions;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_TOOLS)
using namespace Umakerai::Tools;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_Tools_FunctionsHPP
