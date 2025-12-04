// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Core.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_CoreHPP
#define uMakerAi_CoreHPP

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
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.Threading.hpp>
#include <System.Variants.hpp>
#include <System.Net.Mime.hpp>
#include <System.IOUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <System.NetEncoding.hpp>
#include <System.JSON.hpp>
#include <System.StrUtils.hpp>
#include <System.Net.URLClient.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <REST.Json.hpp>
#include <REST.Types.hpp>
#include <REST.Client.hpp>
#include <System.Rtti.hpp>
#include <uMakerAi.Tools.TextEditor.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Core
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiMediaFile;
class DELPHICLASS TAiMediaFiles;
class DELPHICLASS TAiMetadata;
class DELPHICLASS TAiToolsFunction;
class DELPHICLASS TAiToolsFunctions;
class DELPHICLASS TAiWebSearchItem;
class DELPHICLASS TAiWebSearchArray;
class DELPHICLASS TAiWebSearch;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAiFileCategory : unsigned char { Tfc_Text, Tfc_Image, Tfc_Audio, Tfc_Video, Tfc_pdf, Tfc_Document, Tfc_WebSearch, Tfc_CalcSheet, Tfc_Presentation, Tfc_CompressFile, Tfc_Web, Tfc_GraphicDesign, tfc_ExtracttextFile, Tfc_Any, Tfc_Unknow };

typedef System::Set<TAiFileCategory, TAiFileCategory::Tfc_Text, TAiFileCategory::Tfc_Unknow> TAiFileCategories;

enum DECLSPEC_DENUM TAiChatMediaSupport : unsigned char { Tcm_Text, Tcm_Image, Tcm_Audio, Tcm_Video, tcm_pdf, Tcm_Document, tcm_WebSearch, Tcm_CalcSheet, Tcm_Presentation, Tcm_CompressFile, Tcm_Web, Tcm_GraphicDesign, tcm_code_interpreter, tcm_Memory, tcm_TextEditor, tcm_ComputerUse, tcm_Shell, tcm_Any, Tcm_Unknow };

typedef System::Set<TAiChatMediaSupport, TAiChatMediaSupport::Tcm_Text, TAiChatMediaSupport::Tcm_Unknow> TAiChatMediaSupports;

typedef void __fastcall (__closure *TAiErrorEvent)(System::TObject* Sender, const System::UnicodeString ErrorMsg, System::Sysutils::Exception* Exception, const System::Net::Httpclient::_di_IHTTPResponse AResponse);

enum DECLSPEC_DENUM TAiThinkingLevel : unsigned char { tlDefault, tlLow, tlMedium, tlHigh };

enum DECLSPEC_DENUM TAiMediaResolution : unsigned char { mrDefault, mrLow, mrMedium, mrHigh };

enum DECLSPEC_DENUM TAiTranscriptionResponseFormat : unsigned char { trfText, trfJson, trfSrt, trfVtt, trfVerboseJson };

enum DECLSPEC_DENUM TAiTimestampGranularity : unsigned char { tsgNone, tsgWord, tsgSegment };

typedef System::Set<TAiTimestampGranularity, TAiTimestampGranularity::tsgNone, TAiTimestampGranularity::tsgSegment> TAiTimestampGranularities;

typedef void __fastcall (__closure *TMCPLogEvent)(System::TObject* Sender, const System::UnicodeString Msg);

typedef void __fastcall (__closure *TMCPStatusEvent)(System::TObject* Sender, const System::UnicodeString StatusMsg);

enum DECLSPEC_DENUM TToolFormat : unsigned char { tfUnknown, tfOpenAI, tfOpenAIResponses, tfClaude, tfGemini, tfMCP };

enum DECLSPEC_DENUM TToolTransportType : unsigned char { tpStdIo, tpHttp, tpSSE, tpMakerAi };

enum DECLSPEC_DENUM TAiChatState : unsigned char { acsIdle, acsConnecting, acsCreated, acsReasoning, acsWriting, acsToolCalling, acsToolExecuting, acsFinished, acsAborted, acsError };

typedef void __fastcall (__closure *TAiStateChangeEvent)(System::TObject* Sender, TAiChatState State, const System::UnicodeString Description);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiMediaFile : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString Ffilename;
	System::UnicodeString FUrlMedia;
	System::UnicodeString FFileType;
	System::Classes::TMemoryStream* FContent;
	System::UnicodeString FFullFileName;
	System::UnicodeString FTranscription;
	bool FProcesado;
	System::UnicodeString FDetail;
	System::UnicodeString FIdAudio;
	System::UnicodeString FCloudState;
	System::UnicodeString FCloudName;
	System::UnicodeString FCacheName;
	System::UnicodeString FIdFile;
	TAiMediaFiles* FMediaFiles;
	bool FCacheControl;
	bool FEnableCitations;
	System::UnicodeString FContext;
	System::UnicodeString FTitle;
	System::UnicodeString __fastcall GetBase64();
	void __fastcall SetBase64(const System::UnicodeString Value);
	void __fastcall Setfilename(const System::UnicodeString Value);
	void __fastcall SetUrlMedia(const System::UnicodeString Value);
	int __fastcall GetBytes();
	void __fastcall SetFullFileName(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetMimeType();
	TAiFileCategory __fastcall GetFileCategory();
	void __fastcall SetTranscription(const System::UnicodeString Value);
	void __fastcall SetProcesado(const bool Value);
	void __fastcall SetDetail(const System::UnicodeString Value);
	void __fastcall SetIdAudio(const System::UnicodeString Value);
	void __fastcall SetCacheName(const System::UnicodeString Value);
	void __fastcall SetIdFile(const System::UnicodeString Value);
	void __fastcall SetMediaFiles(TAiMediaFiles* const Value);
	
protected:
	virtual void __fastcall DownloadFileFromUrl(System::UnicodeString Url);
	virtual System::Classes::TMemoryStream* __fastcall GetContent();
	
public:
	__fastcall TAiMediaFile();
	__fastcall virtual ~TAiMediaFile();
	virtual void __fastcall LoadFromfile(System::UnicodeString aFileName);
	virtual void __fastcall LoadFromUrl(System::UnicodeString aUrl);
	virtual void __fastcall LoadFromBase64(System::UnicodeString aFileName, System::UnicodeString aBase64);
	virtual void __fastcall LoadFromStream(System::UnicodeString aFileName, System::Classes::TMemoryStream* Stream);
	virtual void __fastcall SaveToFile(System::UnicodeString aFileName);
	virtual System::UnicodeString __fastcall ToString();
	virtual void __fastcall Clear();
	System::Json::TJSONObject* __fastcall ToJsonObject();
	void __fastcall LoadFromJsonObject(System::Json::TJSONObject* AObject);
	void __fastcall Assign(TAiMediaFile* Source);
	__property System::UnicodeString filename = {read=Ffilename, write=Setfilename};
	__property int bytes = {read=GetBytes, nodefault};
	__property System::Classes::TMemoryStream* Content = {read=GetContent};
	__property TAiFileCategory FileCategory = {read=GetFileCategory, nodefault};
	__property System::UnicodeString UrlMedia = {read=FUrlMedia, write=SetUrlMedia};
	__property System::UnicodeString CloudState = {read=FCloudState, write=FCloudState};
	__property System::UnicodeString CloudName = {read=FCloudName, write=FCloudName};
	__property System::UnicodeString CacheName = {read=FCacheName, write=SetCacheName};
	__property System::UnicodeString IdFile = {read=FIdFile, write=SetIdFile};
	__property System::UnicodeString IdAudio = {read=FIdAudio, write=SetIdAudio};
	__property System::UnicodeString Base64 = {read=GetBase64, write=SetBase64};
	__property System::UnicodeString FullFileName = {read=FFullFileName, write=SetFullFileName};
	__property System::UnicodeString MimeType = {read=GetMimeType};
	__property System::UnicodeString Detail = {read=FDetail, write=SetDetail};
	__property System::UnicodeString Transcription = {read=FTranscription, write=SetTranscription};
	__property bool Procesado = {read=FProcesado, write=SetProcesado, nodefault};
	__property TAiMediaFiles* MediaFiles = {read=FMediaFiles, write=SetMediaFiles};
	__property bool CacheControl = {read=FCacheControl, write=FCacheControl, nodefault};
	__property System::UnicodeString Title = {read=FTitle, write=FTitle};
	__property System::UnicodeString Context = {read=FContext, write=FContext};
	__property bool EnableCitations = {read=FEnableCitations, write=FEnableCitations, nodefault};
};

#pragma pack(pop)

typedef System::DynamicArray<TAiMediaFile*> TAiMediaFilesArray;

class PASCALIMPLEMENTATION TAiMediaFiles : public System::Generics::Collections::TObjectList__1<TAiMediaFile*>
{
	typedef System::Generics::Collections::TObjectList__1<TAiMediaFile*> inherited;
	
public:
	TAiMediaFilesArray __fastcall GetMediaList(TAiFileCategories aFilters, bool aProcesado = false);
	TAiMediaFilesArray __fastcall ToMediaFileArray();
public:
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles()/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>() { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TAiMediaFile*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles(System::Generics::Collections::TEnumerable__1<TAiMediaFile*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiMediaFile>.Destroy */ inline __fastcall virtual ~TAiMediaFiles() { }
	
public:
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TAiMediaFile*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>(AComparer) { }
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles(System::Generics::Collections::TEnumerable__1<TAiMediaFile*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles(const System::DelphiInterface<System::IEnumerable__1<TAiMediaFile*> > Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiMediaFile>.Create */ inline __fastcall TAiMediaFiles(TAiMediaFile* const *Values, const System::NativeInt Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMediaFile*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TAiMetadata : public System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>
{
	typedef System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString> inherited;
	
private:
	System::UnicodeString __fastcall GetAsText();
	void __fastcall SetAsText(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetJSonText();
	void __fastcall SetJsonText(const System::UnicodeString Value);
	
public:
	System::Json::TJSONObject* __fastcall ToJSon();
	__property System::UnicodeString AsText = {read=GetAsText, write=SetAsText};
	__property System::UnicodeString JsonText = {read=GetJSonText, write=SetJsonText};
public:
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata()/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>() { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata(System::NativeInt ACapacity)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>(ACapacity) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata(const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>(AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata(System::NativeInt ACapacity, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>(ACapacity, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<System::UnicodeString,System::UnicodeString> >* const Collection)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>(Collection) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<System::UnicodeString,System::UnicodeString> >* const Collection, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>(Collection, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata(const System::Generics::Collections::TPair__2<System::UnicodeString,System::UnicodeString> *AItems, const System::NativeInt AItems_High)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>(AItems, AItems_High) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Create */ inline __fastcall TAiMetadata(const System::Generics::Collections::TPair__2<System::UnicodeString,System::UnicodeString> *AItems, const System::NativeInt AItems_High, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,System::UnicodeString>(AItems, AItems_High, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,System_string>.Destroy */ inline __fastcall virtual ~TAiMetadata() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiToolsFunction : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString id;
	System::UnicodeString Tipo;
	System::UnicodeString name;
	System::UnicodeString Description;
	System::UnicodeString Arguments;
	System::Classes::TStringList* Params;
	System::UnicodeString Function;
	System::UnicodeString Response;
	System::Json::TJSONObject* Body;
	TAiMetadata* Metadata;
	System::TObject* AskMsg;
	System::TObject* ResMsg;
	__fastcall TAiToolsFunction();
	__fastcall virtual ~TAiToolsFunction();
	void __fastcall ParseFunction(System::Json::TJSONObject* JObj);
	void __fastcall Assign(TAiToolsFunction* aSource);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiToolsFunctions : public System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>
{
	typedef System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*> inherited;
	
protected:
	virtual void __fastcall ValueNotify(TAiToolsFunction* const Value, System::Generics::Collections::TCollectionNotification Action);
	
public:
	System::Json::TJSONArray* __fastcall ToOutputJSon();
	System::Json::TJSONArray* __fastcall ToFunctionsJSon();
	void __fastcall AddFunction(System::UnicodeString aBody)/* overload */;
	void __fastcall AddFunction(System::Json::TJSONObject* aBody)/* overload */;
public:
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions()/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>() { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions(System::NativeInt ACapacity)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>(ACapacity) { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions(const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>(AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions(System::NativeInt ACapacity, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>(ACapacity, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<System::UnicodeString,TAiToolsFunction*> >* const Collection)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>(Collection) { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions(System::Generics::Collections::TEnumerable__1<System::Generics::Collections::TPair__2<System::UnicodeString,TAiToolsFunction*> >* const Collection, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>(Collection, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions(const System::Generics::Collections::TPair__2<System::UnicodeString,TAiToolsFunction*> *AItems, const System::NativeInt AItems_High)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>(AItems, AItems_High) { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Create */ inline __fastcall TAiToolsFunctions(const System::Generics::Collections::TPair__2<System::UnicodeString,TAiToolsFunction*> *AItems, const System::NativeInt AItems_High, const System::DelphiInterface<System::Generics::Defaults::IEqualityComparer__1<System::UnicodeString> > AComparer)/* overload */ : System::Generics::Collections::TDictionary__2<System::UnicodeString,TAiToolsFunction*>(AItems, AItems_High, AComparer) { }
	/* {System_Generics_Collections}TDictionary<System_string,uMakerAi_Core_TAiToolsFunction>.Destroy */ inline __fastcall virtual ~TAiToolsFunctions() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiWebSearchItem : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString type;
	int start_index;
	int end_index;
	System::UnicodeString Url;
	System::UnicodeString Title;
public:
	/* TObject.Create */ inline __fastcall TAiWebSearchItem() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAiWebSearchItem() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiWebSearchArray : public System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>
{
	typedef System::Generics::Collections::TObjectList__1<TAiWebSearchItem*> inherited;
	
public:
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray()/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>() { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TAiWebSearchItem*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray(System::Generics::Collections::TEnumerable__1<TAiWebSearchItem*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Core_TAiWebSearchItem>.Destroy */ inline __fastcall virtual ~TAiWebSearchArray() { }
	
public:
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TAiWebSearchItem*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>(AComparer) { }
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray(System::Generics::Collections::TEnumerable__1<TAiWebSearchItem*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray(const System::DelphiInterface<System::IEnumerable__1<TAiWebSearchItem*> > Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Core_TAiWebSearchItem>.Create */ inline __fastcall TAiWebSearchArray(TAiWebSearchItem* const *Values, const System::NativeInt Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TAiWebSearchItem*>(Values, Values_High) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiWebSearch : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString type;
	System::UnicodeString text;
	TAiWebSearchArray* annotations;
	__fastcall TAiWebSearch();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TAiWebSearch() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetParametrosURL(System::Classes::TStringList* Parametros);
extern DELPHI_PACKAGE System::UnicodeString __fastcall StreamToBase64(System::Classes::TMemoryStream* Stream);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetMimeTypeFromFileName(System::UnicodeString FileExtension);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetFileExtensionFromMimeType(System::UnicodeString MimeType);
extern DELPHI_PACKAGE TAiFileCategory __fastcall GetContentCategory(System::UnicodeString FileExtension);
}	/* namespace Core */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CORE)
using namespace Umakerai::Core;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_CoreHPP
