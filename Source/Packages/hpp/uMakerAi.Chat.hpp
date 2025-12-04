// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_ChatHPP
#define uMakerAi_ChatHPP

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
#include <System.StrUtils.hpp>
#include <System.Threading.hpp>
#include <System.TypInfo.hpp>
#include <System.Types.hpp>
#include <System.Net.Mime.hpp>
#include <System.NetConsts.hpp>
#include <System.NetEncoding.hpp>
#include <System.Net.URLClient.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <System.JSON.hpp>
#include <REST.Json.hpp>
#include <uMakerAi.Tools.Functions.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Utils.CodeExtractor.hpp>
#include <uMakerAi.Tools.Shell.hpp>
#include <uMakerAi.Tools.TextEditor.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiChatMessage;
class DELPHICLASS TAiChatMessages;
class DELPHICLASS TAiSourceData;
class DELPHICLASS TAiCitationSource;
class DELPHICLASS TAiMsgCitation;
class DELPHICLASS TAiMsgCitations;
class DELPHICLASS TAiChat;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiChatMessage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FPreviousResponseId;
	Umakerai::Core::TAiWebSearch* FWebSearchResponse;
	System::UnicodeString FReasoningContent;
	bool FIsTollCallResponse;
	System::UnicodeString FModel;
	TAiMsgCitations* FCitations;
	System::UnicodeString FStopReason;
	bool FIsRefusal;
	System::UnicodeString FThinkingSignature;
	bool FCacheControl;
	int FThinking_tokens;
	System::UnicodeString FFinishReason;
	int FCached_tokens;
	void __fastcall SetContent(const System::UnicodeString Value);
	void __fastcall SetRole(const System::UnicodeString Value);
	void __fastcall SetPrompt(const System::UnicodeString Value);
	void __fastcall SetFunctionName(const System::UnicodeString Value);
	void __fastcall SetTollCallId(const System::UnicodeString Value);
	void __fastcall SetTool_calls(const System::UnicodeString Value);
	void __fastcall SetFId(const int Value);
	void __fastcall SetCompletion_tokens(const int Value);
	void __fastcall SetPrompt_tokens(const int Value);
	void __fastcall SetTotal_tokens(const int Value);
	void __fastcall SetFPreviousResponseId(const System::UnicodeString Value);
	void __fastcall SetWebSearchResponse(Umakerai::Core::TAiWebSearch* const Value);
	void __fastcall SetReasoningContent(const System::UnicodeString Value);
	void __fastcall SetIsTollCallResponse(const bool Value);
	void __fastcall SetModel(const System::UnicodeString Value);
	void __fastcall SetCitations(TAiMsgCitations* const Value);
	void __fastcall SetIsRefusal(const bool Value);
	void __fastcall SetStopReason(const System::UnicodeString Value);
	void __fastcall SetThinking_tokens(const int Value);
	void __fastcall SetFinishReason(const System::UnicodeString Value);
	void __fastcall SetCached_tokens(const int Value);
	
protected:
	System::UnicodeString FRole;
	System::UnicodeString FContent;
	System::UnicodeString FPrompt;
	int FCompletion_tokens;
	int FTotal_tokens;
	int FPrompt_tokens;
	int FId;
	System::UnicodeString FTollCallId;
	System::UnicodeString FFunctionName;
	System::UnicodeString FTool_calls;
	Umakerai::Core::TAiMediaFiles* FMediaFiles;
	
public:
	__fastcall TAiChatMessage(System::UnicodeString aPrompt, System::UnicodeString aRole, System::UnicodeString aToolCallId, System::UnicodeString aFunctionName);
	__fastcall virtual ~TAiChatMessage();
	void __fastcall AddMediaFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	void __fastcall LoadMediaFromFile(System::UnicodeString aFileName);
	void __fastcall LoadMediaFromStream(System::UnicodeString aFileName, System::Classes::TMemoryStream* Stream);
	void __fastcall LoadMediaFromBase64(System::UnicodeString aFileName, System::UnicodeString aBase64);
	bool __fastcall HasUnprocessedItems();
	System::UnicodeString __fastcall GetMediaTranscription();
	System::UnicodeString __fastcall StreamToBase64(System::Classes::TMemoryStream* Stream);
	System::Json::TJSONArray* __fastcall ToJSon();
	__property int Id = {read=FId, write=SetFId, nodefault};
	__property System::UnicodeString Role = {read=FRole, write=SetRole};
	__property System::UnicodeString Content = {read=FContent, write=SetContent};
	__property System::UnicodeString Prompt = {read=FPrompt, write=SetPrompt};
	__property int Prompt_tokens = {read=FPrompt_tokens, write=SetPrompt_tokens, nodefault};
	__property int Completion_tokens = {read=FCompletion_tokens, write=SetCompletion_tokens, nodefault};
	__property int Total_tokens = {read=FTotal_tokens, write=SetTotal_tokens, nodefault};
	__property int Thinking_tokens = {read=FThinking_tokens, write=SetThinking_tokens, nodefault};
	__property int Cached_tokens = {read=FCached_tokens, write=SetCached_tokens, nodefault};
	__property System::UnicodeString Model = {read=FModel, write=SetModel};
	__property System::UnicodeString TollCallId = {read=FTollCallId, write=SetTollCallId};
	__property System::UnicodeString FunctionName = {read=FFunctionName, write=SetFunctionName};
	__property System::UnicodeString Tool_calls = {read=FTool_calls, write=SetTool_calls};
	__property Umakerai::Core::TAiMediaFiles* MediaFiles = {read=FMediaFiles};
	__property Umakerai::Core::TAiWebSearch* WebSearchResponse = {read=FWebSearchResponse, write=SetWebSearchResponse};
	__property System::UnicodeString PreviousResponseId = {read=FPreviousResponseId, write=SetFPreviousResponseId};
	__property System::UnicodeString ReasoningContent = {read=FReasoningContent, write=SetReasoningContent};
	__property bool IsTollCallResponse = {read=FIsTollCallResponse, write=SetIsTollCallResponse, nodefault};
	__property TAiMsgCitations* Citations = {read=FCitations, write=SetCitations};
	__property System::UnicodeString StopReason = {read=FStopReason, write=SetStopReason};
	__property bool IsRefusal = {read=FIsRefusal, write=SetIsRefusal, nodefault};
	__property System::UnicodeString ThinkingSignature = {read=FThinkingSignature, write=FThinkingSignature};
	__property bool CacheControl = {read=FCacheControl, write=FCacheControl, nodefault};
	__property System::UnicodeString FinishReason = {read=FFinishReason, write=SetFinishReason};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiChatMessages : public System::Generics::Collections::TList__1<TAiChatMessage*>
{
	typedef System::Generics::Collections::TList__1<TAiChatMessage*> inherited;
	
private:
	Umakerai::Core::TAiFileCategories FNativeInputFiles;
	System::UnicodeString __fastcall GetAsText();
	void __fastcall SetAsText(const System::UnicodeString Value);
	void __fastcall SetNativeInputFiles(const Umakerai::Core::TAiFileCategories Value);
	
public:
	System::Json::TJSONArray* __fastcall ToJSon();
	System::Json::TJSONObject* __fastcall ExportChatHistory();
	void __fastcall SaveToStream(System::Classes::TStream* Stream);
	void __fastcall SaveToFile(System::UnicodeString FileName);
	void __fastcall LoadFromStream(System::Classes::TStream* Stream);
	void __fastcall LoadFromFile(System::UnicodeString FileName);
	__property System::UnicodeString AsText = {read=GetAsText, write=SetAsText};
	__property Umakerai::Core::TAiFileCategories NativeInputFiles = {read=FNativeInputFiles, write=SetNativeInputFiles, nodefault};
public:
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiChatMessage>.Create */ inline __fastcall TAiChatMessages()/* overload */ : System::Generics::Collections::TList__1<TAiChatMessage*>() { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiChatMessage>.Create */ inline __fastcall TAiChatMessages(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TAiChatMessage*> > AComparer)/* overload */ : System::Generics::Collections::TList__1<TAiChatMessage*>(AComparer) { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiChatMessage>.Create */ inline __fastcall TAiChatMessages(System::Generics::Collections::TEnumerable__1<TAiChatMessage*>* const Collection)/* overload */ : System::Generics::Collections::TList__1<TAiChatMessage*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiChatMessage>.Create */ inline __fastcall TAiChatMessages(const System::DelphiInterface<System::IEnumerable__1<TAiChatMessage*> > Collection)/* overload */ : System::Generics::Collections::TList__1<TAiChatMessage*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiChatMessage>.Create */ inline __fastcall TAiChatMessages(TAiChatMessage* const *Values, const System::NativeInt Values_High)/* overload */ : System::Generics::Collections::TList__1<TAiChatMessage*>(Values, Values_High) { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiChatMessage>.Destroy */ inline __fastcall virtual ~TAiChatMessages() { }
	
};


enum DECLSPEC_DENUM TAiChatResponseFormat : unsigned char { tiaChatRfText, tiaChatRfJson, tiaChatRfJsonSchema };

typedef void __fastcall (__closure *TAiChatOnDataEvent)(System::TObject* const Sender, TAiChatMessage* aMsg, System::Json::TJSONObject* aResponse, System::UnicodeString aRole, System::UnicodeString aText);

typedef void __fastcall (__closure *TAiChatOnBeforeSendEvent)(System::TObject* const Sender, TAiChatMessage* &aMsg);

typedef void __fastcall (__closure *TAiChatOnInitChatEvent)(System::TObject* const Sender, System::UnicodeString aRole, System::UnicodeString &aText, System::Json::TJSONObject* &aMemory);

typedef void __fastcall (__closure *TAiChatOnMediaFileEvent)(System::TObject* const Sender, System::UnicodeString Prompt, Umakerai::Core::TAiMediaFile* MediaFile, Umakerai::Core::TAiFileCategories aNativeInputFiles, System::UnicodeString &Respuesta, bool &aProcesado);

typedef void __fastcall (__closure *TAiChatOnProcessResponseEvent)(System::TObject* const Sender, TAiChatMessage* LastMsg, TAiChatMessage* ResMsg, System::UnicodeString &Response);

typedef void __fastcall (__closure *TAiModelProgressEvent)(System::TObject* Sender, System::UnicodeString Status, __int64 Completed, __int64 Total);

typedef void __fastcall (__closure *TOnCallToolFunction)(System::TObject* Sender, Umakerai::Core::TAiToolsFunction* AiToolCall);

enum DECLSPEC_DENUM TAiCitationSourceType : unsigned char { cstUnknown, cstDocument, cstWeb, cstFile, cstDatabase };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiSourceData : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString Id;
	System::UnicodeString Title;
	System::UnicodeString Content;
	System::UnicodeString Url;
	Umakerai::Core::TAiMetadata* Metadata;
	__fastcall TAiSourceData();
	__fastcall virtual ~TAiSourceData();
	void __fastcall Assign(TAiSourceData* Source);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiCitationSource : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TAiCitationSourceType SourceType;
	TAiSourceData* DataSource;
	__fastcall TAiCitationSource();
	__fastcall virtual ~TAiCitationSource();
	void __fastcall Assign(TAiCitationSource* Source);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiMsgCitation : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	int StartIndex;
	int EndIndex;
	System::UnicodeString Text;
	System::Generics::Collections::TObjectList__1<TAiCitationSource*>* Sources;
	__fastcall TAiMsgCitation();
	__fastcall virtual ~TAiMsgCitation();
	void __fastcall Assign(TAiMsgCitation* Source);
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiMsgCitations : public System::Generics::Collections::TObjectList__1<TAiMsgCitation*>
{
	typedef System::Generics::Collections::TObjectList__1<TAiMsgCitation*> inherited;
	
public:
	void __fastcall Assign(TAiMsgCitations* Source);
public:
	/* {System_Generics_Collections}TObjectList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations()/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>() { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations(bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>(AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TAiMsgCitation*> > AComparer, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>(AComparer, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations(System::Generics::Collections::TEnumerable__1<TAiMsgCitation*>* const Collection, bool AOwnsObjects)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>(Collection, AOwnsObjects) { }
	/* {System_Generics_Collections}TObjectList<uMakerAi_Chat_TAiMsgCitation>.Destroy */ inline __fastcall virtual ~TAiMsgCitations() { }
	
public:
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations(const System::DelphiInterface<System::Generics::Defaults::IComparer__1<TAiMsgCitation*> > AComparer)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>(AComparer) { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations(System::Generics::Collections::TEnumerable__1<TAiMsgCitation*>* const Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations(const System::DelphiInterface<System::IEnumerable__1<TAiMsgCitation*> > Collection)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>(Collection) { }
	/* {System_Generics_Collections}TList<uMakerAi_Chat_TAiMsgCitation>.Create */ inline __fastcall TAiMsgCitations(TAiMsgCitation* const *Values, const System::NativeInt Values_High)/* overload */ : System::Generics::Collections::TObjectList__1<TAiMsgCitation*>(Values, Values_High) { }
	
};


class PASCALIMPLEMENTATION TAiChat : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::TObject* FOwner;
	System::UnicodeString FApiKey;
	int FSeed;
	System::UnicodeString FTool_choice;
	int FN;
	double FTop_p;
	bool FLogprobs;
	double FFrequency_penalty;
	System::UnicodeString FStop;
	System::UnicodeString FLogit_bias;
	double FTemperature;
	double FPresence_penalty;
	System::UnicodeString FUser;
	int FMax_tokens;
	bool FAsynchronous;
	System::UnicodeString FTop_logprobs;
	System::UnicodeString FModel;
	System::Classes::TStrings* FInitialInstructions;
	int FCompletion_tokens;
	int FTotal_tokens;
	int FPrompt_tokens;
	bool FTool_Active;
	System::UnicodeString FUrl;
	int FResponseTimeOut;
	TAiChatOnInitChatEvent FOnInitChat;
	System::Classes::TStrings* FMemory;
	Umakerai::Tools::Functions::TAiFunctions* FAiFunctions;
	TAiChatOnMediaFileEvent FOnProcessMediaFile;
	System::Classes::TStrings* FJsonSchema;
	bool FStream_Usage;
	Umakerai::Core::TAiFileCategories FNativeInputFiles;
	Umakerai::Core::TAiFileCategories FNativeOutputFiles;
	Umakerai::Core::TAiErrorEvent FOnError;
	TAiChatOnProcessResponseEvent FOnProcessResponse;
	System::UnicodeString FVoice;
	System::UnicodeString Fvoice_format;
	System::UnicodeString FLanguage;
	System::UnicodeString FTranscription_ResponseFormat;
	System::UnicodeString FTranscription_TimestampGranularities;
	Umakerai::Core::TAiChatMediaSupports FChatMediaSupports;
	System::UnicodeString FReasoningFormat;
	int FK;
	TAiModelProgressEvent FOnProgressEvent;
	TAiChatOnDataEvent FOnReceiveThinking;
	Umakerai::Core::TAiThinkingLevel FThinkingLevel;
	Umakerai::Core::TAiMediaResolution FMediaResolution;
	System::Classes::TStrings* FImageParams;
	System::Classes::TStrings* FVideoParams;
	System::Classes::TStrings* FWebSearchParams;
	Umakerai::Core::TAiStateChangeEvent FOnStateChange;
	void __fastcall SetApiKey(const System::UnicodeString Value);
	void __fastcall SetFrequency_penalty(const double Value);
	void __fastcall SetLogit_bias(const System::UnicodeString Value);
	void __fastcall SetLogprobs(const bool Value);
	void __fastcall SetMax_tokens(const int Value);
	void __fastcall SetN(const int Value);
	void __fastcall SetPresence_penalty(const double Value);
	void __fastcall SetResponse_format(const TAiChatResponseFormat Value);
	void __fastcall SetSeed(const int Value);
	void __fastcall SetStop(const System::UnicodeString Value);
	void __fastcall SetTemperature(const double Value);
	void __fastcall SetTool_choice(const System::UnicodeString Value);
	void __fastcall SetTop_p(const double Value);
	void __fastcall SetUser(const System::UnicodeString Value);
	void __fastcall SetAsynchronous(const bool Value);
	void __fastcall SetTop_logprobs(const System::UnicodeString Value);
	void __fastcall SetOnReceiveDataEvent(const TAiChatOnDataEvent Value);
	void __fastcall SetOnReceiveDataEnd(const TAiChatOnDataEvent Value);
	void __fastcall SetModel(const System::UnicodeString Value);
	void __fastcall SetInitialInstructions(System::Classes::TStrings* const Value);
	void __fastcall SetOnAddMessage(const TAiChatOnDataEvent Value);
	void __fastcall SetOnCallToolFunction(const TOnCallToolFunction Value);
	void __fastcall SetTool_Active(const bool Value);
	void __fastcall SetOnBeforeSendMessage(const TAiChatOnBeforeSendEvent Value);
	void __fastcall SetCompletion_tokens(const int Value);
	void __fastcall SetPrompt_tokens(const int Value);
	void __fastcall SetTotal_tokens(const int Value);
	void __fastcall SetUrl(const System::UnicodeString Value);
	void __fastcall SetLastError(const System::UnicodeString Value);
	void __fastcall SetResponseTimeOut(const int Value);
	void __fastcall SetOnInitChat(const TAiChatOnInitChatEvent Value);
	void __fastcall SetMemory(System::Classes::TStrings* const Value);
	void __fastcall SetJsonSchema(System::Classes::TStrings* const Value);
	void __fastcall SetAiFunctions(Umakerai::Tools::Functions::TAiFunctions* const Value);
	void __fastcall SetOnProcessMediaFile(const TAiChatOnMediaFileEvent Value);
	void __fastcall SetStream_Usage(const bool Value);
	void __fastcall SetNativeInputFiles(const Umakerai::Core::TAiFileCategories Value);
	void __fastcall SetNativeOutputFiles(const Umakerai::Core::TAiFileCategories Value);
	void __fastcall SetOnError(const Umakerai::Core::TAiErrorEvent Value);
	void __fastcall SetOnProcessResponse(const TAiChatOnProcessResponseEvent Value);
	void __fastcall SetVoice(const System::UnicodeString Value);
	void __fastcall Setvoice_format(const System::UnicodeString Value);
	void __fastcall SetLanguage(const System::UnicodeString Value);
	void __fastcall SetTranscription_ResponseFormat(const System::UnicodeString Value);
	void __fastcall SetTranscription_TimestampGranularities(const System::UnicodeString Value);
	void __fastcall SetChatMediaSupports(const Umakerai::Core::TAiChatMediaSupports Value);
	System::UnicodeString __fastcall GetApiKey();
	void __fastcall SetReasoningFormat(const System::UnicodeString Value);
	void __fastcall SetK(const int Value);
	void __fastcall SetOnProgressEvent(const TAiModelProgressEvent Value);
	void __fastcall SetOnReceiveThinking(const TAiChatOnDataEvent Value);
	void __fastcall SetThinkingLevel(const Umakerai::Core::TAiThinkingLevel Value);
	void __fastcall SetMediaResolution(const Umakerai::Core::TAiMediaResolution Value);
	void __fastcall SetImageParams(System::Classes::TStrings* const Value);
	void __fastcall SetVideoParams(System::Classes::TStrings* const Value);
	void __fastcall SetWebSearchParams(System::Classes::TStrings* const Value);
	void __fastcall SetShellTool(Umakerai::Tools::Shell::TAiShell* const Value);
	void __fastcall SetThinking_tokens(const int Value);
	void __fastcall SetTextEditorTool(Umakerai::Tools::Texteditor::TAiTextEditorTool* const Value);
	
protected:
	System::Net::Httpclientcomponent::TNetHTTPClient* FClient;
	System::UnicodeString FTmpRole;
	System::UnicodeString FTmpResponseText;
	System::UnicodeString FTmpResponseText1;
	bool FAbort;
	bool FBusy;
	System::Classes::TStrings* FTools;
	System::UnicodeString FLastContent;
	System::UnicodeString FLastPrompt;
	System::UnicodeString FLastError;
	TAiChatMessages* FMessages;
	TAiChatResponseFormat FResponse_format;
	System::Classes::TStringStream* FResponse;
	TAiChatOnDataEvent FOnReceiveDataEvent;
	TAiChatOnDataEvent FOnReceiveDataEnd;
	TAiChatOnDataEvent FOnAddMessage;
	TOnCallToolFunction FOnCallToolFunction;
	TAiChatOnBeforeSendEvent FOnBeforeSendMessage;
	System::Generics::Collections::TDictionary__2<int,System::Json::TJSONObject*>* FTmpToolCallBuffer;
	Umakerai::Tools::Shell::TAiShell* FShellTool;
	int FThinking_tokens;
	Umakerai::Tools::Texteditor::TAiTextEditorTool* FTextEditorTool;
	virtual void __fastcall OnInternalReceiveData(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &AAbort);
	virtual void __fastcall OnRequestErrorEvent(System::TObject* const Sender, const System::UnicodeString AError);
	virtual void __fastcall OnRequestExceptionEvent(System::TObject* const Sender, System::Sysutils::Exception* const AError);
	virtual void __fastcall OnRequestCompletedEvent(System::TObject* const Sender, const System::Net::Httpclient::_di_IHTTPResponse aResponse);
	virtual TAiChatMessage* __fastcall InternalAddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole, System::UnicodeString aToolCallId, System::UnicodeString aFunctionName)/* overload */;
	virtual TAiChatMessage* __fastcall InternalAddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole, Umakerai::Core::TAiMediaFilesArray aMediaFiles)/* overload */;
	virtual TAiChatMessage* __fastcall InternalAddMessage(TAiChatMessage* aMsg)/* overload */;
	virtual System::UnicodeString __fastcall InternalRunSpeechGeneration(TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunImageGeneration(TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunImageVideoGeneration(TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunWebSearch(TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunCompletions(TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunTranscription(Umakerai::Core::TAiMediaFile* aMediaFile, TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunImageDescription(Umakerai::Core::TAiMediaFile* aMediaFile, TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InternalRunPDFDescription(Umakerai::Core::TAiMediaFile* aMediaFile, TAiChatMessage* ResMsg, TAiChatMessage* AskMsg);
	virtual System::UnicodeString __fastcall InitChatCompletions();
	virtual void __fastcall ParseChat(System::Json::TJSONObject* jObj, TAiChatMessage* ResMsg);
	void __fastcall ParseJsonTranscript(System::Json::TJSONObject* jObj, TAiChatMessage* ResMsg, Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual Umakerai::Core::TAiToolsFunctions* __fastcall ExtractToolCallFromJson(System::Json::TJSONArray* jChoices);
	virtual void __fastcall DoCallFunction(Umakerai::Core::TAiToolsFunction* ToolCall);
	virtual System::Classes::TStrings* __fastcall GetTools(Umakerai::Core::TToolFormat aToolFormat);
	virtual System::UnicodeString __fastcall PrepareSystemMsg();
	void __fastcall DoProcessMediaFile(System::UnicodeString aPrompt, Umakerai::Core::TAiMediaFile* aAiMediaFile, System::UnicodeString &Respuesta, bool &Procesado);
	System::UnicodeString __fastcall AddMessageAndRun(System::UnicodeString aPrompt, System::UnicodeString aRole, System::UnicodeString aToolCallId, System::UnicodeString aFunctionName)/* overload */;
	virtual void __fastcall DoError(const System::UnicodeString ErrorMsg, System::Sysutils::Exception* E);
	void __fastcall DoProcessResponse(TAiChatMessage* aLastMsg, TAiChatMessage* aResMsg, System::UnicodeString &aResponse);
	void __fastcall DoStateChange(Umakerai::Core::TAiChatState State, const System::UnicodeString Description = System::UnicodeString());
	
public:
	__fastcall virtual TAiChat(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiChat();
	System::UnicodeString __fastcall AddMessageAndRun(System::UnicodeString aPrompt, System::UnicodeString aRole, Umakerai::Core::TAiMediaFilesArray aMediaFiles)/* overload */;
	TAiChatMessage* __fastcall AddMessageAndRunMsg(System::UnicodeString aPrompt, System::UnicodeString aRole, Umakerai::Core::TAiMediaFilesArray aMediaFiles)/* overload */;
	TAiChatMessage* __fastcall AddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole);
	TAiChatMessage* __fastcall NewMessage(System::UnicodeString aPrompt, System::UnicodeString aRole);
	virtual System::UnicodeString __fastcall Run(TAiChatMessage* AskMsg, TAiChatMessage* ResMsg = (TAiChatMessage*)(0x0));
	TAiChatMessage* __fastcall GetLastMessage();
	bool __fastcall RemoveMesage(TAiChatMessage* Msg)/* overload */;
	bool __fastcall RemoveMesage(int IdMsg)/* overload */;
	void __fastcall AddToMemory(System::UnicodeString Key, System::UnicodeString Value);
	void __fastcall RemoveFromMemory(System::UnicodeString Key);
	virtual void __fastcall NewChat();
	void __fastcall Abort();
	__classmethod virtual System::Classes::TStringList* __fastcall GetModels(System::UnicodeString aApiKey, System::UnicodeString aUrl = System::UnicodeString())/* overload */;
	virtual System::Classes::TStringList* __fastcall GetModels()/* overload */;
	virtual System::Json::TJSONArray* __fastcall GetMessages();
	System::UnicodeString __fastcall PublicChatToSend();
	System::UnicodeString __fastcall FileCategoriesToString(const Umakerai::Core::TAiFileCategories ACategories);
	Umakerai::Core::TAiFileCategories __fastcall StringToFileCategories(const System::UnicodeString AValue);
	virtual System::UnicodeString __fastcall UploadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DownLoadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall CheckFileState(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DeleteFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual Umakerai::Core::TAiMediaFile* __fastcall RetrieveFile(System::UnicodeString aFileId);
	virtual Umakerai::Core::TAiMediaFiles* __fastcall RetrieveFileList();
	virtual System::UnicodeString __fastcall UploadFileToCache(Umakerai::Core::TAiMediaFile* aMediaFile, int aTTL_Seconds = 0xe10);
	virtual __classmethod System::UnicodeString __fastcall GetDriverName() = 0 ;
	virtual __classmethod void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params) = 0 ;
	virtual __classmethod TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender) = 0 ;
	__property TAiChatMessages* Messages = {read=FMessages};
	__property System::UnicodeString LastError = {read=FLastError, write=SetLastError};
	
__published:
	__property System::UnicodeString ApiKey = {read=GetApiKey, write=SetApiKey};
	__property System::UnicodeString Model = {read=FModel, write=SetModel};
	__property double Frequency_penalty = {read=FFrequency_penalty, write=SetFrequency_penalty};
	__property System::UnicodeString Logit_bias = {read=FLogit_bias, write=SetLogit_bias};
	__property bool Logprobs = {read=FLogprobs, write=SetLogprobs, nodefault};
	__property System::UnicodeString Top_logprobs = {read=FTop_logprobs, write=SetTop_logprobs};
	__property int Max_tokens = {read=FMax_tokens, write=SetMax_tokens, nodefault};
	__property int N = {read=FN, write=SetN, nodefault};
	__property double Presence_penalty = {read=FPresence_penalty, write=SetPresence_penalty};
	__property TAiChatResponseFormat Response_format = {read=FResponse_format, write=SetResponse_format, nodefault};
	__property int Seed = {read=FSeed, write=SetSeed, nodefault};
	__property System::UnicodeString Stop = {read=FStop, write=SetStop};
	__property bool Asynchronous = {read=FAsynchronous, write=SetAsynchronous, nodefault};
	__property double Temperature = {read=FTemperature, write=SetTemperature};
	__property double Top_p = {read=FTop_p, write=SetTop_p};
	__property int K = {read=FK, write=SetK, nodefault};
	__property System::UnicodeString Tool_choice = {read=FTool_choice, write=SetTool_choice};
	__property bool Tool_Active = {read=FTool_Active, write=SetTool_Active, nodefault};
	__property System::UnicodeString User = {read=FUser, write=SetUser};
	__property System::Classes::TStrings* InitialInstructions = {read=FInitialInstructions, write=SetInitialInstructions};
	__property int Prompt_tokens = {read=FPrompt_tokens, write=SetPrompt_tokens, nodefault};
	__property int Completion_tokens = {read=FCompletion_tokens, write=SetCompletion_tokens, nodefault};
	__property int Total_tokens = {read=FTotal_tokens, write=SetTotal_tokens, nodefault};
	__property int Thinking_tokens = {read=FThinking_tokens, write=SetThinking_tokens, nodefault};
	__property System::UnicodeString LastContent = {read=FLastContent};
	__property System::UnicodeString LastPrompt = {read=FLastPrompt};
	__property bool Busy = {read=FBusy, nodefault};
	__property TAiChatOnDataEvent OnReceiveThinking = {read=FOnReceiveThinking, write=SetOnReceiveThinking};
	__property TAiChatOnDataEvent OnReceiveData = {read=FOnReceiveDataEvent, write=SetOnReceiveDataEvent};
	__property TAiChatOnDataEvent OnReceiveDataEnd = {read=FOnReceiveDataEnd, write=SetOnReceiveDataEnd};
	__property TAiChatOnDataEvent OnAddMessage = {read=FOnAddMessage, write=SetOnAddMessage};
	__property TOnCallToolFunction OnCallToolFunction = {read=FOnCallToolFunction, write=SetOnCallToolFunction};
	__property TAiChatOnBeforeSendEvent OnBeforeSendMessage = {read=FOnBeforeSendMessage, write=SetOnBeforeSendMessage};
	__property TAiChatOnInitChatEvent OnInitChat = {read=FOnInitChat, write=SetOnInitChat};
	__property TAiChatOnProcessResponseEvent OnProcessResponse = {read=FOnProcessResponse, write=SetOnProcessResponse};
	__property TAiModelProgressEvent OnProgressEvent = {read=FOnProgressEvent, write=SetOnProgressEvent};
	__property System::UnicodeString Url = {read=FUrl, write=SetUrl};
	__property int ResponseTimeOut = {read=FResponseTimeOut, write=SetResponseTimeOut, nodefault};
	__property System::Classes::TStrings* Memory = {read=FMemory, write=SetMemory};
	__property Umakerai::Tools::Functions::TAiFunctions* AiFunctions = {read=FAiFunctions, write=SetAiFunctions};
	__property TAiChatOnMediaFileEvent OnProcessMediaFile = {read=FOnProcessMediaFile, write=SetOnProcessMediaFile};
	__property System::Classes::TStrings* JsonSchema = {read=FJsonSchema, write=SetJsonSchema};
	__property bool Stream_Usage = {read=FStream_Usage, write=SetStream_Usage, nodefault};
	__property Umakerai::Core::TAiFileCategories NativeInputFiles = {read=FNativeInputFiles, write=SetNativeInputFiles, nodefault};
	__property Umakerai::Core::TAiFileCategories NativeOutputFiles = {read=FNativeOutputFiles, write=SetNativeOutputFiles, nodefault};
	__property Umakerai::Core::TAiChatMediaSupports ChatMediaSupports = {read=FChatMediaSupports, write=SetChatMediaSupports, nodefault};
	__property System::UnicodeString Voice = {read=FVoice, write=SetVoice};
	__property System::UnicodeString voice_format = {read=Fvoice_format, write=Setvoice_format};
	__property Umakerai::Core::TAiErrorEvent OnError = {read=FOnError, write=SetOnError};
	__property System::UnicodeString Language = {read=FLanguage, write=SetLanguage};
	__property System::UnicodeString Transcription_ResponseFormat = {read=FTranscription_ResponseFormat, write=SetTranscription_ResponseFormat};
	__property System::UnicodeString Transcription_TimestampGranularities = {read=FTranscription_TimestampGranularities, write=SetTranscription_TimestampGranularities};
	__property System::UnicodeString ReasoningFormat = {read=FReasoningFormat, write=SetReasoningFormat};
	__property Umakerai::Core::TAiThinkingLevel ThinkingLevel = {read=FThinkingLevel, write=SetThinkingLevel, nodefault};
	__property Umakerai::Core::TAiMediaResolution MediaResolution = {read=FMediaResolution, write=SetMediaResolution, nodefault};
	__property System::Classes::TStrings* VideoParams = {read=FVideoParams, write=SetVideoParams};
	__property System::Classes::TStrings* ImageParams = {read=FImageParams, write=SetImageParams};
	__property System::Classes::TStrings* WebSearchParams = {read=FWebSearchParams, write=SetWebSearchParams};
	__property Umakerai::Core::TAiStateChangeEvent OnStateChange = {read=FOnStateChange, write=FOnStateChange};
	__property Umakerai::Tools::Shell::TAiShell* ShellTool = {read=FShellTool, write=SetShellTool};
	__property Umakerai::Tools::Texteditor::TAiTextEditorTool* TextEditorTool = {read=FTextEditorTool, write=SetTextEditorTool};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall LogDebug(const System::UnicodeString Mensaje);
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT)
using namespace Umakerai::Chat;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_ChatHPP
