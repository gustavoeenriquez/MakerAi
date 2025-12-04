// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Chat.Claude.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Chat_ClaudeHPP
#define uMakerAi_Chat_ClaudeHPP

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
#include <UMakerAi.ParamsRegistry.hpp>
#include <uMakerAi.Chat.hpp>
#include <uMakerAi.Tools.Functions.hpp>
#include <uMakerAi.Core.hpp>
#include <uMakerAi.Utils.CodeExtractor.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Chat
{
namespace Claude
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TClaudeContextTrigger;
class DELPHICLASS TClaudeContextEdit;
class DELPHICLASS TClaudeContextConfig;
class DELPHICLASS TClaudeStreamContentBlock;
class DELPHICLASS TAiClaudeChat;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TClaudeContextTrigger : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString TriggerType;
	int Value;
	__fastcall TClaudeContextTrigger(int aValue, System::UnicodeString aType);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TClaudeContextTrigger() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TClaudeContextEdit : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString EditType;
	TClaudeContextTrigger* Trigger;
	int Keep_ToolUses;
	int ClearAtLeast_InputTokens;
	__fastcall TClaudeContextEdit();
	__fastcall virtual ~TClaudeContextEdit();
	System::Json::TJSONObject* __fastcall ToJSONObject();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TClaudeContextConfig : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Generics::Collections::TObjectList__1<TClaudeContextEdit*>* FEdits;
	
public:
	__fastcall TClaudeContextConfig();
	__fastcall virtual ~TClaudeContextConfig();
	void __fastcall AddRule_ClearTools(int TriggerTokens, int KeepCount = 0x0, int ClearAtLeast = 0x0);
	System::Json::TJSONObject* __fastcall ToJSONObject();
	bool __fastcall IsEmpty();
	void __fastcall Clear();
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TClaudeStreamContentBlock : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	System::UnicodeString BlockType;
	System::Sysutils::TStringBuilder* TextContent;
	System::Sysutils::TStringBuilder* JsonContent;
	Umakerai::Core::TAiToolsFunction* ToolFunction;
	System::Sysutils::TStringBuilder* Signature;
	System::Json::TJSONArray* CitationsBuffer;
	System::Json::TJSONObject* ExtraData;
	__fastcall TClaudeStreamContentBlock();
	__fastcall virtual ~TClaudeStreamContentBlock();
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TAiClaudeChat : public Umakerai::Chat::TAiChat
{
	typedef Umakerai::Chat::TAiChat inherited;
	
private:
	Umakerai::Chat::TAiChatMessage* FStreamResponseMsg;
	System::Generics::Collections::TDictionary__2<int,TClaudeStreamContentBlock*>* FStreamContentBlocks;
	System::Sysutils::TStringBuilder* FStreamBuffer;
	System::UnicodeString FStreamLastEventType;
	bool FEnableMemory;
	bool FEnableThinking;
	int FThinkingBudget;
	TClaudeContextConfig* FContextConfig;
	bool FCacheSystemPrompt;
	System::UnicodeString FServiceTier;
	System::Json::TJSONArray* __fastcall GetToolJson(Umakerai::Core::TToolFormat aToolFormat);
	System::Net::Urlclient::TNetHeaders __fastcall GetDynamicHeaders();
	System::Net::Urlclient::TNetHeaders __fastcall GetFileHeaders();
	void __fastcall ClearStreamState();
	void __fastcall ProcessStreamChunk(const System::UnicodeString AChunk);
	void __fastcall SetEnableMemory(const bool Value);
	void __fastcall SetEnableThinking(const bool Value);
	void __fastcall SetThinkingBudget(const int Value);
	
protected:
	virtual void __fastcall OnInternalReceiveData(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &AAbort);
	virtual Umakerai::Chat::TAiChatMessage* __fastcall InternalAddMessage(Umakerai::Chat::TAiChatMessage* aMsg)/* overload */;
	virtual System::UnicodeString __fastcall InitChatCompletions();
	virtual void __fastcall ParseChat(System::Json::TJSONObject* jObj, Umakerai::Chat::TAiChatMessage* ResMsg);
	virtual System::UnicodeString __fastcall InternalRunCompletions(Umakerai::Chat::TAiChatMessage* ResMsg, Umakerai::Chat::TAiChatMessage* AskMsg);
	virtual Umakerai::Core::TAiToolsFunctions* __fastcall ExtractToolCallFromJson(System::Json::TJSONArray* jChoices);
	virtual void __fastcall DoCallFunction(Umakerai::Core::TAiToolsFunction* ToolCall);
	System::Json::TJSONArray* __fastcall ExtractToolCallJson(System::Json::TJSONArray* jChoices);
	
public:
	__fastcall virtual TAiClaudeChat(System::Classes::TComponent* Sender);
	__fastcall virtual ~TAiClaudeChat();
	__classmethod virtual System::Classes::TStringList* __fastcall GetModels(System::UnicodeString aApiKey, System::UnicodeString aUrl = System::UnicodeString())/* overload */;
	virtual System::Json::TJSONArray* __fastcall GetMessages();
	virtual System::UnicodeString __fastcall UploadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DownLoadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall CheckFileState(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual System::UnicodeString __fastcall DeleteFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	virtual Umakerai::Core::TAiMediaFile* __fastcall RetrieveFile(System::UnicodeString aFileId);
	virtual Umakerai::Core::TAiMediaFiles* __fastcall RetrieveFileList();
	virtual System::UnicodeString __fastcall UploadFileToCache(Umakerai::Core::TAiMediaFile* aMediaFile, int aTTL_Seconds = 0xe10);
	System::UnicodeString __fastcall CreateMessageBatch(System::UnicodeString InputFileId);
	__classmethod virtual System::UnicodeString __fastcall GetDriverName();
	__classmethod virtual void __fastcall RegisterDefaultParams(System::Classes::TStrings* Params);
	__classmethod virtual Umakerai::Chat::TAiChat* __fastcall CreateInstance(System::Classes::TComponent* Sender);
	void __fastcall ConfigureAutoContextClearing(int TriggerTokens, int KeepLast = 0x3);
	__property TClaudeContextConfig* ContextConfig = {read=FContextConfig};
	__property bool CacheSystemPrompt = {read=FCacheSystemPrompt, write=FCacheSystemPrompt, nodefault};
	
__published:
	__property bool EnableMemory = {read=FEnableMemory, write=SetEnableMemory, default=0};
	__property bool EnableThinking = {read=FEnableThinking, write=SetEnableThinking, default=0};
	__property int ThinkingBudget = {read=FThinkingBudget, write=SetThinkingBudget, default=1024};
	__property System::UnicodeString ServiceTier = {read=FServiceTier, write=FServiceTier};
	/* Hoisted overloads: */
	
protected:
	inline Umakerai::Chat::TAiChatMessage* __fastcall  InternalAddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole, System::UnicodeString aToolCallId, System::UnicodeString aFunctionName){ return Umakerai::Chat::TAiChat::InternalAddMessage(aPrompt, aRole, aToolCallId, aFunctionName); }
	inline Umakerai::Chat::TAiChatMessage* __fastcall  InternalAddMessage(System::UnicodeString aPrompt, System::UnicodeString aRole, Umakerai::Core::TAiMediaFilesArray aMediaFiles){ return Umakerai::Chat::TAiChat::InternalAddMessage(aPrompt, aRole, aMediaFiles); }
	
public:
	inline System::Classes::TStringList* __fastcall  GetModels(){ return Umakerai::Chat::TAiChat::GetModels(); }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Claude */
}	/* namespace Chat */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_CHAT_CLAUDE)
using namespace Umakerai::Chat::Claude;
#endif
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
#endif	// uMakerAi_Chat_ClaudeHPP
