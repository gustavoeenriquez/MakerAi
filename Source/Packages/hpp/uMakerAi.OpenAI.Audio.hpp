// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.OpenAI.Audio.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_OpenAI_AudioHPP
#define uMakerAi_OpenAI_AudioHPP

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
#include <System.Rtti.hpp>
#include <System.StrUtils.hpp>
#include <System.Net.URLClient.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.HttpClientComponent.hpp>
#include <REST.Json.hpp>
#include <REST.Types.hpp>
#include <REST.Client.hpp>
#include <uMakerAi.Core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Openai
{
namespace Audio
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTranscriptionResult;
class DELPHICLASS TAiOpenAiAudio;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAiTTSModel : unsigned char { tts_1, tts_1_hd, gpt_4o_mini_tts };

enum DECLSPEC_DENUM TAiTTSVoice : unsigned char { tvAlloy, tvAsh, tvBallad, tvCoral, tvEcho, tvFable, tvOnyx, tvNova, tvSage, tvShimmer, tvVerse };

enum DECLSPEC_DENUM TAiTTSResponseFormat : unsigned char { trfMp3, trfOpus, trfAac, trfFlac, trfWav, trfPcm };

enum DECLSPEC_DENUM TAiTranscriptionModel : unsigned char { tmWhisper1, tmGpt4o, tmGpt4oMini, tmGpt4oDiarize };

enum DECLSPEC_DENUM TAiTranscriptionResponseFormat : unsigned char { trfJson, trfText, trfSrt, trfVerboseJson, trfVtt, trfDiarizedJson };

enum DECLSPEC_DENUM TStreamOperation : unsigned char { soNone, soSpeech, soTranscription };

class PASCALIMPLEMENTATION TTranscriptionResult : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FText;
	System::Json::TJSONObject* FJsonObject;
	double FDuration;
	System::UnicodeString FLanguage;
	
public:
	__fastcall TTranscriptionResult(const System::UnicodeString AResponse, TAiTranscriptionResponseFormat AFormat);
	__fastcall virtual ~TTranscriptionResult();
	__property System::UnicodeString Text = {read=FText};
	__property double Duration = {read=FDuration};
	__property System::UnicodeString Language = {read=FLanguage};
	__property System::Json::TJSONObject* RawJson = {read=FJsonObject};
};


typedef void __fastcall (__closure *TOnAudioChunkReceived)(System::TObject* Sender, const System::Sysutils::TBytes AAudioChunk);

typedef void __fastcall (__closure *TOnSpeechCompleted)(System::TObject* Sender);

typedef void __fastcall (__closure *TOnTranscriptDeltaReceived)(System::TObject* Sender, const System::UnicodeString ATextDelta);

typedef void __fastcall (__closure *TOnTranscriptionCompleted)(System::TObject* Sender, TTranscriptionResult* const AFinalResult);

typedef void __fastcall (__closure *TOnAudioError)(System::TObject* Sender, const System::UnicodeString AMessage);

class PASCALIMPLEMENTATION TAiOpenAiAudio : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FUrl;
	TAiTTSModel FTTSModel;
	TAiTTSVoice FTTSVoice;
	TAiTTSResponseFormat FTTSResponseFormat;
	double FTTSSpeed;
	System::UnicodeString FTTSInstructions;
	TAiTranscriptionModel FTranscriptionModel;
	TAiTranscriptionResponseFormat FTranscriptionResponseFormat;
	System::UnicodeString FTranscriptionLanguage;
	double FTranscriptionTemperature;
	Umakerai::Core::TAiTimestampGranularities FTranscriptionTimestampGranularities;
	TOnAudioChunkReceived FOnAudioChunkReceived;
	TOnSpeechCompleted FOnSpeechCompleted;
	TOnTranscriptDeltaReceived FOnTranscriptDeltaReceived;
	TOnTranscriptionCompleted FOnTranscriptionCompleted;
	TOnAudioError FOnAudioError;
	System::Sysutils::TStringBuilder* FStreamBuffer;
	__int64 FBytesProcessed;
	System::Classes::TMemoryStream* FActiveResponseStream;
	TStreamOperation FCurrentStreamOperation;
	System::UnicodeString __fastcall GetApiKey();
	void __fastcall SetApiKey(const System::UnicodeString Value);
	void __fastcall SetUrl(const System::UnicodeString Value);
	
protected:
	bool __fastcall ConvertAudioIfNeeded(Umakerai::Core::TAiMediaFile* aMediaFile);
	void __fastcall HandleStreamEvent(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &AAbort);
	void __fastcall ProcessSpeechStreamBuffer();
	void __fastcall ProcessTranscriptionStreamBuffer();
	void __fastcall BuildTranscriptionBody(System::Net::Mime::TMultipartFormData* const ABody, Umakerai::Core::TAiMediaFile* const AAudioFile, const System::UnicodeString APrompt = System::UnicodeString());
	
public:
	__fastcall virtual TAiOpenAiAudio(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiOpenAiAudio();
	System::Classes::TMemoryStream* __fastcall Speech(const System::UnicodeString AInput)/* overload */;
	void __fastcall Speech(const System::UnicodeString AInput, System::Classes::TStream* const AOutputStream)/* overload */;
	void __fastcall SpeechStreamed(const System::UnicodeString AInput);
	TTranscriptionResult* __fastcall Transcribe(Umakerai::Core::TAiMediaFile* const AAudioFile, const System::UnicodeString APrompt = System::UnicodeString());
	void __fastcall TranscribeStreamed(Umakerai::Core::TAiMediaFile* const AAudioFile);
	TTranscriptionResult* __fastcall TranslateToEnglish(Umakerai::Core::TAiMediaFile* const AAudioFile, const System::UnicodeString APrompt = System::UnicodeString());
	
__published:
	__property System::UnicodeString ApiKey = {read=GetApiKey, write=SetApiKey};
	__property System::UnicodeString Url = {read=FUrl, write=SetUrl};
	__property TAiTTSModel TTSModel = {read=FTTSModel, write=FTTSModel, default=0};
	__property TAiTTSVoice TTSVoice = {read=FTTSVoice, write=FTTSVoice, default=0};
	__property TAiTTSResponseFormat TTSResponseFormat = {read=FTTSResponseFormat, write=FTTSResponseFormat, default=0};
	__property double TTSSpeed = {read=FTTSSpeed, write=FTTSSpeed};
	__property System::UnicodeString TTSInstructions = {read=FTTSInstructions, write=FTTSInstructions};
	__property TAiTranscriptionModel TranscriptionModel = {read=FTranscriptionModel, write=FTranscriptionModel, default=0};
	__property TAiTranscriptionResponseFormat TranscriptionResponseFormat = {read=FTranscriptionResponseFormat, write=FTranscriptionResponseFormat, default=0};
	__property System::UnicodeString TranscriptionLanguage = {read=FTranscriptionLanguage, write=FTranscriptionLanguage};
	__property double TranscriptionTemperature = {read=FTranscriptionTemperature, write=FTranscriptionTemperature};
	__property Umakerai::Core::TAiTimestampGranularities TranscriptionTimestampGranularities = {read=FTranscriptionTimestampGranularities, write=FTranscriptionTimestampGranularities, nodefault};
	__property TOnAudioChunkReceived OnAudioChunkReceived = {read=FOnAudioChunkReceived, write=FOnAudioChunkReceived};
	__property TOnSpeechCompleted OnSpeechCompleted = {read=FOnSpeechCompleted, write=FOnSpeechCompleted};
	__property TOnTranscriptDeltaReceived OnTranscriptDeltaReceived = {read=FOnTranscriptDeltaReceived, write=FOnTranscriptDeltaReceived};
	__property TOnTranscriptionCompleted OnTranscriptionCompleted = {read=FOnTranscriptionCompleted, write=FOnTranscriptionCompleted};
	__property TOnAudioError OnAudioError = {read=FOnAudioError, write=FOnAudioError};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Audio */
}	/* namespace Openai */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_OPENAI_AUDIO)
using namespace Umakerai::Openai::Audio;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_OPENAI)
using namespace Umakerai::Openai;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_OpenAI_AudioHPP
