// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Whisper.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_WhisperHPP
#define uMakerAi_WhisperHPP

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
#include <uMakerAi.Core.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Whisper
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAIWhisper;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TAIWhisper : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FUrl;
	System::UnicodeString FModel;
	System::UnicodeString FVoice;
	System::UnicodeString FFormat;
	System::UnicodeString FLanguaje;
	float FTemperature;
	float FSpeed;
	System::UnicodeString FResponseFormat;
	System::UnicodeString FQuality;
	System::UnicodeString Ftimestamp_granularities;
	void __fastcall SetApiKey(const System::UnicodeString Value);
	void __fastcall SetUrl(const System::UnicodeString Value);
	void __fastcall SetModel(const System::UnicodeString Value);
	void __fastcall SetVoice(const System::UnicodeString Value);
	void __fastcall SetFormat(const System::UnicodeString Value);
	void __fastcall SetLanguaje(const System::UnicodeString Value);
	void __fastcall SetSpeed(const float Value);
	void __fastcall SetTemperature(const float Value);
	void __fastcall SetResponseFormat(const System::UnicodeString Value);
	void __fastcall SetQuality(const System::UnicodeString Value);
	void __fastcall Settimestamp_granularities(const System::UnicodeString Value);
	System::UnicodeString __fastcall GetApiKey();
	
protected:
	bool __fastcall IsValidExtension(System::UnicodeString FileExtension);
	void __fastcall ConvertAudioIfNeeded(System::Classes::TMemoryStream* &aStream, System::UnicodeString &aFileName);
	
public:
	__fastcall virtual TAIWhisper(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAIWhisper();
	System::Classes::TMemoryStream* __fastcall Speech(System::UnicodeString aText, System::UnicodeString aVoice = System::UnicodeString());
	System::UnicodeString __fastcall Transcription(System::Classes::TMemoryStream* aStream, System::UnicodeString aFileName, System::UnicodeString aPrompt);
	System::UnicodeString __fastcall Translation(System::Classes::TMemoryStream* aStream, System::UnicodeString aFileName, System::UnicodeString aPrompt);
	
__published:
	__property System::UnicodeString ApiKey = {read=GetApiKey, write=SetApiKey};
	__property System::UnicodeString Url = {read=FUrl, write=SetUrl};
	__property System::UnicodeString Model = {read=FModel, write=SetModel};
	__property System::UnicodeString Voice = {read=FVoice, write=SetVoice};
	__property System::UnicodeString Format = {read=FFormat, write=SetFormat};
	__property System::UnicodeString Languaje = {read=FLanguaje, write=SetLanguaje};
	__property float Speed = {read=FSpeed, write=SetSpeed};
	__property float Temperature = {read=FTemperature, write=SetTemperature};
	__property System::UnicodeString ResponseFormat = {read=FResponseFormat, write=SetResponseFormat};
	__property System::UnicodeString Quality = {read=FQuality, write=SetQuality};
	__property System::UnicodeString timestamp_granularities = {read=Ftimestamp_granularities, write=Settimestamp_granularities};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Whisper */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_WHISPER)
using namespace Umakerai::Whisper;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_WhisperHPP
