// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.OpenAi.Dalle.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_OpenAi_DalleHPP
#define uMakerAi_OpenAi_DalleHPP

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
#include <System.Math.hpp>
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
namespace Dalle
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiDalleImage;
class DELPHICLASS TAiDalle;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TAiDalleImage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TMemoryStream* FImageStream;
	System::UnicodeString FBase64;
	System::UnicodeString FRevisedPrompt;
	System::UnicodeString FUrlFile;
	System::UnicodeString FBackground;
	System::UnicodeString FOutputFormat;
	System::UnicodeString FQuality;
	System::UnicodeString FSize;
	System::Json::TJSONObject* FUsage;
	System::Classes::TMemoryStream* __fastcall GetImage();
	
protected:
	System::Classes::TMemoryStream* __fastcall Base64ToStream(const System::UnicodeString ABase64);
	void __fastcall LoadImageFromUrl(const System::UnicodeString AUrlFile);
	void __fastcall ParseData(System::Json::TJSONObject* JObj);
	
public:
	__fastcall TAiDalleImage();
	__fastcall virtual ~TAiDalleImage();
	void __fastcall ParseStreamEvent(System::Json::TJSONObject* JObj);
	__property System::UnicodeString RevisedPrompt = {read=FRevisedPrompt};
	__property System::UnicodeString Base64 = {read=FBase64};
	__property System::UnicodeString UrlFile = {read=FUrlFile};
	__property System::Classes::TMemoryStream* Image = {read=GetImage};
	__property System::UnicodeString Background = {read=FBackground};
	__property System::UnicodeString OutputFormat = {read=FOutputFormat};
	__property System::UnicodeString Quality = {read=FQuality};
	__property System::UnicodeString Size = {read=FSize};
	__property System::Json::TJSONObject* Usage = {read=FUsage};
};

#pragma pack(pop)

typedef System::DynamicArray<TAiDalleImage*> TAiDalleImages;

typedef void __fastcall (__closure *TOnPartialImageReceived)(System::TObject* Sender, TAiDalleImage* const APartialImage, int AIndex);

typedef void __fastcall (__closure *TOnStreamCompleted)(System::TObject* Sender, TAiDalleImage* const AFinalImage);

typedef void __fastcall (__closure *TOnStreamError)(System::TObject* Sender, const System::UnicodeString AErrorMessage);

enum DECLSPEC_DENUM TAiImageModel : unsigned char { imDallE2, imDallE3, imGptImage1 };

enum DECLSPEC_DENUM TAiImageQuality : unsigned char { iqAuto, iqStandard, iqHD, iqHigh, iqMedium, iqLow };

enum DECLSPEC_DENUM TAiImageBackground : unsigned char { ibAuto, ibTransparent, ibOpaque };

enum DECLSPEC_DENUM TAiImageOutputFormat : unsigned char { ifPng, ifJpeg, ifWebp };

enum DECLSPEC_DENUM TAiImageStyle : unsigned char { isVivid, isNatural };

enum DECLSPEC_DENUM TAiImageResponseFormat : unsigned char { irfUrl, irfBase64Json };

enum DECLSPEC_DENUM TAiImageSize : unsigned char { is256x256, is512x512, is1024x1024, is1792x1024, is1024x1792, is1536x1024, is1024x1536 };

class PASCALIMPLEMENTATION TAiDalle : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FApiKey;
	TAiDalleImages FImages;
	System::UnicodeString FPrompt;
	System::UnicodeString FUrl;
	System::UnicodeString FUser;
	TAiImageModel FModel;
	TAiImageResponseFormat FResponseFormat;
	TAiImageQuality FQuality;
	TAiImageStyle FStyle;
	bool FStream;
	TAiImageBackground FBackground;
	TAiImageOutputFormat FOutputFormat;
	TOnPartialImageReceived FOnPartialImageReceived;
	TOnStreamCompleted FOnStreamCompleted;
	TOnStreamError FOnStreamError;
	System::Sysutils::TStringBuilder* FStreamBuffer;
	__int64 FBytesProcessed;
	System::Classes::TMemoryStream* FActiveResponseStream;
	System::UnicodeString __fastcall GetApiKey();
	void __fastcall SetApiKey(const System::UnicodeString Value);
	void __fastcall SetModel(const TAiImageModel Value);
	void __fastcall SetQuality(const TAiImageQuality Value);
	void __fastcall SetResponseFormat(const TAiImageResponseFormat Value);
	void __fastcall SetStyle(const TAiImageStyle Value);
	void __fastcall SetStream(const bool Value);
	void __fastcall SetBackground(const TAiImageBackground Value);
	void __fastcall SetOutputFormat(const TAiImageOutputFormat Value);
	
protected:
	void __fastcall ClearImages();
	void __fastcall ParseResponse(System::Json::TJSONObject* JObj);
	void __fastcall HandleStreamData(System::TObject* const Sender, __int64 AContentLength, __int64 AReadCount, bool &AAbort);
	void __fastcall ProcessStreamBuffer();
	
public:
	__fastcall virtual TAiDalle(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAiDalle();
	TAiDalleImage* __fastcall Generate(const System::UnicodeString aPrompt, TAiImageSize aSize, int N = 0x1);
	TAiDalleImage* __fastcall Edit(Umakerai::Core::TAiMediaFiles* aMediaFiles, Umakerai::Core::TAiMediaFile* aMaskFile, const System::UnicodeString aPrompt, TAiImageSize aSize, int N = 0x1);
	TAiDalleImage* __fastcall Variation(Umakerai::Core::TAiMediaFile* aImageFile, TAiImageSize aSize, int N = 0x1);
	
__published:
	__property System::UnicodeString Url = {read=FUrl, write=FUrl};
	__property System::UnicodeString ApiKey = {read=GetApiKey, write=SetApiKey};
	__property System::UnicodeString Prompt = {read=FPrompt};
	__property System::UnicodeString User = {read=FUser, write=FUser};
	__property TAiImageModel Model = {read=FModel, write=SetModel, default=1};
	__property TAiImageQuality Quality = {read=FQuality, write=SetQuality, default=0};
	__property TAiImageStyle Style = {read=FStyle, write=SetStyle, default=0};
	__property TAiImageResponseFormat ResponseFormat = {read=FResponseFormat, write=SetResponseFormat, nodefault};
	__property bool Stream = {read=FStream, write=SetStream, default=0};
	__property TAiImageBackground Background = {read=FBackground, write=SetBackground, default=0};
	__property TAiImageOutputFormat OutputFormat = {read=FOutputFormat, write=SetOutputFormat, default=0};
	__property TOnPartialImageReceived OnPartialImageReceived = {read=FOnPartialImageReceived, write=FOnPartialImageReceived};
	__property TOnStreamCompleted OnStreamCompleted = {read=FOnStreamCompleted, write=FOnStreamCompleted};
	__property TOnStreamError OnStreamError = {read=FOnStreamError, write=FOnStreamError};
};


//-- var, const, procedure ---------------------------------------------------
#define GlOpenAIUrl L"https://api.openai.com/v1/"
}	/* namespace Dalle */
}	/* namespace Openai */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_OPENAI_DALLE)
using namespace Umakerai::Openai::Dalle;
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
#endif	// uMakerAi_OpenAi_DalleHPP
