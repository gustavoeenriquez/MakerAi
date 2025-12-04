// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.OpenAI.Sora.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_OpenAI_SoraHPP
#define uMakerAi_OpenAI_SoraHPP

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
#include <System.Threading.hpp>
#include <System.JSON.hpp>
#include <System.Net.HttpClient.hpp>
#include <System.Net.URLClient.hpp>
#include <uMakerAi.Core.hpp>
#include <System.Net.HttpClientComponent.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Openai
{
namespace Sora
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiSoraGenerator;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TSoraModel : unsigned char { smSora2, smCustom };

enum DECLSPEC_DENUM TSoraResolution : unsigned char { srDefault, sr1280x720, sr720x1280, sr1920x1080, sr1080x1920 };

typedef void __fastcall (__closure *TProgressEvent)(System::TObject* Sender, const System::UnicodeString StatusMessage);

typedef void __fastcall (__closure *TGenerationSuccessEvent)(System::TObject* Sender, Umakerai::Core::TAiMediaFile* ResultVideo);

typedef void __fastcall (__closure *TGenerationErrorEvent)(System::TObject* Sender, const System::UnicodeString ErrorMessage);

class PASCALIMPLEMENTATION TAiSoraGenerator : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FApiKey;
	TSoraModel FModel;
	System::UnicodeString FCustomModelName;
	TSoraResolution FResolution;
	int FSeconds;
	__int64 FSeed;
	TProgressEvent FOnProgress;
	TGenerationSuccessEvent FOnSuccess;
	TGenerationErrorEvent FOnError;
	System::UnicodeString __fastcall GetEffectiveModelName();
	System::UnicodeString __fastcall GetResolutionString();
	void __fastcall DoError(const System::UnicodeString AMessage);
	void __fastcall DoProgress(const System::UnicodeString AMessage);
	void __fastcall DoSuccess(Umakerai::Core::TAiMediaFile* AVideoFile);
	void __fastcall InternalExecuteGeneration(const System::UnicodeString APrompt, Umakerai::Core::TAiMediaFile* AInputImage = (Umakerai::Core::TAiMediaFile*)(0x0));
	void __fastcall InternalExecuteRemix(const System::UnicodeString APrompt, const System::UnicodeString AVideoIdToRemix);
	void __fastcall PollAndDownloadVideoJob(System::Net::Httpclientcomponent::TNetHTTPClient* AHttpClient, const System::UnicodeString AVideoJobId, const System::UnicodeString AInitialStatus, const System::Net::Urlclient::TNetHeaders AHeaders);
	Umakerai::Core::TAiMediaFile* __fastcall DownloadVideoContent(System::Net::Httpclientcomponent::TNetHTTPClient* AHttpClient, const System::UnicodeString AVideoJobId, const System::Net::Urlclient::TNetHeaders AHeaders);
	System::UnicodeString __fastcall GetApiKey();
	
public:
	__fastcall virtual TAiSoraGenerator(System::Classes::TComponent* AOwner);
	System::Threading::_di_ITask __fastcall GenerateFromText(const System::UnicodeString APrompt);
	System::Threading::_di_ITask __fastcall GenerateFromImage(const System::UnicodeString APrompt, Umakerai::Core::TAiMediaFile* AImage);
	System::Threading::_di_ITask __fastcall RemixVideo(const System::UnicodeString APrompt, const System::UnicodeString AOriginalVideoId);
	
__published:
	__property System::UnicodeString ApiKey = {read=GetApiKey, write=FApiKey};
	__property TSoraModel Model = {read=FModel, write=FModel, default=0};
	__property System::UnicodeString CustomModelName = {read=FCustomModelName, write=FCustomModelName};
	__property TSoraResolution Resolution = {read=FResolution, write=FResolution, default=2};
	__property int Seconds = {read=FSeconds, write=FSeconds, default=4};
	__property __int64 Seed = {read=FSeed, write=FSeed, default=0};
	__property TProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property TGenerationSuccessEvent OnSuccess = {read=FOnSuccess, write=FOnSuccess};
	__property TGenerationErrorEvent OnError = {read=FOnError, write=FOnError};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiSoraGenerator() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Sora */
}	/* namespace Openai */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_OPENAI_SORA)
using namespace Umakerai::Openai::Sora;
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
#endif	// uMakerAi_OpenAI_SoraHPP
