// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Gemini.Veo.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Gemini_VeoHPP
#define uMakerAi_Gemini_VeoHPP

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
#include <uMakerAi.Chat.Gemini.hpp>
#include <uMakerAi.Core.hpp>
#include <System.Net.HttpClientComponent.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Gemini
{
namespace Veo
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiVeoGenerator;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TVeoModel : unsigned char { vmCustom, vmVeo3_1, vmVeo3_1_Fast, vmVeo3_0, vmVeo3_0_Fast, vmVeo2_0 };

enum DECLSPEC_DENUM TVeoAspectRatio : unsigned char { arDefault, ar16x9, ar9x16 };

enum DECLSPEC_DENUM TVeoResolution : unsigned char { vrDefault, vr720p, vr1080p };

enum DECLSPEC_DENUM TVeoPersonGeneration : unsigned char { pgDefault, pgAllowAll, pgAllowAdult, pgDontAllow };

typedef void __fastcall (__closure *TProgressEvent)(System::TObject* Sender, const System::UnicodeString StatusMessage);

typedef void __fastcall (__closure *TGenerationSuccessEvent)(System::TObject* Sender, Umakerai::Core::TAiMediaFile* ResultVideo);

typedef void __fastcall (__closure *TGenerationErrorEvent)(System::TObject* Sender, const System::UnicodeString ErrorMessage);

class PASCALIMPLEMENTATION TAiVeoGenerator : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FApiKey;
	TVeoModel FModel;
	System::UnicodeString FCustomModelName;
	TVeoAspectRatio FAspectRatio;
	TVeoResolution FResolution;
	int FDurationSeconds;
	TVeoPersonGeneration FPersonGeneration;
	__int64 FSeed;
	System::UnicodeString FNegativePrompt;
	TProgressEvent FOnProgress;
	TGenerationSuccessEvent FOnSuccess;
	TGenerationErrorEvent FOnError;
	System::UnicodeString __fastcall GetEffectiveModelName();
	System::Json::TJSONObject* __fastcall BuildParametersJson();
	void __fastcall DoError(const System::UnicodeString AMessage);
	void __fastcall DoProgress(const System::UnicodeString AMessage);
	void __fastcall DoSuccess(Umakerai::Core::TAiMediaFile* AVideoFile);
	void __fastcall InternalExecuteGeneration(System::Json::TJSONObject* ARequestBody);
	Umakerai::Core::TAiMediaFile* __fastcall DownloadVideoFile(const System::UnicodeString AVideoUri);
	System::UnicodeString __fastcall GetApiKey();
	
public:
	__fastcall virtual TAiVeoGenerator(System::Classes::TComponent* AOwner);
	System::Threading::_di_ITask __fastcall GenerateFromText(const System::UnicodeString APrompt);
	System::Threading::_di_ITask __fastcall GenerateFromImage(const System::UnicodeString APrompt, Umakerai::Core::TAiMediaFile* AImage);
	System::Threading::_di_ITask __fastcall GenerateFromFrames(const System::UnicodeString APrompt, Umakerai::Core::TAiMediaFile* AFirstFrame, Umakerai::Core::TAiMediaFile* ALastFrame);
	System::Threading::_di_ITask __fastcall GenerateWithReferences(const System::UnicodeString APrompt, Umakerai::Core::TAiMediaFilesArray AReferenceImages);
	System::Threading::_di_ITask __fastcall ExtendVideo(const System::UnicodeString APrompt, Umakerai::Core::TAiMediaFile* AVideoToExtend);
	System::UnicodeString __fastcall UploadFile(Umakerai::Core::TAiMediaFile* aMediaFile);
	void __fastcall UploadFileSync(Umakerai::Core::TAiMediaFile* aMediaFile);
	
__published:
	__property System::UnicodeString ApiKey = {read=GetApiKey, write=FApiKey};
	__property TVeoModel Model = {read=FModel, write=FModel, default=1};
	__property System::UnicodeString CustomModelName = {read=FCustomModelName, write=FCustomModelName};
	__property TVeoAspectRatio AspectRatio = {read=FAspectRatio, write=FAspectRatio, default=1};
	__property TVeoResolution Resolution = {read=FResolution, write=FResolution, default=1};
	__property int DurationSeconds = {read=FDurationSeconds, write=FDurationSeconds, default=8};
	__property TVeoPersonGeneration PersonGeneration = {read=FPersonGeneration, write=FPersonGeneration, default=1};
	__property System::UnicodeString NegativePrompt = {read=FNegativePrompt, write=FNegativePrompt};
	__property __int64 Seed = {read=FSeed, write=FSeed, default=0};
	__property TProgressEvent OnProgress = {read=FOnProgress, write=FOnProgress};
	__property TGenerationSuccessEvent OnSuccess = {read=FOnSuccess, write=FOnSuccess};
	__property TGenerationErrorEvent OnError = {read=FOnError, write=FOnError};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiVeoGenerator() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Veo */
}	/* namespace Gemini */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_GEMINI_VEO)
using namespace Umakerai::Gemini::Veo;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_GEMINI)
using namespace Umakerai::Gemini;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_Gemini_VeoHPP
