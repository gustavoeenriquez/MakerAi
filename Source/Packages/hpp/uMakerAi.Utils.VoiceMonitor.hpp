// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Utils.VoiceMonitor.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Utils_VoiceMonitorHPP
#define uMakerAi_Utils_VoiceMonitorHPP

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
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <System.IOUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Math.hpp>
#include <System.Permissions.hpp>
#include <System.Threading.hpp>
#include <System.Diagnostics.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.MMSystem.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Utils
{
namespace Voicemonitor
{
//-- forward type declarations -----------------------------------------------
struct TWaveInDeviceInfo;
struct TRiffHeader;
struct TFmtChunk;
struct TDataChunk;
class DELPHICLASS TAIVoiceMonitor;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TWaveInDeviceInfo
{
public:
	unsigned DeviceID;
	System::UnicodeString DeviceName;
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TRiffHeader
{
public:
	System::StaticArray<char, 4> ChunkID;
	System::LongWord ChunkSize;
	System::StaticArray<char, 4> Format;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TFmtChunk
{
public:
	System::StaticArray<char, 4> Subchunk1ID;
	System::LongWord Subchunk1Size;
	short AudioFormat;
	short NumChannels;
	System::LongWord SampleRate;
	System::LongWord ByteRate;
	short BlockAlign;
	short BitsPerSample;
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TDataChunk
{
public:
	System::StaticArray<char, 4> Subchunk2ID;
	System::LongWord Subchunk2Size;
};
#pragma pack(pop)


enum DECLSPEC_DENUM TAiMonitorState : unsigned char { msIdle, msRequestingPermission, msCalibrating, msMonitoring, msError };

typedef void __fastcall (__closure *TAIVoiceMonitorOnChange)(System::TObject* Sender, bool aUserSpeak, bool aIsValidForIA, System::Classes::TMemoryStream* aStream);

typedef void __fastcall (__closure *TSpeechEndEvent)(System::TObject* Sender, bool aIsValidForIA, System::Classes::TMemoryStream* aStream);

typedef void __fastcall (__closure *TWakeWordCheckEvent)(System::TObject* Sender, System::Classes::TMemoryStream* aWakeWordStream, bool &IsValid);

typedef void __fastcall (__closure *TAIVoiceMonitorOnCalibrated)(System::TObject* Sender, const int aNoiseLevel, const int aSensitivity, const int aStopSensitivity);

typedef void __fastcall (__closure *TAIVoiceMonitorOnUpdate)(System::TObject* Sender, const __int64 aSoundLevel);

typedef void __fastcall (__closure *TAIVoiceMonitorOnError)(System::TObject* Sender, const System::UnicodeString ErrorMessage);

typedef void __fastcall (__closure *TTranscriptionFragmentEvent)(System::TObject* Sender, System::Classes::TMemoryStream* aFragmentStream);

class PASCALIMPLEMENTATION TAIVoiceMonitor : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::Syncobjs::TCriticalSection* FCS;
	System::Sysutils::TBytes FBuffer;
	System::DynamicArray<bool> FArrBuf;
	System::Classes::TMemoryStream* FFileStream;
	bool FIsSpeaking;
	bool FActive;
	TAiMonitorState FMonitorState;
	__int64 FSoundLevel;
	bool FInDestroy;
	int FSensitivity;
	int FStopSensitivity;
	int FCalibrationSamples;
	__int64 FCalibrationAccumulator;
	int FCalibrationDurationSec;
	bool FWakeWordChecked;
	bool FIsWakeWordValid;
	int FWakeWordDurationMs;
	int FSilenceDuration;
	int FSampleRate;
	int FChannels;
	int FBitsPerSample;
	int FBufferSize;
	double FSensitivityMultiplier;
	double FStopSensitivityMultiplier;
	System::Diagnostics::TStopwatch FTranscriptionStopwatch;
	int FTranscriptionIntervalMs;
	int FTranscriptionMaxWaitMs;
	bool FWaitingForFragmentSplit;
	__int64 FLastTranscriptionPosition;
	__int64 FPeakLevelInFragment;
	double FFragmentSplitRatio;
	TTranscriptionFragmentEvent FOnTranscriptionFragment;
	TAIVoiceMonitorOnChange FOnChangeState;
	TAIVoiceMonitorOnCalibrated FOnCalibrated;
	TAIVoiceMonitorOnUpdate FOnUpdate;
	TAIVoiceMonitorOnError FOnError;
	TWakeWordCheckEvent FOnWakeWordCheck;
	int FhWaveIn;
	Winapi::Mmsystem::TWaveHdr FWaveHdr;
	int FNoiseLevel;
	unsigned FDeviceID;
	bool FWakeWordActive;
	System::UnicodeString FWakeWord;
	TSpeechEndEvent FOnSpeechEnd;
	void __fastcall SetActive(const bool Value);
	void __fastcall SetSilenceDuration(const int Value);
	void __fastcall DoError(const System::UnicodeString aMessage);
	void __fastcall StartCapture();
	void __fastcall StopCapture();
	void __fastcall UpdateAudioBuffers();
	void __fastcall FireTranscriptionFragment();
	void __fastcall SetBitsPerSample(const int Value);
	void __fastcall SetChannels(const int Value);
	void __fastcall SetSampleRate(const int Value);
	void __fastcall SetDeviceID(const unsigned Value);
	void __fastcall SetWakeWordActive(const bool Value);
	void __fastcall SetWakeWord(const System::UnicodeString Value);
	void __fastcall SetOnSpeechEnd(const TSpeechEndEvent Value);
	
protected:
	virtual void __fastcall Loaded();
	void __fastcall DoChangeState(bool aIsSpeaking);
	void __fastcall ConvertPCMToWAV(System::Classes::TMemoryStream* PCMStream, System::Classes::TMemoryStream* WAVStream);
	void __fastcall ProcessAudioBuffer(const System::Sysutils::TBytes aBuffer, int aSize);
	void __fastcall CalcSilencio();
	void __fastcall StartCaptureAudioWindows();
	void __fastcall StopCaptureAudioWindows();
	__property unsigned DeviceID = {read=FDeviceID, write=SetDeviceID, default=-1};
	
public:
	__fastcall virtual TAIVoiceMonitor(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TAIVoiceMonitor();
	__property bool Active = {read=FActive, write=SetActive, nodefault};
	__property bool IsSpeaking = {read=FIsSpeaking, nodefault};
	__property int Sensitivity = {read=FSensitivity, nodefault};
	__property int StopSensitivity = {read=FStopSensitivity, nodefault};
	__property TAiMonitorState State = {read=FMonitorState, nodefault};
	__property __int64 SoundLevel = {read=FSoundLevel};
	__property int NoiseLevel = {read=FNoiseLevel, nodefault};
	
__published:
	__property int SilenceDuration = {read=FSilenceDuration, write=SetSilenceDuration, default=1000};
	__property double SensitivityMultiplier = {read=FSensitivityMultiplier, write=FSensitivityMultiplier};
	__property double StopSensitivityMultiplier = {read=FStopSensitivityMultiplier, write=FStopSensitivityMultiplier};
	__property int WakeWordDurationMs = {read=FWakeWordDurationMs, write=FWakeWordDurationMs, default=1000};
	__property int TranscriptionIntervalMs = {read=FTranscriptionIntervalMs, write=FTranscriptionIntervalMs, default=1500};
	__property int TranscriptionMaxWaitMs = {read=FTranscriptionMaxWaitMs, write=FTranscriptionMaxWaitMs, default=4000};
	__property double FragmentSplitRatio = {read=FFragmentSplitRatio, write=FFragmentSplitRatio};
	__property TAIVoiceMonitorOnChange OnChangeState = {read=FOnChangeState, write=FOnChangeState};
	__property TAIVoiceMonitorOnCalibrated OnCalibrated = {read=FOnCalibrated, write=FOnCalibrated};
	__property TAIVoiceMonitorOnUpdate OnUpdate = {read=FOnUpdate, write=FOnUpdate};
	__property TAIVoiceMonitorOnError OnError = {read=FOnError, write=FOnError};
	__property TWakeWordCheckEvent OnWakeWordCheck = {read=FOnWakeWordCheck, write=FOnWakeWordCheck};
	__property TSpeechEndEvent OnSpeechEnd = {read=FOnSpeechEnd, write=SetOnSpeechEnd};
	__property TTranscriptionFragmentEvent OnTranscriptionFragment = {read=FOnTranscriptionFragment, write=FOnTranscriptionFragment};
	__property int SampleRate = {read=FSampleRate, write=SetSampleRate, default=44100};
	__property int Channels = {read=FChannels, write=SetChannels, default=1};
	__property int BitsPerSample = {read=FBitsPerSample, write=SetBitsPerSample, default=16};
	__property System::UnicodeString WakeWord = {read=FWakeWord, write=SetWakeWord};
	__property bool WakeWordActive = {read=FWakeWordActive, write=SetWakeWordActive, nodefault};
};


//-- var, const, procedure ---------------------------------------------------
static _DELPHI_CONST System::Word DEFAULT_SAMPLE_RATE = System::Word(0xac44);
static _DELPHI_CONST System::Int8 DEFAULT_CHANNELS = System::Int8(0x1);
static _DELPHI_CONST System::Int8 DEFAULT_BITS_PER_SAMPLE = System::Int8(0x10);
static _DELPHI_CONST System::Int8 DEFAULT_BUFFER_DURATION_MS = System::Int8(0x64);
static _DELPHI_CONST System::Int8 DEFAULT_CALIBRATION_DURATION_SEC = System::Int8(0x3);
static _DELPHI_CONST System::Word DEFAULT_SILENCE_DURATION_MS = System::Word(0x3e8);
static _DELPHI_CONST System::Word DEFAULT_WAKE_WORD_DURATION_MS = System::Word(0x3e8);
static _DELPHI_CONST System::Word DEFAULT_TRANSCRIPTION_INTERVAL_MS = System::Word(0x5dc);
static _DELPHI_CONST System::Word DEFAULT_TRANSCRIPTION_MAX_WAIT_MS = System::Word(0xfa0);
#define DEFAULT_FRAGMENT_SPLIT_RATIO  (3.500000E-01)
extern DELPHI_PACKAGE void __fastcall Register(void);
extern DELPHI_PACKAGE System::DynamicArray<TWaveInDeviceInfo> __fastcall GetWaveInDevices(void);
}	/* namespace Voicemonitor */
}	/* namespace Utils */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_UTILS_VOICEMONITOR)
using namespace Umakerai::Utils::Voicemonitor;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_UTILS)
using namespace Umakerai::Utils;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_Utils_VoiceMonitorHPP
