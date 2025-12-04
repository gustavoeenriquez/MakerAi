// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Utils.PcmToWav.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Utils_PcmToWavHPP
#define uMakerAi_Utils_PcmToWavHPP

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

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Utils
{
namespace Pcmtowav
{
//-- forward type declarations -----------------------------------------------
struct TWAVHeader;
//-- type declarations -------------------------------------------------------
#pragma pack(push,1)
struct DECLSPEC_DRECORD TWAVHeader
{
public:
	System::StaticArray<char, 4> ChunkID;
	unsigned ChunkSize;
	System::StaticArray<char, 4> Format;
	System::StaticArray<char, 4> Subchunk1ID;
	unsigned Subchunk1Size;
	System::Word AudioFormat;
	System::Word NumChannels;
	unsigned SampleRate;
	unsigned ByteRate;
	System::Word BlockAlign;
	System::Word BitsPerSample;
	System::StaticArray<char, 4> Subchunk2ID;
	unsigned Subchunk2Size;
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE bool __fastcall ConvertPCMToWAV(const System::UnicodeString PCMFilePath, const System::UnicodeString WAVFilePath, unsigned SampleRate = (unsigned)(0xac44), System::Word Channels = (System::Word)(0x2), System::Word BitsPerSample = (System::Word)(0x10));
extern DELPHI_PACKAGE bool __fastcall ConvertPCMStreamToWAVStream(System::Classes::TMemoryStream* const PCMStream, /* out */ System::Classes::TMemoryStream* &WAVStream, unsigned SampleRate = (unsigned)(0xac44), System::Word Channels = (System::Word)(0x2), System::Word BitsPerSample = (System::Word)(0x10));
}	/* namespace Pcmtowav */
}	/* namespace Utils */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_UTILS_PCMTOWAV)
using namespace Umakerai::Utils::Pcmtowav;
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
#endif	// uMakerAi_Utils_PcmToWavHPP
