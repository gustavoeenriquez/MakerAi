// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Utils.CodeExtractor.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Utils_CodeExtractorHPP
#define uMakerAi_Utils_CodeExtractorHPP

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
#include <System.RegularExpressions.hpp>
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Utils
{
namespace Codeextractor
{
//-- forward type declarations -----------------------------------------------
struct TCodeFile;
class DELPHICLASS TMarkdownCodeExtractor;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TCodeFile
{
public:
	System::UnicodeString FileName;
	System::UnicodeString FileType;
	System::UnicodeString Code;
	int LineNumber;
};


typedef System::Generics::Collections::TList__1<TCodeFile> TCodeFileList;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMarkdownCodeExtractor : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TCodeFileList* FCodeFiles;
	System::UnicodeString __fastcall NormalizeLanguage(const System::UnicodeString ALanguage);
	
public:
	__fastcall TMarkdownCodeExtractor();
	__fastcall virtual ~TMarkdownCodeExtractor();
	TCodeFileList* __fastcall ExtractCodeFiles(const System::UnicodeString AMarkdownText);
	void __fastcall Clear();
	__property TCodeFileList* CodeFiles = {read=FCodeFiles};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Codeextractor */
}	/* namespace Utils */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_UTILS_CODEEXTRACTOR)
using namespace Umakerai::Utils::Codeextractor;
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
#endif	// uMakerAi_Utils_CodeExtractorHPP
