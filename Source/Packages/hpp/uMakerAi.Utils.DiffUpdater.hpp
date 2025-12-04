// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Utils.DiffUpdater.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Utils_DiffUpdaterHPP
#define uMakerAi_Utils_DiffUpdaterHPP

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
#include <System.Generics.Defaults.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Utils
{
namespace Diffupdater
{
//-- forward type declarations -----------------------------------------------
struct TDiffLine;
class DELPHICLASS TDiffHunk;
class DELPHICLASS TDiffParser;
class DELPHICLASS TDiffApplier;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TDiffOperation : unsigned char { doContext, doAdd, doDelete };

struct DECLSPEC_DRECORD TDiffLine
{
public:
	TDiffOperation Operation;
	System::UnicodeString Content;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TDiffHunk : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FOriginalStart;
	int FOriginalCount;
	int FNewStart;
	int FNewCount;
	System::Generics::Collections::TList__1<TDiffLine>* FLines;
	
public:
	__fastcall TDiffHunk();
	__fastcall virtual ~TDiffHunk();
	__property int OriginalStart = {read=FOriginalStart, write=FOriginalStart, nodefault};
	__property int OriginalCount = {read=FOriginalCount, write=FOriginalCount, nodefault};
	__property int NewStart = {read=FNewStart, write=FNewStart, nodefault};
	__property int NewCount = {read=FNewCount, write=FNewCount, nodefault};
	__property System::Generics::Collections::TList__1<TDiffLine>* Lines = {read=FLines};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDiffParser : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool __fastcall ParseHunkHeader(const System::UnicodeString Line, /* out */ TDiffHunk* &Hunk);
	
public:
	System::Generics::Collections::TList__1<TDiffHunk*>* __fastcall Parse(const System::UnicodeString DiffText);
public:
	/* TObject.Create */ inline __fastcall TDiffParser() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDiffParser() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TDiffApplier : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool __fastcall MatchHunkAt(System::Classes::TStringList* const FileLines, TDiffHunk* Hunk, int StartIndex);
	bool __fastcall FindHunkPosition(System::Classes::TStringList* const FileLines, TDiffHunk* Hunk, /* out */ int &ActualStart);
	
public:
	bool __fastcall Apply(const System::UnicodeString OriginalContent, const System::UnicodeString DiffText, /* out */ System::UnicodeString &NewContent, /* out */ System::UnicodeString &ErrorMsg);
public:
	/* TObject.Create */ inline __fastcall TDiffApplier() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TDiffApplier() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Diffupdater */
}	/* namespace Utils */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_UTILS_DIFFUPDATER)
using namespace Umakerai::Utils::Diffupdater;
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
#endif	// uMakerAi_Utils_DiffUpdaterHPP
