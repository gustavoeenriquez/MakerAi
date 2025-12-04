// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Utils.system.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Utils_systemHPP
#define uMakerAi_Utils_systemHPP

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
#include <System.IOUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Character.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.ShellAPI.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Utils
{
namespace System
{
//-- forward type declarations -----------------------------------------------
struct TPipeHandles;
class DELPHICLASS TInteractiveProcessInfo;
class DELPHICLASS TUtilsSystem;
//-- type declarations -------------------------------------------------------
typedef Winapi::Windows::THandle TProcessHandle;

typedef Winapi::Windows::THandle TPipeHandle;

struct DECLSPEC_DRECORD TPipeHandles
{
public:
	TPipeHandle InputRead;
	TPipeHandle InputWrite;
	TPipeHandle OutputRead;
	TPipeHandle OutputWrite;
	TPipeHandle ErrorRead;
	TPipeHandle ErrorWrite;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TInteractiveProcessInfo : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TProcessHandle ProcessHandle;
	Winapi::Windows::THandle ThreadHandle;
	unsigned ProcessID;
	TPipeHandles PipeHandles;
	bool Running;
	unsigned ExitCode;
	__fastcall TInteractiveProcessInfo();
	__fastcall virtual ~TInteractiveProcessInfo();
	bool __fastcall IsRunning();
	bool __fastcall WaitOnExit(unsigned ATimeoutMs = (unsigned)(0xffffffff));
	void __fastcall Terminate();
	void __fastcall Kill();
	int __fastcall WriteInput(const void *Buffer, int Count);
	int __fastcall ReadOutput(void *Buffer, int Count);
	int __fastcall ReadError(void *Buffer, int Count);
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TUtilsSystem : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod System::UnicodeString __fastcall RunCommandLine(System::UnicodeString ACommand)/* overload */;
	__classmethod bool __fastcall ExecuteCommandLine(System::UnicodeString ACommand);
	__classmethod TInteractiveProcessInfo* __fastcall StartInteractiveProcess(const System::UnicodeString ACommand, System::UnicodeString ACurrentDirectory = System::UnicodeString(), System::Classes::TStrings* AEnvironment = (System::Classes::TStrings*)(0x0));
	__classmethod void __fastcall StopInteractiveProcess(TInteractiveProcessInfo* &AProcessInfo);
	__classmethod System::Classes::TStringList* __fastcall GetSystemEnvironment();
	__classmethod bool __fastcall ShellOpenFile(const System::UnicodeString AFileName);
public:
	/* TObject.Create */ inline __fastcall TUtilsSystem() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TUtilsSystem() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace System */
}	/* namespace Utils */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_UTILS_SYSTEM)
using namespace Umakerai::Utils::System;
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
#endif	// uMakerAi_Utils_systemHPP
