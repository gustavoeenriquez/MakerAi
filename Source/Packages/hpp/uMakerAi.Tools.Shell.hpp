// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Tools.Shell.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Tools_ShellHPP
#define uMakerAi_Tools_ShellHPP

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
#include <System.JSON.hpp>
#include <System.StrUtils.hpp>
#include <System.Diagnostics.hpp>
#include <System.Generics.Collections.hpp>
#include <uMakerAi.Utils.system.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Tools
{
namespace Shell
{
//-- forward type declarations -----------------------------------------------
struct TShellExecutionResult;
class DELPHICLASS TAiShell;
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TShellExecutionResult
{
public:
	System::UnicodeString StdOut;
	System::UnicodeString StdErr;
	int ExitCode;
	bool TimedOut;
};


typedef void __fastcall (__closure *TAiShellCommandEvent)(System::TObject* Sender, const System::UnicodeString Command, const System::UnicodeString CallId, TShellExecutionResult &Result, bool &Handled);

typedef void __fastcall (__closure *TAiShellLogEvent)(System::TObject* Sender, const System::UnicodeString Command, const System::UnicodeString StdOut, const System::UnicodeString StdErr, int ExitCode);

class PASCALIMPLEMENTATION TAiShell : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Umakerai::Utils::System::TInteractiveProcessInfo* FSession;
	unsigned FTimeOut;
	System::UnicodeString FShellPath;
	TAiShellCommandEvent FOnCommand;
	bool FActive;
	System::Classes::TStringList* FEnvironment;
	int FMaxOutputSize;
	TAiShellLogEvent FOnConsoleLog;
	void __fastcall SetActive(const bool Value);
	System::UnicodeString __fastcall GenerateSentinel();
	System::UnicodeString __fastcall CleanOutput(const System::UnicodeString RawOutput, const System::UnicodeString Sentinel);
	void __fastcall StartSession();
	void __fastcall StopSession();
	TShellExecutionResult __fastcall InternalExecuteCommand(const System::UnicodeString ACommand, unsigned TimeOutMs);
	System::UnicodeString __fastcall ExecuteClaudeAction(const System::UnicodeString CallId, System::Json::TJSONObject* JArgs);
	System::UnicodeString __fastcall ExecuteOpenAIAction(const System::UnicodeString CallId, System::Json::TJSONObject* JArgs);
	System::UnicodeString __fastcall ExecuteGenericAction(const System::UnicodeString CallId, System::Json::TJSONObject* JArgs);
	void __fastcall SetShellPath(const System::UnicodeString Value);
	
public:
	__fastcall virtual TAiShell(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TAiShell();
	System::UnicodeString __fastcall Execute(const System::UnicodeString CallId, const System::UnicodeString JsonArguments)/* overload */;
	System::UnicodeString __fastcall Execute(const System::UnicodeString CallId, System::Json::TJSONObject* JArgs)/* overload */;
	System::UnicodeString __fastcall ExecuteManual(const System::UnicodeString Command);
	void __fastcall Restart();
	
__published:
	__property bool Active = {read=FActive, write=SetActive, default=0};
	__property unsigned TimeOut = {read=FTimeOut, write=FTimeOut, default=30000};
	__property System::UnicodeString ShellPath = {read=FShellPath, write=SetShellPath};
	__property int MaxOutputSize = {read=FMaxOutputSize, write=FMaxOutputSize, default=20000};
	__property TAiShellCommandEvent OnCommand = {read=FOnCommand, write=FOnCommand};
	__property TAiShellLogEvent OnConsoleLog = {read=FOnConsoleLog, write=FOnConsoleLog};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Shell */
}	/* namespace Tools */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_TOOLS_SHELL)
using namespace Umakerai::Tools::Shell;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_TOOLS)
using namespace Umakerai::Tools;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI)
using namespace Umakerai;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// uMakerAi_Tools_ShellHPP
