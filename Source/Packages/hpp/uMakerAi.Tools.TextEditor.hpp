// CodeGear C++Builder
// Copyright (c) 1995, 2024 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'uMakerAi.Tools.TextEditor.pas' rev: 36.00 (Windows)

#ifndef uMakerAi_Tools_TextEditorHPP
#define uMakerAi_Tools_TextEditorHPP

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
#include <System.IOUtils.hpp>
#include <System.StrUtils.hpp>
#include <uMakerAi.Utils.DiffUpdater.hpp>

//-- user supplied -----------------------------------------------------------

namespace Umakerai
{
namespace Tools
{
namespace Texteditor
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TAiTextEditorTool;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TAiFileReadEvent)(System::TObject* Sender, const System::UnicodeString Path, System::UnicodeString &Content, bool &Handled);

typedef void __fastcall (__closure *TAiFileWriteEvent)(System::TObject* Sender, const System::UnicodeString Path, const System::UnicodeString Content, bool &Handled);

typedef void __fastcall (__closure *TAiFileCheckEvent)(System::TObject* Sender, const System::UnicodeString Path, bool &Exists, bool &Handled);

typedef void __fastcall (__closure *TAiDirEvent)(System::TObject* Sender, const System::UnicodeString Path, bool &Handled);

typedef void __fastcall (__closure *TAiCommandEvent)(System::TObject* Sender, const System::UnicodeString Command, const System::UnicodeString Path, System::Json::TJSONObject* Args, System::UnicodeString &Result, bool &Handled);

class PASCALIMPLEMENTATION TAiTextEditorTool : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	TAiFileReadEvent FOnLoadFile;
	TAiFileWriteEvent FOnSaveFile;
	TAiFileCheckEvent FOnFileExists;
	TAiDirEvent FOnEnsureDirectory;
	TAiCommandEvent FOnBeforeCommand;
	int __fastcall CountOccurrences(const System::UnicodeString Text, const System::UnicodeString SubText);
	
protected:
	virtual System::UnicodeString __fastcall LoadFileContent(const System::UnicodeString Path);
	virtual void __fastcall SaveFileContent(const System::UnicodeString Path, const System::UnicodeString Content);
	virtual bool __fastcall FileExists(const System::UnicodeString Path);
	virtual bool __fastcall EnsureDirectory(const System::UnicodeString Path);
	virtual bool __fastcall ValidatePath(const System::UnicodeString APath);
	virtual System::UnicodeString __fastcall Cmd_View(const System::UnicodeString Path, System::Json::TJSONObject* const jArgs);
	virtual System::UnicodeString __fastcall Cmd_Create(const System::UnicodeString Path, System::Json::TJSONObject* const jArgs);
	virtual System::UnicodeString __fastcall Cmd_StrReplace(const System::UnicodeString Path, System::Json::TJSONObject* const jArgs);
	virtual System::UnicodeString __fastcall Cmd_Insert(const System::UnicodeString Path, System::Json::TJSONObject* const jArgs);
	virtual System::UnicodeString __fastcall Cmd_ApplyDiff(const System::UnicodeString Path, System::Json::TJSONObject* const jArgs);
	
public:
	virtual System::UnicodeString __fastcall Execute(const System::UnicodeString JsonArguments);
	
__published:
	__property TAiFileReadEvent OnLoadFile = {read=FOnLoadFile, write=FOnLoadFile};
	__property TAiFileWriteEvent OnSaveFile = {read=FOnSaveFile, write=FOnSaveFile};
	__property TAiFileCheckEvent OnFileExists = {read=FOnFileExists, write=FOnFileExists};
	__property TAiDirEvent OnEnsureDirectory = {read=FOnEnsureDirectory, write=FOnEnsureDirectory};
	__property TAiCommandEvent OnBeforeCommand = {read=FOnBeforeCommand, write=FOnBeforeCommand};
public:
	/* TComponent.Create */ inline __fastcall virtual TAiTextEditorTool(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TAiTextEditorTool() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall Register(void);
}	/* namespace Texteditor */
}	/* namespace Tools */
}	/* namespace Umakerai */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_UMAKERAI_TOOLS_TEXTEDITOR)
using namespace Umakerai::Tools::Texteditor;
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
#endif	// uMakerAi_Tools_TextEditorHPP
