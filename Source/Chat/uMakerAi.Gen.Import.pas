{ ===============================================================================
  uMakerAi.Gen.Import

  Delphi wrapper for makerai.gen.dll.
  Provides streaming LLM text generation backed by llama.cpp GGUF models.

  Usage:
    Gen := TLlamaGenerator.Create;
    if Gen.ValidateModelFile('C:\models\llama3.2.gguf') = llamaErrNone then
      Gen.Load('C:\models\llama3.2.gguf');

    // Standard generation
    Output := Gen.Generate('Write a haiku about the sea:');

    // Extended: Mirostat v2
    Gen.GenerateStream('Tell me a joke:', 256, 0.8, 0.95,
      smMirostatV2, 5.0, 0.1,
      procedure(const Piece: string; IsLast: Boolean)
      begin
        if not IsLast then Write(Piece);
      end);
    Gen.Free;

  License: MIT
=============================================================================== }
unit uMakerAi.Gen.Import;

interface

uses
  System.SysUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ELSE}
  Posix.Dlfcn,
  Posix.Errno,
{$ENDIF}
  uMakerAi.ErrorCodes;

{$IFDEF MSWINDOWS}
// En Windows el DLL acepta wchar_t* (Delphi PChar = PWideChar) — sin conversión.
type TNativeStr = PChar;
{$DEFINE GEN_NATIVE_WCHAR}
{$ELSE}
// En POSIX (Linux, macOS, Android) los .so esperan const char* (UTF-8).
// Usamos MarshaledAString y convertimos en los call sites.
type TNativeStr = MarshaledAString;
{$ENDIF}

{$IFNDEF MSWINDOWS}
// ---------------------------------------------------------------------------
// POSIX shim — expose the Win32 module-loading API surface on Linux / macOS
// ---------------------------------------------------------------------------
type
  HMODULE = NativeUInt;  // matches dlopen/dlsym/dlclose signatures in Posix.Dlfcn

function LoadLibrary(LibName: PChar): HMODULE; inline;
begin
  Result := dlopen(MarshaledAString(UTF8String(LibName)), RTLD_NOW or RTLD_GLOBAL);
end;

function GetProcAddress(Lib: HMODULE; Name: PChar): Pointer; inline;
begin
  Result := dlsym(Lib, MarshaledAString(UTF8String(Name)));
end;

procedure FreeLibrary(Lib: HMODULE); inline;
begin
  if Lib <> 0 then dlclose(Lib);
end;

function GetLastError: Integer; inline;
begin
  Result := errno;
end;
// ---------------------------------------------------------------------------
{$ENDIF}

type
  EGenError = class(Exception)
  public
    Code: TLlamaErrorCode;
    constructor Create(ACode: TLlamaErrorCode; const AMsg: string);
  end;

  { Sampler mode for GenerateEx / GenerateStream overload }
  TSamplerMode = (
    smGreedy     = 0,  // Greedy only (Temp ignored)
    smTempTopP   = 1,  // Temperature + Top-P + Greedy (default)
    smMinP       = 2,  // Min-P:        Param1=min_p,    Param2=min_keep
    smMirostat   = 3,  // Mirostat v1:  Param1=tau,      Param2=eta
    smMirostatV2 = 4,  // Mirostat v2:  Param1=tau,      Param2=eta
    smTypical    = 5   // Typical-P:    Param1=typical_p, Param2=min_keep
  );

  { Return False from OnToken to abort generation early (e.g. stop token detected). }
  TTokenPieceProc = reference to procedure(const Piece: string; IsLast: Boolean;
                                           var Stop: Boolean);

  TLlamaGenerator = class
  private type
    TRawCallback = function(Piece: PAnsiChar; IsLast: LongBool;
                            UserData: Pointer): LongBool; cdecl;
  private
    FLib:      HMODULE;
    FInst:     Pointer;
    FDLLPath:  string;

    FCreate:            function: Pointer; stdcall;
    FLoad:              function(Inst: Pointer; ModelPath: TNativeStr;
                          NGPULayers, NCtx: Integer): LongBool; stdcall;
    FGenerate:          function(Inst: Pointer; Prompt: TNativeStr; MaxTokens: Integer;
                          Temp, TopP: Single;
                          Callback: TRawCallback; UserData: Pointer): LongBool; stdcall;
    FGenerateEx:        function(Inst: Pointer; Prompt: TNativeStr; MaxTokens: Integer;
                          Temp, TopP: Single; SamplerMode: Integer;
                          Param1, Param2: Single;
                          Callback: TRawCallback; UserData: Pointer): LongBool; stdcall;
    FReset:             procedure(Inst: Pointer); stdcall;
    FFree:              procedure(Inst: Pointer); stdcall;
    FIsGPUEnabled:      function(Inst: Pointer): LongBool; stdcall;
    FGetLastError:      function: PAnsiChar; stdcall;  // siempre narrow (UTF-8)
    FGetLastErrorCode:  function: Integer; stdcall;
    FValidate:          function(Path: TNativeStr): Integer; stdcall;

    procedure LoadDLL(const DLLPath: string);
    function  Bind(const Name: string): Pointer;
    function  LastErrorCode: TLlamaErrorCode;
    procedure RaiseLastError;
    procedure CheckOK(OK: Boolean);
  public
    constructor Create(const DLLPath: string =
      {$IFDEF MSWINDOWS}'makerai.gen.dll'
      {$ELSEIF DEFINED(ANDROID) or DEFINED(ANDROID64)}'libmakerai_gen.so'
      {$ELSE}'makerai.gen.so'{$ENDIF});
    destructor  Destroy; override;

    { Validates the model file before loading. Returns llamaErrNone on success. }
    function ValidateModelFile(const Path: string): TLlamaErrorCode;

    { Load a GGUF model.
      NGPULayers: 99 = all on GPU, 0 = CPU only.
      NCtx: context size in tokens. 0 = default (4096). }
    procedure Load(const GGUFPath: string; NGPULayers: Integer = 99;
                   NCtx: Integer = 0);

    { Generate text and return the full response (Temperature + Top-P). }
    function Generate(const Prompt: string; MaxTokens: Integer = 512;
                      Temp: Single = 0.8; TopP: Single = 0.95): string;

    { Generate with explicit sampler mode and parameters. }
    function GenerateEx(const Prompt: string; MaxTokens: Integer;
                        Temp, TopP: Single; Mode: TSamplerMode;
                        Param1: Single = 0; Param2: Single = 0): string;

    { Stream with default sampler (Temperature + Top-P). }
    procedure GenerateStream(const Prompt: string; MaxTokens: Integer;
                             Temp, TopP: Single;
                             OnToken: TTokenPieceProc); overload;

    { Stream with explicit sampler mode. }
    procedure GenerateStream(const Prompt: string; MaxTokens: Integer;
                             Temp, TopP: Single; Mode: TSamplerMode;
                             Param1, Param2: Single;
                             OnToken: TTokenPieceProc); overload;

    { Clear KV cache before a fresh generation. }
    procedure Reset;

    function GetIsGPUEnabled: Boolean;
    property IsGPUEnabled: Boolean read GetIsGPUEnabled;
    property DLLPath: string read FDLLPath;
  end;

implementation

// =============================================================================
// Callback context
// =============================================================================
type
  PCallbackCtx = ^TCallbackCtx;
  TCallbackCtx = record
    Builder: TStringBuilder;
    OnToken: TTokenPieceProc;
  end;

function RawTokenCallback(Piece: PAnsiChar; IsLast: LongBool;
  UserData: Pointer): LongBool; cdecl;
var
  Ctx:  PCallbackCtx;
  S:    string;
  Stop: Boolean;
begin
  Result := True;  // continue by default
  Ctx := PCallbackCtx(UserData);
  if Ctx = nil then Exit;

  Stop := False;
  if IsLast then
  begin
    if Assigned(Ctx^.OnToken) then
      Ctx^.OnToken('', True, Stop);
  end
  else if (Piece <> nil) and (Piece^ <> #0) then
  begin
    S := UTF8ToString(PAnsiChar(Piece));
    if Assigned(Ctx^.Builder) then
      Ctx^.Builder.Append(S);
    if Assigned(Ctx^.OnToken) then
      Ctx^.OnToken(S, False, Stop);
  end;
  Result := not Stop;
end;

// =============================================================================
// EGenError
// =============================================================================
constructor EGenError.Create(ACode: TLlamaErrorCode; const AMsg: string);
begin
  inherited Create(AMsg);
  Code := ACode;
end;

// =============================================================================
// TLlamaGenerator
// =============================================================================

constructor TLlamaGenerator.Create(const DLLPath: string);
begin
  inherited Create;
  LoadDLL(DLLPath);
  FInst := FCreate();
  if FInst = nil then
    RaiseLastError;
end;

destructor TLlamaGenerator.Destroy;
begin
  if (FInst <> nil) and Assigned(FFree) then
    FFree(FInst);
  if FLib <> 0 then
    FreeLibrary(FLib);
  inherited;
end;

function TLlamaGenerator.Bind(const Name: string): Pointer;
begin
  Result := GetProcAddress(FLib, PChar(Name));
  if Result = nil then
    raise EGenError.Create(llamaErrEntryPoint,
      Format('%s: entry point "%s" not found', [FDLLPath, Name]));
end;

procedure TLlamaGenerator.LoadDLL(const DLLPath: string);
{$IFDEF MSWINDOWS}
var
  LDir: string;
{$ENDIF}
begin
  FDLLPath := DLLPath;
{$IFDEF MSWINDOWS}
  // Agrega el directorio de makerai.gen.dll al search path para que Windows
  // encuentre las dependencias (ggml-cuda.dll, cublas*.dll, etc.) aunque el
  // .exe este en otro directorio.
  LDir := ExtractFilePath(ExpandFileName(DLLPath));
  if LDir <> '' then
    SetDllDirectory(PChar(LDir));
{$ENDIF}
  FLib := LoadLibrary(PChar(DLLPath));
  if FLib = 0 then
    raise EGenError.Create(llamaErrDllLoad,
      Format('Cannot load %s (error %d)', [DLLPath, GetLastError]));

  @FCreate           := Bind('LlamaGenCreate');
  @FLoad             := Bind('LlamaGenLoad');
  @FGenerate         := Bind('LlamaGenerate');
  @FGenerateEx       := Bind('LlamaGenerateEx');
  @FReset            := Bind('LlamaGenReset');
  @FFree             := Bind('LlamaGenFree');
  @FIsGPUEnabled     := Bind('LlamaGenIsGPUEnabled');
  @FGetLastError     := Bind('LlamaGenGetLastError');
  @FGetLastErrorCode := Bind('LlamaGenGetLastErrorCode');
  @FValidate         := Bind('LlamaGenValidateModelFile');
end;

function TLlamaGenerator.LastErrorCode: TLlamaErrorCode;
begin
  if Assigned(FGetLastErrorCode) then
    Result := TLlamaErrorCode(FGetLastErrorCode())
  else
    Result := llamaErrUnknown;
end;

procedure TLlamaGenerator.RaiseLastError;
var
  Msg: string;
begin
  if Assigned(FGetLastError) then Msg := UTF8ToString(FGetLastError())
  else Msg := '(unknown error)';
  raise EGenError.Create(LastErrorCode, Msg);
end;

procedure TLlamaGenerator.CheckOK(OK: Boolean);
begin
  if not OK then RaiseLastError;
end;

function TLlamaGenerator.ValidateModelFile(const Path: string): TLlamaErrorCode;
begin
{$IFDEF GEN_NATIVE_WCHAR}
  Result := TLlamaErrorCode(FValidate(PChar(Path)));
{$ELSE}
  Result := TLlamaErrorCode(FValidate(MarshaledAString(UTF8String(Path))));
{$ENDIF}
end;

procedure TLlamaGenerator.Load(const GGUFPath: string; NGPULayers: Integer;
  NCtx: Integer);
begin
{$IFDEF GEN_NATIVE_WCHAR}
  CheckOK(FLoad(FInst, PChar(GGUFPath), NGPULayers, NCtx));
{$ELSE}
  CheckOK(FLoad(FInst, MarshaledAString(UTF8String(GGUFPath)), NGPULayers, NCtx));
{$ENDIF}
end;

procedure TLlamaGenerator.Reset;
begin
  FReset(FInst);
end;

function TLlamaGenerator.GetIsGPUEnabled: Boolean;
begin
  Result := FIsGPUEnabled(FInst);
end;

function TLlamaGenerator.Generate(const Prompt: string; MaxTokens: Integer;
  Temp, TopP: Single): string;
var
  Ctx: TCallbackCtx;
begin
  FillChar(Ctx, SizeOf(Ctx), 0);
  Ctx.Builder := TStringBuilder.Create;
  try
{$IFDEF GEN_NATIVE_WCHAR}
    CheckOK(FGenerate(FInst, PChar(Prompt), MaxTokens, Temp, TopP,
      @RawTokenCallback, @Ctx));
{$ELSE}
    CheckOK(FGenerate(FInst, MarshaledAString(UTF8String(Prompt)), MaxTokens, Temp, TopP,
      @RawTokenCallback, @Ctx));
{$ENDIF}
    Result := Ctx.Builder.ToString;
  finally
    Ctx.Builder.Free;
  end;
end;

function TLlamaGenerator.GenerateEx(const Prompt: string; MaxTokens: Integer;
  Temp, TopP: Single; Mode: TSamplerMode; Param1, Param2: Single): string;
var
  Ctx: TCallbackCtx;
begin
  FillChar(Ctx, SizeOf(Ctx), 0);
  Ctx.Builder := TStringBuilder.Create;
  try
{$IFDEF GEN_NATIVE_WCHAR}
    CheckOK(FGenerateEx(FInst, PChar(Prompt), MaxTokens, Temp, TopP,
      Ord(Mode), Param1, Param2, @RawTokenCallback, @Ctx));
{$ELSE}
    CheckOK(FGenerateEx(FInst, MarshaledAString(UTF8String(Prompt)), MaxTokens, Temp, TopP,
      Ord(Mode), Param1, Param2, @RawTokenCallback, @Ctx));
{$ENDIF}
    Result := Ctx.Builder.ToString;
  finally
    Ctx.Builder.Free;
  end;
end;

procedure TLlamaGenerator.GenerateStream(const Prompt: string;
  MaxTokens: Integer; Temp, TopP: Single; OnToken: TTokenPieceProc);
var
  Ctx: TCallbackCtx;
begin
  FillChar(Ctx, SizeOf(Ctx), 0);
  Ctx.OnToken := OnToken;
{$IFDEF GEN_NATIVE_WCHAR}
  CheckOK(FGenerate(FInst, PChar(Prompt), MaxTokens, Temp, TopP,
    @RawTokenCallback, @Ctx));
{$ELSE}
  CheckOK(FGenerate(FInst, MarshaledAString(UTF8String(Prompt)), MaxTokens, Temp, TopP,
    @RawTokenCallback, @Ctx));
{$ENDIF}
end;

procedure TLlamaGenerator.GenerateStream(const Prompt: string;
  MaxTokens: Integer; Temp, TopP: Single; Mode: TSamplerMode;
  Param1, Param2: Single; OnToken: TTokenPieceProc);
var
  Ctx: TCallbackCtx;
begin
  FillChar(Ctx, SizeOf(Ctx), 0);
  Ctx.OnToken := OnToken;
{$IFDEF GEN_NATIVE_WCHAR}
  CheckOK(FGenerateEx(FInst, PChar(Prompt), MaxTokens, Temp, TopP,
    Ord(Mode), Param1, Param2, @RawTokenCallback, @Ctx));
{$ELSE}
  CheckOK(FGenerateEx(FInst, MarshaledAString(UTF8String(Prompt)), MaxTokens, Temp, TopP,
    Ord(Mode), Param1, Param2, @RawTokenCallback, @Ctx));
{$ENDIF}
end;

end.
